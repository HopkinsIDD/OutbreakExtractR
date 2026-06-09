#' @export
#' @title add_population
#' @name add_population
#' @description Attach WorldPop population estimates to a normalized weekly
#'   data frame. Intended as a Stage 1 pipeline step immediately after
#'   fill_missing_lps(), and before writing to parquet.
#'
#'   A single population value is computed per unique location_period_id (not
#'   per week), which is correct: get_outbreak_threshold() and
#'   identify_epidemic_start() both use pop as a static denominator for
#'   incidence calculations.
#'
#'   Population is estimated by:
#'     1. Looking up each location_period_id geometry in raw_sf (already in
#'        memory — no extra DB call).
#'     2. Clamping the representative year (median TL) to 2015-2030 (WorldPop
#'        constrained raster range).
#'     3. For each unique year: loading the raster ONCE, computing the UN
#'        adjustment factor with one exact_extract call on the country boundary,
#'        then extracting ALL LP populations with a single vectorized
#'        exact_extract call. The raster is released immediately after.
#'
#'   This avoids the 2N-loads-per-year penalty that results from calling
#'   get_pop() per LP (each call loads the raster twice: once for the LP
#'   geometry and once inside estimate_adj_factors() for the country boundary).
#'
#' @param normalized_data data.frame: weekly normalized data from the Stage 1
#'   pipeline (output of fill_missing_lps()). Must contain columns
#'   location_period_id and TL.
#' @param raw_sf sf object: geometry-bearing data returned by
#'   taxdat::pull_taxonomy_data() (before geometry is dropped). Must contain
#'   a location_period_id, locationPeriod_id, or lctn_pr column and an sf
#'   geometry column.
#' @param country_iso3 character: ISO3 country code (e.g. "COD"). Used to
#'   download the correct WorldPop raster and to fetch the country boundary
#'   for the UN population adjustment factor.
#' @param raster_dir character: directory for caching WorldPop raster files.
#'   Defaults to "worldpop". Created if it does not exist.
#'
#' @return normalized_data with a numeric pop column added. Rows whose
#'   location_period_id has no matching geometry in raw_sf receive NA.
add_population <- function(normalized_data, raw_sf, country_iso3,
                           raster_dir = "worldpop") {

  country_iso3 <- toupper(country_iso3)

  # ---------------------------------------------------------------------------
  # 1. Build named geometry lookup: LP ID (character) -> sfg object
  # ---------------------------------------------------------------------------
  # taxdat::rename_database_fields(source="api") uses "locationPeriod_id" (camelCase);
  # taxdat::rename_database_fields(source="psql") / after clean_psql_data() uses
  # "location_period_id" (snake_case); get_shp() uses "lctn_pr". Accept all three.
  geom_id_col <- if ("location_period_id" %in% names(raw_sf)) {
    "location_period_id"
  } else if ("locationPeriod_id" %in% names(raw_sf)) {
    "locationPeriod_id"
  } else if ("lctn_pr" %in% names(raw_sf)) {
    "lctn_pr"
  } else {
    stop("raw_sf must have a 'location_period_id', 'locationPeriod_id', or 'lctn_pr' column.")
  }

  lp_geoms <- raw_sf %>%
    dplyr::rename(lp_id = !!geom_id_col) %>%
    dplyr::group_by(lp_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Named list: character(LP ID) -> sfg geometry
  geom_lookup <- setNames(
    as.list(sf::st_geometry(lp_geoms)),
    as.character(lp_geoms$lp_id)
  )
  source_crs <- sf::st_crs(raw_sf)

  # ---------------------------------------------------------------------------
  # 2. Representative year per LP (clamped to WorldPop range 2015-2030)
  # ---------------------------------------------------------------------------
  lp_years <- normalized_data %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::group_by(location_period_id) %>%
    dplyr::summarise(
      year = pmax(2015L, pmin(2030L,
                              as.integer(stats::median(lubridate::year(TL))))),
      .groups = "drop"
    )

  # All location_period_ids are NA (absent from API response or filtered out).
  # purrr::list_rbind() on an empty map returns a 0-column tibble, which breaks
  # the left_join below. Return early with pop = NA for all rows.
  if (nrow(lp_years) == 0) {
    message("add_population(): no valid location_period_ids — pop set to NA for all rows.")
    normalized_data$pop <- NA_real_
    return(normalized_data)
  }

  # ---------------------------------------------------------------------------
  # 3. Country boundary for the UN adjustment factor (fetched once)
  # ---------------------------------------------------------------------------
  # Primary: rgeoboundaries network call.
  # Fallback: union of all LP geometries (approximation, avoids network dep).
  country_shp <- tryCatch(
    sf::st_transform(rgeoboundaries::gb_adm0(country = country_iso3), 4326),
    error = function(e) {
      message("rgeoboundaries::gb_adm0() failed: ", conditionMessage(e),
              "\nFalling back to union of LP geometries as country boundary.")
      all_geoms <- Filter(Negate(is.null), as.list(geom_lookup))
      all_geoms_sfc <- do.call(sf::st_sfc, all_geoms)
      # Set CRS separately — passing crs inside the do.call list triggers
      # c.sfc dispatch which tries to compute st_bbox on the crs object.
      sf::st_crs(all_geoms_sfc) <- if (!is.na(source_crs)) source_crs else 4326
      sf::st_sf(
        geometry = sf::st_union(sf::st_transform(all_geoms_sfc, 4326))
      )
    }
  )

  # ---------------------------------------------------------------------------
  # 4. WPP2024 national totals (loaded once, used for adj factor in every year)
  # ---------------------------------------------------------------------------
  data("WPP2024", package = "OutbreakExtractR", envir = environment())

  # ---------------------------------------------------------------------------
  # 5. One raster load per year; two exact_extract calls per year
  #
  #    Avoids the 2N raster-load penalty of calling get_pop() per LP, where
  #    each call loads the raster in get_pop() AND again inside
  #    estimate_adj_factors(). Here we load once and inline the adj-factor
  #    calculation against the same in-memory raster object.
  # ---------------------------------------------------------------------------
  message("add_population(): ", nrow(lp_years), " LP(s) across ",
          dplyr::n_distinct(lp_years$year), " year(s) in ", country_iso3)

  lp_pop <- lp_years %>%
    dplyr::group_by(year) %>%
    dplyr::group_split() %>%
    purrr::map(\(year_group) {

      yr    <- year_group$year[1]
      lp_ids <- year_group$location_period_id
      message("  Year ", yr, ": ", length(lp_ids), " LP(s)")

      # -- a. Download / cache raster (no-op if already on disk) --------------
      raster_path <- tryCatch(
        download_worldpop_constrained(country_iso3, yr, dest_dir = raster_dir),
        error = function(e) {
          message("    Raster download failed: ", conditionMessage(e))
          NULL
        }
      )

      if (is.null(raster_path)) {
        message("    Skipping year ", yr, " — all LPs set to NA.")
        return(dplyr::tibble(location_period_id = lp_ids, pop = NA_real_))
      }

      # Load raster ONCE for this year
      pop_raster <- raster::raster(raster_path)

      # -- b. Adj factor: one exact_extract call on the country boundary ------
      #    Inlined from estimate_adj_factors() to reuse the already-loaded
      #    raster instead of having that function load it a second time.
      country_raw <- sum(
        exactextractr::exact_extract(
          pop_raster, sf::st_geometry(country_shp), "sum"
        ),
        na.rm = TRUE
      )
      tot_UN <- WPP2024$PopTotal[
        WPP2024$Time == yr & WPP2024$ISO3_code == country_iso3
      ] * 1e3

      adj_factor <- if (length(tot_UN) == 1L && country_raw > 0) {
        tot_UN / country_raw
      } else {
        message("    Could not compute adj factor for year ", yr,
                " — using 1.0 (population will be unadjusted).")
        1.0
      }

      # -- c. Identify which LPs have geometry in this batch ------------------
      geoms   <- lapply(as.character(lp_ids), \(id) geom_lookup[[id]])
      missing <- vapply(geoms, is.null, logical(1L))

      if (any(missing)) {
        message("    No geometry for LP(s): ",
                paste(lp_ids[missing], collapse = ", "), " — pop = NA.")
      }

      pop_values <- rep(NA_real_, length(lp_ids))
      valid_idx  <- which(!missing)

      if (length(valid_idx) > 0L) {
        # -- d. Single vectorized exact_extract call for all LPs this year ----
        #    exactextractr processes multiple geometries in one C++ pass,
        #    reading each raster tile at most once.
        valid_sfc <- do.call(sf::st_sfc, geoms[valid_idx])
        sf::st_crs(valid_sfc) <- if (!is.na(source_crs)) source_crs else 4326
        valid_sfc <- sf::st_transform(valid_sfc, 4326)

        # exact_extract requires 2-D polygon geometries.
        # st_dimension() returns: NA for empty, 0 for point, 1 for line, 2 for polygon.
        # A single pass covers both the "GEOMETRYCOLLECTION EMPTY" case (NA) and
        # the centroid-only "POINT" case (0) that would otherwise cause
        # exactextractr's internal if(!all(st_dimension(y)==2)) to throw
        # "missing value where TRUE/FALSE needed".
        dims     <- sf::st_dimension(valid_sfc)
        bad_geom <- is.na(dims) | dims != 2L
        if (any(bad_geom)) {
          message("    Unusable geometry (empty/non-polygon) for LP(s): ",
                  paste(lp_ids[valid_idx[bad_geom]], collapse = ", "),
                  " — pop = NA.")
          valid_idx <- valid_idx[!bad_geom]
          valid_sfc <- valid_sfc[!bad_geom]
        }

        if (length(valid_idx) > 0L) {
          raw_pops <- exactextractr::exact_extract(
            pop_raster, valid_sfc, "sum"
          )
          pop_values[valid_idx] <- as.numeric(raw_pops) * adj_factor
        }
      }

      # -- e. Release raster from memory before moving to the next year -------
      rm(pop_raster)
      gc(verbose = FALSE)

      dplyr::tibble(location_period_id = lp_ids, pop = pop_values, adj_factor)
    }) %>%
    purrr::list_rbind()

  # ---------------------------------------------------------------------------
  # 6. Join pop back onto the normalized data
  # ---------------------------------------------------------------------------
  if ("pop" %in% names(normalized_data)) {
    normalized_data <- dplyr::select(normalized_data, -pop)
  }

  normalized_data <- dplyr::left_join(normalized_data, lp_pop,
                                      by = "location_period_id")

  n_with <- sum(!is.na(normalized_data$pop))
  message(sprintf("add_population(): %d / %d rows have a population estimate.",
                  n_with, nrow(normalized_data)))

  normalized_data
}
