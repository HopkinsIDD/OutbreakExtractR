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
    dplyr::ungroup() %>%
    sf::st_make_valid()

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
  # Strip any sub-national suffix (e.g. "TZA::Mainland" -> "TZA") so that
  # gb_adm0() receives a plain ISO3 code it can resolve.
  iso3_for_boundary <- regmatches(country_iso3, regexpr("[A-Z]{3}", country_iso3))
  country_shp <- tryCatch(
    sf::st_transform(rgeoboundaries::gb_adm0(country = iso3_for_boundary), 4326),
    error = function(e) {
      message("rgeoboundaries::gb_adm0() failed: ", conditionMessage(e),
              "\nFalling back to union of LP geometries as country boundary.")
      all_geoms <- Filter(Negate(is.null), as.list(geom_lookup))
      all_geoms_sfc <- do.call(sf::st_sfc, all_geoms)
      # Set CRS separately — passing crs inside the do.call list triggers
      # c.sfc dispatch which tries to compute st_bbox on the crs object.
      sf::st_crs(all_geoms_sfc) <- if (!is.na(source_crs)) source_crs else 4326
      sf::st_sf(
        geometry = sf::st_union(sf::st_make_valid(sf::st_transform(all_geoms_sfc, 4326)))
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
        download_worldpop_constrained(iso3_for_boundary, yr, dest_dir = raster_dir),
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
      #    Wrapped in tryCatch: the fallback country boundary (LP-geometry union,
      #    used when rgeoboundaries is unavailable) can produce a geometry that
      #    exactextractr cannot resolve ("Error getting geometry extent") — in
      #    that case we fall through to adj_factor = 1.0.
      country_raw <- tryCatch(
        sum(
          exactextractr::exact_extract(
            pop_raster, sf::st_geometry(country_shp), "sum"
          ),
          na.rm = TRUE
        ),
        error = function(e) {
          message("    adj factor extraction failed: ", conditionMessage(e),
                  " — using 1.0 (population will be unadjusted).")
          0
        }
      )
      tot_UN <- WPP2024$PopTotal[
        WPP2024$Time == yr & WPP2024$ISO3_code == iso3_for_boundary
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

        # Normalize to a uniform geometry type.
        # exactextractr::exact_extract() errors with "Mixed-type geometries not
        # supported" when valid_sfc contains a mix of POLYGON and MULTIPOLYGON.
        # After the bad_geom filter all remaining features have dimension == 2
        # (polygon-type), so casting to MULTIPOLYGON is always safe.
        if (length(valid_sfc) > 0L) {
          geom_types <- unique(as.character(sf::st_geometry_type(valid_sfc)))
          if (length(geom_types) > 1L || identical(geom_types, "POLYGON")) {
            valid_sfc <- sf::st_cast(valid_sfc, "MULTIPOLYGON", warn = FALSE)
          }
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

#' @export
#' @title estimate_pop_for_geometries
#' @name estimate_pop_for_geometries
#' @description Estimate a UN-adjusted WorldPop population for each polygon in an
#'   sf object by extracting the constrained WorldPop raster directly on the
#'   geometry. Intended for composite locations, whose denominator should be the
#'   population of the actual (child-union) sub-area rather than the sum of
#'   child populations or the parent-admin polygon. Mirrors the WorldPop machinery
#'   in \code{add_population()} (one raster load per year, one adjustment-factor
#'   \code{exact_extract} on the country boundary, one vectorized
#'   \code{exact_extract} for all geometries in that year) and reuses the same
#'   geometry sanitation (\code{st_make_valid}, drop empty/non-polygon, cast to
#'   MULTIPOLYGON) so mixed-type / POINT / empty geometries do not crash the
#'   extraction.
#' @param geom_sf sf: polygons to estimate population for. One value is returned
#'   per row, in input order.
#' @param country_iso3 character: ISO3 country code (e.g. "BDI"); a sub-national
#'   suffix is tolerated (the leading 3-letter code is extracted).
#' @param year integer: representative year(s), length 1 (recycled) or
#'   \code{nrow(geom_sf)}. Clamped to the WorldPop constrained range 2015-2030.
#' @param raster_dir character: directory for caching WorldPop rasters.
#' @return numeric vector of length \code{nrow(geom_sf)} with the UN-adjusted
#'   population per geometry (NA where the raster is unavailable or the geometry
#'   is unusable).
estimate_pop_for_geometries <- function(geom_sf, country_iso3, year,
                                         raster_dir = "worldpop") {

  n <- nrow(geom_sf)
  if (n == 0L) return(numeric(0L))

  country_iso3     <- toupper(country_iso3)
  iso3_for_boundary <- regmatches(country_iso3, regexpr("[A-Z]{3}", country_iso3))

  # Representative year per geometry, clamped to the WorldPop range.
  if (length(year) == 1L) year <- rep(year, n)
  if (length(year) != n) {
    stop("estimate_pop_for_geometries(): 'year' must have length 1 or nrow(geom_sf).")
  }
  year <- pmax(2015L, pmin(2030L, as.integer(year)))

  # Work in EPSG:4326 (WorldPop CRS); keep an explicit row index for reassembly.
  geoms_sfc <- sf::st_geometry(sf::st_make_valid(geom_sf))
  if (is.na(sf::st_crs(geoms_sfc))) sf::st_crs(geoms_sfc) <- 4326
  geoms_sfc <- sf::st_transform(geoms_sfc, 4326)

  # Country boundary for the UN adjustment factor (fetched once).
  country_shp <- tryCatch(
    sf::st_transform(rgeoboundaries::gb_adm0(country = iso3_for_boundary), 4326),
    error = function(e) {
      message("estimate_pop_for_geometries(): gb_adm0() failed: ",
              conditionMessage(e), " — using union of input geometries as boundary.")
      sf::st_sf(geometry = sf::st_union(geoms_sfc))
    }
  )

  data("WPP2024", package = "OutbreakExtractR", envir = environment())

  pop_out <- rep(NA_real_, n)

  for (yr in sort(unique(year))) {
    idx <- which(year == yr)

    raster_path <- tryCatch(
      download_worldpop_constrained(iso3_for_boundary, yr, dest_dir = raster_dir),
      error = function(e) {
        message("  estimate_pop_for_geometries(): raster download failed for ",
                yr, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(raster_path)) next

    pop_raster <- raster::raster(raster_path)

    # Adjustment factor: one exact_extract on the country boundary.
    country_raw <- tryCatch(
      sum(exactextractr::exact_extract(pop_raster, sf::st_geometry(country_shp), "sum"),
          na.rm = TRUE),
      error = function(e) 0
    )
    tot_UN <- WPP2024$PopTotal[
      WPP2024$Time == yr & WPP2024$ISO3_code == iso3_for_boundary
    ] * 1e3
    adj_factor <- if (length(tot_UN) == 1L && country_raw > 0) tot_UN / country_raw else 1.0

    # Sanitize this year's geometries (same guards as add_population()).
    this_sfc <- geoms_sfc[idx]
    dims     <- sf::st_dimension(this_sfc)
    good     <- !is.na(dims) & dims == 2L
    if (!any(good)) {
      rm(pop_raster); gc(verbose = FALSE); next
    }
    keep_idx <- idx[good]
    this_sfc <- this_sfc[good]

    geom_types <- unique(as.character(sf::st_geometry_type(this_sfc)))
    if (length(geom_types) > 1L || identical(geom_types, "POLYGON")) {
      this_sfc <- sf::st_cast(this_sfc, "MULTIPOLYGON", warn = FALSE)
    }

    raw_pops <- tryCatch(
      exactextractr::exact_extract(pop_raster, this_sfc, "sum"),
      error = function(e) {
        message("  estimate_pop_for_geometries(): extraction failed for year ",
                yr, ": ", conditionMessage(e))
        rep(NA_real_, length(this_sfc))
      }
    )
    pop_out[keep_idx] <- as.numeric(raw_pops) * adj_factor

    rm(pop_raster); gc(verbose = FALSE)
  }

  pop_out
}
