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
#' @param boundary_cache_dir character or NULL: directory for caching national
#'   boundaries used by the UN adjustment. Passed to
#'   \code{get_country_boundary()}.
#'
#' @return normalized_data with a numeric \code{pop} column added, plus
#'   provenance columns: \code{pop_source}, \code{pop_geom_dup_n},
#'   \code{pop_geom_dup_class}, \code{pop_year_obs}, \code{pop_year_raster},
#'   \code{pop_natl_ref}, \code{adj_factor} and \code{adj_factor_flag}.
#'   Rows whose location_period_id has no matching geometry in raw_sf receive
#'   \code{pop = NA} (never 0 — see the note on zero denominators below).
#'
#'   \code{pop} is \code{NA}, never 0, whenever a population cannot be
#'   established. \code{get_outbreak_threshold()} routes \code{is.na(pop)} to
#'   the "low" surveillance class, but \code{pop == 0} yields
#'   \code{sCh / pop == Inf}, which classifies as "high" — so a zero denominator
#'   silently flips the outbreak-detection threshold rather than merely
#'   producing a bad rate.
add_population <- function(normalized_data, raw_sf, country_iso3,
                           raster_dir = "worldpop",
                           boundary_cache_dir = "country_boundaries") {

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
  # pop_year_obs keeps the *unclamped* median observation year; pop_year_raster
  # is the year actually rastered. Downstream can rescale a static population to
  # an arbitrary year with pop * WPP(iso3, t) / pop_natl_ref, which needs both.
  lp_years <- normalized_data %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::group_by(location_period_id) %>%
    dplyr::summarise(
      pop_year_obs = as.integer(stats::median(lubridate::year(TL))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(year = pmax(2015L, pmin(2030L, pop_year_obs)))

  # All location_period_ids are NA (absent from API response or filtered out).
  # purrr::list_rbind() on an empty map returns a 0-column tibble, which breaks
  # the left_join below. Return early with pop = NA for all rows.
  if (nrow(lp_years) == 0) {
    message("add_population(): no valid location_period_ids — pop set to NA for all rows.")
    return(attach_empty_pop_provenance(normalized_data))
  }

  # ---------------------------------------------------------------------------
  # 3. Country boundary for the UN adjustment factor (fetched once)
  # ---------------------------------------------------------------------------
  # Resolved by get_country_boundary(): disk cache -> gb_adm0() -> NULL.
  #
  # There is deliberately NO union-of-LP-geometries fallback. That union covers
  # only the surveilled sub-areas, so extracting the raster on it understates
  # country_raw and inflates adj_factor = tot_UN / country_raw, scaling every
  # population in the country upward. When the boundary cannot be resolved we
  # skip the adjustment (adj_factor = 1.0), leaving populations unadjusted
  # rather than wrong.
  iso3_for_boundary <- regmatches(country_iso3, regexpr("[A-Z]{3}", country_iso3))
  country_shp <- get_country_boundary(iso3_for_boundary,
                                      cache_dir = boundary_cache_dir)
  if (is.null(country_shp)) {
    message("add_population(): no national boundary for ", iso3_for_boundary,
            " — UN adjustment skipped (adj_factor = 1.0).")
  }

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

      tot_UN <- WPP2024$PopTotal[
        WPP2024$Time == yr & WPP2024$ISO3_code == iso3_for_boundary
      ] * 1e3
      pop_natl_ref <- if (length(tot_UN) == 1L) tot_UN else NA_real_

      if (is.null(raster_path)) {
        message("    Skipping year ", yr, " — all LPs set to NA.")
        return(dplyr::tibble(location_period_id = lp_ids, pop = NA_real_,
                             pop_source = "none", adj_factor = NA_real_,
                             adj_factor_flag = NA_character_,
                             pop_natl_ref = pop_natl_ref))
      }

      # Load raster ONCE for this year
      pop_raster <- raster::raster(raster_path)

      # -- b. Adj factor: one exact_extract call on the country boundary ------
      #    Inlined from estimate_adj_factors() to reuse the already-loaded
      #    raster instead of having that function load it a second time.
      country_raw <- if (is.null(country_shp)) {
        0
      } else {
        tryCatch(
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
      }

      adj_banded <- if (length(tot_UN) == 1L && country_raw > 0) {
        band_adj_factor(tot_UN / country_raw)
      } else {
        message("    Could not compute adj factor for year ", yr,
                " — using 1.0 (population will be unadjusted).")
        list(value = 1.0, flag = "unadjusted")
      }
      adj_factor      <- adj_banded$value
      adj_factor_flag <- adj_banded$flag

      # -- c. Identify which LPs have geometry in this batch ------------------
      geoms   <- lapply(as.character(lp_ids), \(id) geom_lookup[[id]])
      missing <- vapply(geoms, is.null, logical(1L))

      if (any(missing)) {
        message("    No geometry for LP(s): ",
                paste(lp_ids[missing], collapse = ", "), " — pop = NA.")
      }

      pop_values <- rep(NA_real_, length(lp_ids))
      pop_source <- rep("none", length(lp_ids))
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
          extracted <- as.numeric(raw_pops) * adj_factor

          # A raster sum of exactly 0 is not a population — it means the
          # constrained raster has no built-up cells under this footprint. Emit
          # NA so that get_outbreak_threshold() routes the LP to the "low"
          # surveillance class instead of computing sCh/0 == Inf and
          # classifying it as "high".
          zero_pop <- !is.na(extracted) & extracted <= 0
          if (any(zero_pop)) {
            message("    WorldPop returned 0 for LP(s): ",
                    paste(lp_ids[valid_idx[zero_pop]], collapse = ", "),
                    " — pop = NA (a zero denominator would flip the ",
                    "detection threshold).")
            extracted[zero_pop] <- NA_real_
          }

          pop_values[valid_idx] <- extracted
          pop_source[valid_idx] <- ifelse(is.na(extracted), "none",
                                          "worldpop_constrained")
        }
      }

      # -- e. Release raster from memory before moving to the next year -------
      rm(pop_raster)
      gc(verbose = FALSE)

      dplyr::tibble(location_period_id = lp_ids, pop = pop_values,
                    pop_source = pop_source, adj_factor = adj_factor,
                    adj_factor_flag = adj_factor_flag,
                    pop_natl_ref = pop_natl_ref)
    }) %>%
    purrr::list_rbind()

  # ---------------------------------------------------------------------------
  # 6. Duplicate-geometry detection
  #
  #    Several LPs can carry byte-identical geometry, in which case they are all
  #    assigned the same population. This originates in the Taxonomy source
  #    (distinct location_period_ids, distinct shape ids, identical shape
  #    content), not in the join here, so the response is to mark rather than
  #    to recompute. See detect_duplicate_geometries() for the class meanings.
  # ---------------------------------------------------------------------------
  lp_locations <- NULL
  if ("location" %in% names(normalized_data)) {
    loc_map <- normalized_data %>%
      dplyr::filter(!is.na(location_period_id)) %>%
      dplyr::group_by(location_period_id) %>%
      dplyr::summarise(location = dplyr::first(location), .groups = "drop")
    lp_locations <- loc_map$location[match(lp_geoms$lp_id,
                                           loc_map$location_period_id)]
  }

  dup_info <- detect_duplicate_geometries(
    lp_ids       = lp_geoms$lp_id,
    geoms        = sf::st_geometry(lp_geoms),
    lp_locations = lp_locations
  )

  n_dup <- sum(dup_info$pop_geom_dup_n > 1L)
  if (n_dup > 0L) {
    cls <- table(dup_info$pop_geom_dup_class[dup_info$pop_geom_dup_n > 1L])
    message("add_population(): ", n_dup, " LP(s) share geometry with another LP (",
            paste(names(cls), cls, sep = "=", collapse = ", "), ").")
  }

  # ---------------------------------------------------------------------------
  # 7. Join pop and provenance back onto the normalized data
  # ---------------------------------------------------------------------------
  prov_cols <- c("pop", "pop_source", "pop_geom_dup_n", "pop_geom_dup_class",
                 "pop_year_obs", "pop_year_raster", "pop_natl_ref",
                 "adj_factor", "adj_factor_flag")
  normalized_data <- dplyr::select(normalized_data, -dplyr::any_of(prov_cols))

  lp_pop <- lp_pop %>%
    dplyr::left_join(
      lp_years %>%
        dplyr::select(location_period_id, pop_year_obs,
                      pop_year_raster = year),
      by = "location_period_id"
    ) %>%
    dplyr::left_join(
      dup_info %>%
        dplyr::mutate(location_period_id = as(location_period_id,
                                              class(lp_pop$location_period_id))),
      by = "location_period_id"
    ) %>%
    dplyr::mutate(
      pop_geom_dup_n     = dplyr::coalesce(pop_geom_dup_n, 1L),
      pop_geom_dup_class = dplyr::coalesce(pop_geom_dup_class, "unique")
    )

  normalized_data <- dplyr::left_join(normalized_data, lp_pop,
                                      by = "location_period_id")

  n_with <- sum(!is.na(normalized_data$pop))
  message(sprintf("add_population(): %d / %d rows have a population estimate.",
                  n_with, nrow(normalized_data)))

  normalized_data
}

#' @title attach_empty_pop_provenance
#' @name attach_empty_pop_provenance
#' @description Attach the population provenance schema with empty values, so
#'   that early-return paths in \code{add_population()} produce the same columns
#'   as the full path. Consumers can then rely on the schema unconditionally.
#' @param d data.frame to attach columns to.
#' @return \code{d} with the provenance columns added.
#' @keywords internal
attach_empty_pop_provenance <- function(d) {
  d$pop                <- NA_real_
  d$pop_source         <- "none"
  d$pop_geom_dup_n     <- 1L
  d$pop_geom_dup_class <- "unique"
  d$pop_year_obs       <- NA_integer_
  d$pop_year_raster    <- NA_integer_
  d$pop_natl_ref       <- NA_real_
  d$adj_factor         <- NA_real_
  d$adj_factor_flag    <- NA_character_
  d
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
#' @param boundary_cache_dir character or NULL: directory for caching national
#'   boundaries, passed to \code{get_country_boundary()}.
#' @return numeric vector of length \code{nrow(geom_sf)} with the UN-adjusted
#'   population per geometry (NA where the raster is unavailable, the geometry
#'   is unusable, or the raster sum is zero — zero is never returned as a
#'   population).
estimate_pop_for_geometries <- function(geom_sf, country_iso3, year,
                                         raster_dir = "worldpop",
                                         boundary_cache_dir = "country_boundaries") {

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
  # As in add_population(): no union-of-input-geometries fallback — that
  # understates the national raster total and inflates the adjustment factor.
  # NULL means "skip the adjustment", not "approximate it".
  country_shp <- get_country_boundary(iso3_for_boundary,
                                      cache_dir = boundary_cache_dir)

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
    country_raw <- if (is.null(country_shp)) {
      0
    } else {
      tryCatch(
        sum(exactextractr::exact_extract(pop_raster, sf::st_geometry(country_shp), "sum"),
            na.rm = TRUE),
        error = function(e) 0
      )
    }
    tot_UN <- WPP2024$PopTotal[
      WPP2024$Time == yr & WPP2024$ISO3_code == iso3_for_boundary
    ] * 1e3
    adj_factor <- if (length(tot_UN) == 1L && country_raw > 0) {
      band_adj_factor(tot_UN / country_raw)$value
    } else {
      1.0
    }

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
    extracted <- as.numeric(raw_pops) * adj_factor
    # Zero is not a population — see add_population(). NA keeps the LP in the
    # "low" surveillance class instead of producing sCh/0 == Inf.
    extracted[!is.na(extracted) & extracted <= 0] <- NA_real_
    pop_out[keep_idx] <- extracted

    rm(pop_raster); gc(verbose = FALSE)
  }

  pop_out
}
