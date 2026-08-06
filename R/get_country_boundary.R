#' @export
#' @title get_country_boundary
#' @name get_country_boundary
#' @description Resolve a national boundary polygon for the UN population
#'   adjustment factor, deterministically and without ever falling back to the
#'   union of location-period geometries.
#'
#'   The resolution ladder is: on-disk cache -> \code{rgeoboundaries::gb_adm0()}
#'   -> \code{NULL}. There is deliberately no fourth step. A union of the
#'   observed LP geometries is *not* a country boundary — it covers only the
#'   surveilled sub-areas, so the raster total extracted on it is too small and
#'   the resulting adjustment factor (\code{tot_UN / country_raw}) is inflated,
#'   scaling every population in the country upward. Callers must treat
#'   \code{NULL} as "skip the adjustment" (\code{adj_factor = 1.0}), which
#'   leaves populations unadjusted rather than wrong.
#'
#'   \code{rgeoboundaries} is an optional dependency (Suggests). It is checked
#'   with \code{requireNamespace()} before use so that a missing package
#'   produces a legible message instead of being swallowed by a
#'   \code{tryCatch()} that cannot distinguish it from a network failure.
#'
#' @param country_iso3 character: ISO3 code, optionally with a sub-national
#'   suffix (e.g. "TZA::Mainland"); the leading 3-letter code is extracted.
#' @param cache_dir character or NULL: directory for caching boundary GeoJSON.
#'   When NULL, no cache is read or written.
#' @return an sf object with a single boundary geometry in EPSG:4326, or
#'   \code{NULL} if the boundary could not be resolved.
get_country_boundary <- function(country_iso3, cache_dir = "country_boundaries") {

  iso3 <- regmatches(toupper(country_iso3),
                     regexpr("[A-Z]{3}", toupper(country_iso3)))
  if (length(iso3) != 1L || is.na(iso3)) {
    message("get_country_boundary(): could not extract an ISO3 code from '",
            country_iso3, "' — returning NULL.")
    return(NULL)
  }

  # -- 1. Disk cache ---------------------------------------------------------
  cache_file <- NULL
  if (!is.null(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0(iso3, "_adm0.geojson"))
    if (file.exists(cache_file)) {
      cached <- tryCatch(
        sf::st_transform(sf::st_read(cache_file, quiet = TRUE), 4326),
        error = function(e) {
          message("get_country_boundary(): cached boundary for ", iso3,
                  " unreadable (", conditionMessage(e), ") — refetching.")
          NULL
        }
      )
      if (!is.null(cached) && nrow(cached) > 0L) return(cached)
    }
  }

  # -- 2. rgeoboundaries -----------------------------------------------------
  if (!requireNamespace("rgeoboundaries", quietly = TRUE)) {
    message("get_country_boundary(): package 'rgeoboundaries' is not installed, ",
            "so the boundary for ", iso3, " cannot be fetched. ",
            "The UN adjustment will be skipped (adj_factor = 1.0). ",
            "Install it to enable the adjustment.")
    return(NULL)
  }

  shp <- tryCatch(
    sf::st_transform(rgeoboundaries::gb_adm0(country = iso3), 4326),
    error = function(e) {
      message("get_country_boundary(): gb_adm0() failed for ", iso3, ": ",
              conditionMessage(e),
              " — the UN adjustment will be skipped (adj_factor = 1.0).")
      NULL
    }
  )
  if (is.null(shp) || nrow(shp) == 0L) return(NULL)

  # -- 3. Populate the cache -------------------------------------------------
  if (!is.null(cache_file)) {
    tryCatch({
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      sf::st_write(shp, cache_file, quiet = TRUE, delete_dsn = TRUE)
    }, error = function(e) {
      message("get_country_boundary(): could not cache boundary for ", iso3,
              ": ", conditionMessage(e))
    })
  }

  shp
}


#' @export
#' @title band_adj_factor
#' @name band_adj_factor
#' @description Apply the accept / flag / clamp banding to a UN population
#'   adjustment factor.
#'
#'   The observed corpus-wide range is 1.005-1.034, so any value far outside
#'   1.0 indicates that the raster total was extracted on the wrong polygon
#'   rather than that the country genuinely disagrees with WPP. Rather than
#'   propagate such a factor, extreme values are clamped to 1.0 (leaving the
#'   population unadjusted) and flagged.
#'
#' @param adj_factor numeric: the raw \code{tot_UN / country_raw} ratio.
#' @return a list with \code{value} (the factor to use) and \code{flag}, one of
#'   "ok", "wide", or "clamped".
band_adj_factor <- function(adj_factor) {
  if (!is.finite(adj_factor) || adj_factor <= 0) {
    return(list(value = 1.0, flag = "clamped"))
  }
  if (adj_factor >= 0.67 && adj_factor <= 1.5) {
    return(list(value = adj_factor, flag = "ok"))
  }
  if (adj_factor >= 0.5 && adj_factor <= 2.0) {
    message("    adj_factor ", signif(adj_factor, 4),
            " is outside the expected band [0.67, 1.5] — accepted but flagged.")
    return(list(value = adj_factor, flag = "wide"))
  }
  message("    adj_factor ", signif(adj_factor, 4),
          " is outside [0.5, 2.0] — clamped to 1.0 (population left unadjusted).")
  list(value = 1.0, flag = "clamped")
}
