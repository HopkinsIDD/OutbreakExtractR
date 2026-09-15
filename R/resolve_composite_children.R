# Directed per-child location-period + geometry retrieval for composite
# locations.
#
# A "composite location" is a surveillance observation whose location name joins
# several admin units with "|" (e.g. "AFR::SEN::Saint-Louis::Dagana::Mbane|Ross-Bethio").
# The Cholera Taxonomy `by_location` API returns such observations with
# location_period_id = NA and no geometry, so their constituent children carry no
# shape in the country-wide pull. build_composite_locations() therefore cannot
# always reconstruct a composite from atomic rows already present in the country
# pull (the children may never be observed atomically, or only under a different
# vocabulary such as "<name> Sanitary District").
#
# resolve_composite_children() makes a *directed* per-child API call: for every
# unique child location string it queries the API for that child alone and keeps
# the child's own location_period_id + polygon geometry when the API returns one.
# The resulting child LP/geometry can then be fed to build_composite_locations()
# (via raw_sf / a child-LP lookup) so previously-unresolved composites recover a
# real child-union geometry.
#
# The default time window is deliberately WIDE (2000-01-01 -> 2024-12-31):
# composites and their children frequently sit in early years (e.g. SEN's only
# composite is ~2002-2010), so a narrow detection window would miss them. The API
# requires a date range, so an unbounded pull is not possible.
#
# The pull function is injected (pull_fn) so the function is unit-testable without
# any network access, and each child pull is wrapped in tryCatch() so a single
# failing / empty child never aborts the whole batch. Optionally, a cache_dir
# memoizes each child's raw pull to a window-independent key so repeated runs
# (and overlapping detection windows) do not re-hit the API.

# Internal: an empty child-LP sf with the canonical schema.
empty_child_lp_sf <- function() {
  sf::st_sf(
    location           = character(0L),
    location_period_id = character(0L),
    geometry           = sf::st_sfc(crs = 4326L)
  )
}

# Internal: sanitize a location string into a filesystem-safe cache key.
child_cache_key <- function(child_location) {
  key <- gsub("[^A-Za-z0-9]+", "_", child_location)
  key <- gsub("^_+|_+$", "", key)
  paste0("raw_api_cache_child_", key, ".rds")
}

# Internal: resolve a single child location to its (location, LP, geometry) rows.
# Returns an sf with the canonical schema (possibly 0 rows). Never throws.
resolve_one_child <- function(child_location, time_left, time_right,
                              api_user, api_key, pull_fn, cache_dir,
                              location_prefix) {
  raw <- NULL

  cache_file <- if (!is.null(cache_dir)) {
    file.path(cache_dir, child_cache_key(child_location))
  } else {
    NULL
  }

  if (!is.null(cache_file) && file.exists(cache_file)) {
    raw <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  }

  if (is.null(raw)) {
    raw <- tryCatch(
      pull_fn(
        username   = api_user,
        api_key    = api_key,
        locations  = paste0(location_prefix, child_location),
        time_left  = time_left,
        time_right = time_right
      ),
      error = function(e) {
        message("  resolve_composite_children(): child pull failed for ",
                child_location, " -- ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(raw) && !is.null(cache_file)) {
      tryCatch(saveRDS(raw, cache_file), error = function(e) NULL)
    }
  }

  if (is.null(raw) || !inherits(raw, "sf") || nrow(raw) == 0L) {
    return(empty_child_lp_sf())
  }

  # Locate the LP-id column (API naming) robustly.
  lp_col <- intersect(
    c("attributes.location_period_id", "location_period_id"),
    names(raw)
  )[1]
  if (is.na(lp_col)) return(empty_child_lp_sf())

  raw$.lp <- as.character(raw[[lp_col]])

  # Keep only rows with a real LP and a real (dimension-2) polygon.
  dims <- suppressWarnings(sf::st_dimension(sf::st_geometry(raw)))
  keep <- !is.na(raw$.lp) & !is.na(dims) & dims == 2L
  raw  <- raw[keep, , drop = FALSE]
  if (nrow(raw) == 0L) return(empty_child_lp_sf())

  # One row per distinct LP (deterministic: lowest id first).
  raw <- raw[order(raw$.lp), , drop = FALSE]
  raw <- raw[!duplicated(raw$.lp), , drop = FALSE]

  sf::st_sf(
    location           = rep(child_location, nrow(raw)),
    location_period_id = raw$.lp,
    geometry           = sf::st_geometry(raw)
  )
}

#' @export
#' @title resolve_composite_children
#' @name resolve_composite_children
#' @description Make a directed per-child Cholera Taxonomy API call for every
#'   constituent child of a set of composite ("|"-joined) location names, and
#'   return each child's own location_period_id and polygon geometry. Intended
#'   for the Batch 1 pull step: the resulting child LP/geometry lets Stage 2
#'   \code{build_composite_locations()} reconstruct composites whose children are
#'   not observed atomically in the country-wide pull.
#' @param composite_names character: composite location names (containing "|").
#'   Names are de-composited with \code{decompose_composite_names()} to obtain the
#'   unique child location strings that are queried.
#' @param time_left,time_right Date: the (wide) pull window. Defaults to
#'   2000-01-01 .. 2024-12-31 because composites and their children often sit in
#'   early years; the API requires a bounded range.
#' @param api_user,api_key character: API credentials. Default to the
#'   \code{CHOLERA_API_USERNAME} / \code{CHOLERA_API_KEY} environment variables.
#' @param pull_fn function: the API pull function, injected for testability.
#'   Must accept \code{username, api_key, locations, time_left, time_right} and
#'   return an sf. Defaults to \code{taxdat::read_taxonomy_data_api}.
#' @param cache_dir character or NULL: if given, each child's raw pull is
#'   memoized to a window-independent key in this directory (deduplicating across
#'   overlapping windows and avoiding repeat API calls).
#' @param location_prefix character: prepended to each child location for the
#'   API query (default "CT-World::").
#' @return an sf keyed by location_period_id with columns
#'   \code{location} (the queried child string), \code{location_period_id}, and
#'   \code{geometry}. Returns a 0-row sf (canonical schema) when nothing resolves.
resolve_composite_children <- function(composite_names,
                                       time_left  = as.Date("2000-01-01"),
                                       time_right = as.Date("2024-12-31"),
                                       api_user   = Sys.getenv("CHOLERA_API_USERNAME"),
                                       api_key    = Sys.getenv("CHOLERA_API_KEY"),
                                       pull_fn    = taxdat::read_taxonomy_data_api,
                                       cache_dir  = NULL,
                                       location_prefix = "CT-World::") {

  composite_names <- unique(composite_names[!is.na(composite_names) &
                                              grepl("\\|", composite_names)])
  if (length(composite_names) == 0L) return(empty_child_lp_sf())

  children <- unique(decompose_composite_names(composite_names)$location)
  children <- children[!is.na(children) & nzchar(children)]
  if (length(children) == 0L) return(empty_child_lp_sf())

  if (!is.null(cache_dir) && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  resolved <- lapply(children, function(ch) {
    tryCatch(
      resolve_one_child(
        child_location  = ch,
        time_left       = time_left,
        time_right      = time_right,
        api_user        = api_user,
        api_key         = api_key,
        pull_fn         = pull_fn,
        cache_dir       = cache_dir,
        location_prefix = location_prefix
      ),
      error = function(e) {
        message("  resolve_composite_children(): failed to resolve ", ch,
                " -- ", conditionMessage(e))
        empty_child_lp_sf()
      }
    )
  })

  resolved <- Filter(function(x) !is.null(x) && nrow(x) > 0L, resolved)
  if (length(resolved) == 0L) return(empty_child_lp_sf())

  do.call(rbind, resolved)
}
