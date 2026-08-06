#' @export
#' @title detect_duplicate_geometries
#' @name detect_duplicate_geometries
#' @description Identify location periods that share an identical polygon, and
#'   classify why.
#'
#'   The Cholera Taxonomy database stores a separate shape record per location
#'   period, but the *content* of those records is sometimes duplicated: several
#'   distinct location_period_ids, each with its own distinct shape id, resolve
#'   to byte-identical geometry. \code{add_population()} then extracts the same
#'   raster footprint for each of them and assigns them all the same population.
#'   This is a defect in the source data, not in the join — every LP receives
#'   the geometry the database associates with it.
#'
#'   Not every duplicate is harmful, so the cluster is classified:
#'
#'   \describe{
#'     \item{\code{unique}}{No other LP in the country shares this geometry.}
#'     \item{\code{alias}}{All members sit at the same hierarchy depth and share
#'       a base name once an ISO-style code prefix is stripped (e.g.
#'       \code{GN-B::Fria} and \code{GN-B::GN-FR.Fria}). These are duplicate
#'       records for one real place; the geometry is *correct*. They still
#'       double-count if a consumer sums population across LPs.}
#'     \item{\code{parent_inherited}}{Members span more than one hierarchy
#'       depth, i.e. children carry their parent's polygon (e.g. the Conakry
#'       region polygon on six Conakry sub-districts). Severe: each child is
#'       assigned the whole parent population.}
#'     \item{\code{cross_unit}}{Members sit at the same depth but have different
#'       base names, i.e. genuinely distinct units share one polygon (e.g.
#'       \code{Lagos::Shomolu} and \code{Nasarawa::Awe}). Severe: at least one
#'       unit has an entirely wrong denominator.}
#'     \item{\code{unknown}}{Duplicated, but no location names were supplied so
#'       the cluster could not be classified.}
#'   }
#'
#' @param lp_ids vector: location period identifiers, one per geometry.
#' @param geoms sfc or sf: geometries aligned with \code{lp_ids}.
#' @param lp_locations character or NULL: \code{::}-delimited location name per
#'   LP, aligned with \code{lp_ids}. When NULL, duplicated clusters are class
#'   \code{"unknown"}.
#' @return a tibble with columns \code{location_period_id},
#'   \code{pop_geom_dup_n} (size of the identical-geometry cluster; 1 means
#'   unique) and \code{pop_geom_dup_class}.
detect_duplicate_geometries <- function(lp_ids, geoms, lp_locations = NULL) {

  n <- length(lp_ids)
  if (n == 0L) {
    return(dplyr::tibble(location_period_id = lp_ids,
                         pop_geom_dup_n = integer(0),
                         pop_geom_dup_class = character(0)))
  }

  geoms_sfc <- if (inherits(geoms, "sf")) sf::st_geometry(geoms) else geoms

  # Geometry identity key. Empty geometries are never treated as duplicates of
  # one another: they carry no footprint, so sharing "emptiness" says nothing.
  wkt <- vapply(geoms_sfc, function(g) {
    if (is.null(g)) return(NA_character_)
    txt <- tryCatch(sf::st_as_text(g), error = function(e) NA_character_)
    txt
  }, character(1L))

  empty <- is.na(wkt) | vapply(geoms_sfc, function(g) {
    if (is.null(g)) return(TRUE)
    isTRUE(tryCatch(sf::st_is_empty(sf::st_sfc(g)), error = function(e) TRUE))
  }, logical(1L))

  if (requireNamespace("digest", quietly = TRUE)) {
    key <- vapply(seq_len(n), function(i) {
      if (empty[i]) NA_character_ else digest::digest(wkt[i])
    }, character(1L))
  } else {
    key <- ifelse(empty, NA_character_, wkt)
  }

  # Cluster size: NA keys (empty geometry) are always singletons.
  dup_n <- rep(1L, n)
  keyed <- which(!is.na(key))
  if (length(keyed) > 0L) {
    tab <- table(key[keyed])
    dup_n[keyed] <- as.integer(tab[key[keyed]])
  }

  dup_class <- rep("unique", n)

  if (is.null(lp_locations)) {
    dup_class[dup_n > 1L] <- "unknown"
  } else {
    depth <- lengths(strsplit(as.character(lp_locations), "::", fixed = TRUE))
    leaf  <- vapply(strsplit(as.character(lp_locations), "::", fixed = TRUE),
                    function(p) if (length(p) == 0L) NA_character_ else p[length(p)],
                    character(1L))
    # "GN-FR.Fria" and "Fria" name the same unit under two conventions.
    base <- tolower(trimws(sub("^[A-Za-z]{2}-[A-Za-z0-9]+\\.", "", leaf)))

    for (k in unique(key[keyed])) {
      idx <- which(key == k & !is.na(key))
      if (length(idx) < 2L) next
      dup_class[idx] <- if (dplyr::n_distinct(depth[idx]) > 1L) {
        "parent_inherited"
      } else if (dplyr::n_distinct(base[idx]) == 1L) {
        "alias"
      } else {
        "cross_unit"
      }
    }
  }

  dplyr::tibble(
    location_period_id = lp_ids,
    pop_geom_dup_n     = dup_n,
    pop_geom_dup_class = dup_class
  )
}
