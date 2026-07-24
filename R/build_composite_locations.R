# Composite-location handling for Stage 2 outbreak detection.
#
# A "composite location" is an observation whose location name joins several
# admin units with "|" (e.g. "AFR::BDI::Cankuzo::Cankuzo|Cendajuru|Kigamba")
# and which the taxonomy API returns with location_period_id = NA and no
# geometry. Such rows survive Stage 1 but are silently dropped during Stage 2:
# add_population() skips NA location_period_id, so pop = NA, so
# get_outbreak_threshold() forces risk = "low" and identify_outbreaks() never
# flags an epidemic start.
#
# build_composite_locations() reproduces the handling previously done in
# GenevaIDD/global-cholera-surveillance-timeseries Step2_Extract_outbreak.R:
# de-composite the joined name into its child admin units, look up each child's
# location_period_id and population from the atomic rows already present in the
# normalized data (matching exactly, then on a name normalized to strip the
# " Sanitary District" suffix the taxonomy appends to health-system units),
# assign the composite a synthetic "composite_loc_<ISO3>_<n>"
# id with summed child population, and build the composite geometry as the
# union of its children's geometries. The rewritten rows then flow through
# identify_outbreaks() like any ordinary location.
#
# Population (geometry-derived, matching the colleague's reference):
# When a raster_dir is supplied, the composite's population is estimated
# directly from the constrained WorldPop raster on its (child-union or
# parent-fallback) geometry via estimate_pop_for_geometries(). This is the true
# sub-area denominator the reference script computed with get_pop() on the
# unioned shapefile, and it gives accurate incidence for composites whose
# children resolve to real geometry. The summed-child pop and parent-polygon pop
# are retained only as fallbacks (for composites whose geometry the raster could
# not resolve). Without a raster_dir the older summed-child / parent pop is used.
#
# Fallback (parent-location approximation):
# When composite children are not observed atomically (e.g. BDI sanitary
# districts that only ever appear in aggregate), no matched-child geometry is
# available. In that case the function falls back to the parent admin location
# (the prefix before the first "|" token) for geometry (and, without a
# raster_dir, for pop). This is an approximation: the incidence denominator then
# covers the full parent area rather than just the composite subunits, so
# detection thresholds are correspondingly lower (incidence underestimated).
# With a raster_dir, WorldPop-on-the-parent-geometry is still used, but it
# remains a parent-area (over-estimated) denominator for these composites.

# Internal: split composite names into one (composite_name, location) row per
# child admin unit. Mirrors Step2_Extract_outbreak.R:105-134 — handles a "|" at
# any admin depth and decodes the "##" deeper-admin separator into "::".
decompose_composite_names <- function(composite_names) {
  out <- vector("list", length(composite_names))

  for (i in seq_along(composite_names)) {
    nm     <- composite_names[i]
    tokens <- strsplit(nm, "::", fixed = TRUE)[[1]]
    rows   <- list()

    # Reproduce the original per-column loop: every admin token that contains a
    # "|" yields a set of children (paths up to and including that token).
    for (k in seq_along(tokens)) {
      if (!grepl("|", tokens[k], fixed = TRUE)) next

      prefix     <- if (k > 1L) paste(tokens[seq_len(k - 1L)], collapse = "::") else ""
      pieces     <- strsplit(tokens[k], "|", fixed = TRUE)[[1]]
      child_locs <- vapply(pieces, function(p) {
        full <- if (nzchar(prefix)) paste(prefix, p, sep = "::") else p
        gsub("##", "::", full, fixed = TRUE)
      }, character(1))

      rows[[length(rows) + 1L]] <- data.frame(
        composite_name = nm,
        location       = child_locs,
        stringsAsFactors = FALSE
      )
    }

    if (length(rows) > 0L) out[[i]] <- do.call(rbind, rows)
  }

  res <- do.call(rbind, out)
  if (is.null(res)) {
    return(data.frame(composite_name = character(0L),
                      location       = character(0L),
                      stringsAsFactors = FALSE))
  }
  res
}

# Internal: resolve each composite-child location to at most one atomic
# location_period_id + pop. It matches on the exact location string first, then
# falls back to a normalized key that strips a trailing " Sanitary District"
# from the terminal admin token. The taxonomy stores many BDI health-system
# units as "<name> Sanitary District" while composite children carry the bare
# admin name, so the exact join alone leaves most children unmatched even though
# the like-named district LP (and its geometry) is present in the country pull.
# Each child is resolved to a SINGLE LP (deterministic: prefer non-NA pop, then
# lowest location_period_id) so that downstream population sums over distinct
# child LPs never double-count multiple location periods of the same place.
match_children_to_lps <- function(child_tbl, loc_lookup) {
  strip_sd <- function(x) sub(" Sanitary District$", "", x)

  atomic <- loc_lookup %>%
    dplyr::mutate(.key_norm = strip_sd(location)) %>%
    dplyr::arrange(is.na(pop), location_period_id)

  exact <- atomic %>%
    dplyr::distinct(location, .keep_all = TRUE) %>%
    dplyr::select(location,
                  lp_exact  = location_period_id,
                  pop_exact = pop)

  norm <- atomic %>%
    dplyr::distinct(.key_norm, .keep_all = TRUE) %>%
    dplyr::select(.key_norm,
                  lp_norm  = location_period_id,
                  pop_norm = pop)

  child_tbl %>%
    dplyr::mutate(.key_norm = strip_sd(location)) %>%
    dplyr::left_join(exact, by = "location") %>%
    dplyr::left_join(norm, by = ".key_norm") %>%
    dplyr::mutate(
      location_period_id = dplyr::coalesce(lp_exact, lp_norm),
      pop = dplyr::if_else(!is.na(lp_exact), pop_exact, pop_norm)
    ) %>%
    dplyr::select(composite_name, location, location_period_id, pop)
}

# Internal: return the parent location string (all tokens before the first
# pipe-containing token). Returns NA_character_ when the pipe is in the first
# or second token (country level — no meaningful parent available).
get_composite_parent <- function(composite_name) {
  tokens     <- strsplit(composite_name, "::", fixed = TRUE)[[1]]
  first_pipe <- which(vapply(tokens, function(t) grepl("|", t, fixed = TRUE),
                             logical(1L)))[1L]
  if (is.na(first_pipe) || first_pipe <= 2L) return(NA_character_)
  paste(tokens[seq_len(first_pipe - 1L)], collapse = "::")
}

#' @export
#' @title build_composite_locations
#' @name build_composite_locations
#' @description Resolve composite locations (NA location_period_id, "|"-joined
#'   names) into synthetic "composite_loc_<ISO3>_<n>" pseudo location periods so
#'   that Stage 2 outbreak detection can run on them. Geometry is the union of a
#'   composite's children's geometries from raw_sf (parent-location fallback when
#'   children are not individually observed). Population: when \code{raster_dir}
#'   is supplied, it is estimated directly from WorldPop on that composite
#'   geometry (the true sub-area denominator); otherwise it is the sum of the
#'   children's WorldPop populations (attached by add_population()), with a
#'   parent-location pop fallback.
#' @param normalized data.frame: the normalized weekly data AFTER
#'   add_population(), so atomic location_period_ids carry a pop column.
#' @param raw_sf sf: geometry-bearing data from the Stage 1 geo files, keyed by
#'   location_period_id.
#' @param iso3 character: ISO3 country code, used to namespace composite ids and
#'   to gate the known-LP corrections for SSD/SOM.
#' @param raster_dir character or NULL: when supplied, each composite's
#'   population is estimated directly from the constrained WorldPop raster on the
#'   composite (child-union or parent-fallback) geometry via
#'   \code{estimate_pop_for_geometries()}. This geometry-derived denominator is
#'   the true sub-area population and is the primary source; the summed-child and
#'   parent-polygon populations (steps 5/5b) are retained only as fallbacks for
#'   composites whose geometry the raster extraction could not resolve. When NULL
#'   (default), the previous summed-child / parent-polygon behaviour is used.
#' @return list(data = normalized with composites resolved,
#'              geometry = sf(lctn_pr, area_per_1km2, geometry) for composites,
#'                         or NULL when there are none).
build_composite_locations <- function(normalized, raw_sf, iso3,
                                       raster_dir = NULL) {

  iso3 <- toupper(regmatches(iso3, regexpr("[A-Z]{3}", iso3)))

  normalized <- normalized %>%
    dplyr::mutate(location_period_id = as.character(location_period_id))

  # 1. Identify composite rows -----------------------------------------------
  is_composite <- is.na(normalized$location_period_id) &
    stringr::str_detect(normalized$location, "\\|")

  if (!any(is_composite)) {
    return(list(data = apply_known_lp_fixes(normalized, iso3), geometry = NULL))
  }

  composite_names <- unique(normalized$location[is_composite])
  message("build_composite_locations(): ", length(composite_names),
          " composite location(s) in ", iso3)

  # 2. De-composite names into child location strings ------------------------
  child_tbl <- decompose_composite_names(composite_names)

  # 3. Map child location -> location_period_id + pop (from atomic rows) ------
  loc_lookup <- normalized %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::distinct(location, location_period_id, pop)

  # Extend loc_lookup with dot-stripped country keys: "AFR::BDI.Burundi" is
  # stored in normalized but the parent prefix derived from composite names is
  # "AFR::BDI". Adding the stripped variant allows parent lookups to match.
  loc_lookup_extended <- dplyr::bind_rows(
    loc_lookup,
    loc_lookup %>%
      dplyr::mutate(location = sub("\\.[^:]+$", "", location)) %>%
      dplyr::anti_join(loc_lookup, by = "location")
  )

  cp <- match_children_to_lps(child_tbl, loc_lookup)

  n_missing <- sum(is.na(cp$location_period_id))
  if (n_missing > 0L) {
    miss <- unique(cp$location[is.na(cp$location_period_id)])
    message("  ", n_missing, " child location(s) had no atomic match: ",
            paste(utils::head(miss, 5L), collapse = "; "),
            if (length(miss) > 5L) " ..." else "")
  }

  # 4. Assign composite ids ---------------------------------------------------
  comp_ids <- data.frame(
    composite_name = composite_names,
    composite_id   = paste0("composite_loc_", iso3, "_", seq_along(composite_names)),
    stringsAsFactors = FALSE
  )

  # 5. Composite population = sum of distinct matched child-LP populations ----
  comp_pop_raw <- cp %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::distinct(composite_name, location_period_id, pop) %>%
    dplyr::group_by(composite_name) %>%
    dplyr::summarise(composite_pop = sum(pop, na.rm = TRUE), .groups = "drop")

  # Ensure every composite name is represented (zero when no children matched).
  comp_pop <- comp_ids %>%
    dplyr::select(composite_name, composite_id) %>%
    dplyr::left_join(comp_pop_raw, by = "composite_name") %>%
    dplyr::mutate(
      composite_pop = dplyr::if_else(is.na(composite_pop), 0, composite_pop)
    )

  # 5b. Parent-location fallback: replace 0-pop composites with their parent's
  # pop (all children were unobserved — e.g. BDI sanitary-district composites).
  zero_pop_composites <- comp_pop$composite_name[comp_pop$composite_pop == 0]
  if (length(zero_pop_composites) > 0L) {
    parent_pop_df <- data.frame(
      composite_name  = zero_pop_composites,
      parent_location = vapply(zero_pop_composites, get_composite_parent,
                               character(1L)),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::filter(!is.na(parent_location)) %>%
      dplyr::left_join(
        dplyr::select(loc_lookup_extended,
                      parent_location = location,
                      parent_pop = pop),
        by = "parent_location"
      )

    n_ok  <- sum(!is.na(parent_pop_df$parent_pop))
    n_bad <- length(zero_pop_composites) - n_ok
    if (n_ok > 0L)
      message("  ", n_ok,
              " composite(s) using parent-location pop as fallback ",
              "(children not observed atomically — denominator approximated).")
    if (n_bad > 0L)
      message("  ", n_bad,
              " composite(s) have no pop (children + parent both absent); ",
              "detection thresholds will be NaN.")

    comp_pop <- comp_pop %>%
      dplyr::left_join(
        dplyr::select(parent_pop_df, composite_name, parent_pop),
        by = "composite_name"
      ) %>%
      dplyr::mutate(
        composite_pop = dplyr::if_else(
          composite_pop == 0 & !is.na(parent_pop),
          parent_pop,
          composite_pop
        )
      ) %>%
      dplyr::select(-parent_pop)
  }

  # 6. Composite geometry: union of matched child geometries (sf left table) --
  geom_lookup <- raw_sf %>%
    dplyr::mutate(location_period_id = as.character(location_period_id)) %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::group_by(location_period_id) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    sf::st_make_valid()

  child_lp_map <- cp %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::mutate(location_period_id = as.character(location_period_id)) %>%
    dplyr::distinct(composite_name, location_period_id) %>%
    dplyr::left_join(comp_ids, by = "composite_name")

  # geom_lookup is sf (left table) → result is sf with geometry
  composite_geom <- geom_lookup %>%
    dplyr::inner_join(child_lp_map, by = "location_period_id") %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    dplyr::group_by(composite_id) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    dplyr::mutate(
      lctn_pr       = composite_id,
      area_per_1km2 = as.numeric(sf::st_area(
        sf::st_transform(geometry, "+proj=moll")
      )) / 1e6
    ) %>%
    dplyr::select(lctn_pr, area_per_1km2)

  # 6b. Parent-location geometry fallback for composites still without geom ---
  composites_with_geom <- composite_geom$lctn_pr  # character(0) when 0 rows
  needs_parent_geom    <- comp_ids$composite_id[
    !comp_ids$composite_id %in% composites_with_geom
  ]

  if (length(needs_parent_geom) > 0L) {
    names_needing <- comp_ids$composite_name[
      comp_ids$composite_id %in% needs_parent_geom
    ]
    parent_lp_df <- data.frame(
      composite_id    = needs_parent_geom,
      parent_location = vapply(names_needing, get_composite_parent,
                               character(1L)),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::filter(!is.na(parent_location)) %>%
      dplyr::left_join(
        dplyr::select(loc_lookup_extended,
                      parent_location = location,
                      location_period_id = location_period_id),
        by = "parent_location"
      ) %>%
      dplyr::filter(!is.na(location_period_id)) %>%
      dplyr::mutate(location_period_id = as.character(location_period_id)) %>%
      dplyr::distinct(composite_id, location_period_id)

    if (nrow(parent_lp_df) > 0L) {
      # geom_lookup is sf (left table) → inner_join keeps sf semantics
      parent_geom <- geom_lookup %>%
        dplyr::inner_join(parent_lp_df, by = "location_period_id") %>%
        dplyr::filter(!sf::st_is_empty(geometry)) %>%
        dplyr::mutate(
          lctn_pr       = composite_id,
          area_per_1km2 = as.numeric(sf::st_area(
            sf::st_transform(geometry, "+proj=moll")
          )) / 1e6
        ) %>%
        dplyr::select(lctn_pr, area_per_1km2)

      if (nrow(parent_geom) > 0L) {
        message("  ", nrow(parent_geom),
                " composite(s) using parent geometry as fallback.")
        composite_geom <- rbind(composite_geom, parent_geom)
      }
    }
  }

  if (nrow(composite_geom) > 0L) {
    sf::st_crs(composite_geom) <- sf::st_crs(raw_sf)
  } else {
    composite_geom <- NULL
  }

  # 6c. Geometry-derived population (primary source when raster_dir given) ----
  # Estimate each composite's population directly from WorldPop on its
  # (child-union or parent-fallback) geometry. This is the true sub-area
  # denominator; it overrides the summed-child / parent-polygon pop from step 5
  # wherever the raster extraction resolves. Composites whose geometry could not
  # be resolved keep the step-5 fallback pop.
  if (!is.null(raster_dir) && !is.null(composite_geom) &&
      nrow(composite_geom) > 0L) {

    # Representative year per composite = median year(TL) over its rows.
    comp_year <- normalized %>%
      dplyr::filter(location %in% comp_ids$composite_name) %>%
      dplyr::left_join(comp_ids, by = c("location" = "composite_name")) %>%
      dplyr::group_by(composite_id) %>%
      dplyr::summarise(
        year = as.integer(stats::median(lubridate::year(TL))),
        .groups = "drop"
      )

    geom_years <- data.frame(lctn_pr = composite_geom$lctn_pr,
                             stringsAsFactors = FALSE) %>%
      dplyr::left_join(comp_year, by = c("lctn_pr" = "composite_id"))
    geom_years$year[is.na(geom_years$year)] <-
      as.integer(stats::median(geom_years$year, na.rm = TRUE))

    geom_pop_vals <- tryCatch(
      estimate_pop_for_geometries(
        geom_sf      = composite_geom,
        country_iso3 = iso3,
        year         = geom_years$year,
        raster_dir   = raster_dir
      ),
      error = function(e) {
        message("  build_composite_locations(): geometry-derived pop failed (",
                conditionMessage(e), ") — keeping summed-child/parent pop.")
        rep(NA_real_, nrow(composite_geom))
      }
    )

    geom_pop_df <- comp_ids %>%
      dplyr::left_join(
        data.frame(composite_id = composite_geom$lctn_pr,
                   geom_pop      = as.numeric(geom_pop_vals),
                   stringsAsFactors = FALSE),
        by = "composite_id"
      ) %>%
      dplyr::select(composite_name, geom_pop)

    n_geom_pop <- sum(!is.na(geom_pop_df$geom_pop) & geom_pop_df$geom_pop > 0)
    if (n_geom_pop > 0L)
      message("  ", n_geom_pop,
              " composite(s) using WorldPop-on-geometry as population ",
              "(true sub-area denominator).")

    comp_pop <- comp_pop %>%
      dplyr::left_join(geom_pop_df, by = "composite_name") %>%
      dplyr::mutate(
        composite_pop = dplyr::if_else(
          !is.na(geom_pop) & geom_pop > 0,
          geom_pop,
          composite_pop
        )
      ) %>%
      dplyr::select(-geom_pop)
  }

  # 7. Rewrite composite rows in the normalized data -------------------------
  data_out <- normalized %>%
    dplyr::left_join(comp_ids, by = c("location" = "composite_name")) %>%
    dplyr::left_join(
      dplyr::select(comp_pop, composite_name, composite_pop),
      by = c("location" = "composite_name")
    ) %>%
    dplyr::mutate(
      location_period_id = dplyr::coalesce(composite_id, location_period_id),
      pop = dplyr::if_else(!is.na(composite_id), composite_pop, pop),
      # 8. Tag spatial_scale for composite rows.
      spatial_scale = dplyr::if_else(
        !is.na(composite_id),
        paste(as.character(spatial_scale), "composite"),
        as.character(spatial_scale)
      )
    ) %>%
    dplyr::select(-composite_id, -composite_pop)

  # 9. Known atomic-LP corrections (SSD/SOM only) ----------------------------
  data_out <- apply_known_lp_fixes(data_out, iso3)

  list(data = data_out, geometry = composite_geom)
}

# Internal: hardcoded location_period_id corrections carried over from
# Step2_Extract_outbreak.R:199-211. These are atomic-LP fixes (not composite
# logic) and only affect SSD/SOM, so they are a no-op elsewhere.
apply_known_lp_fixes <- function(df, iso3) {
  if (!iso3 %in% c("SSD", "SOM")) return(df)
  df %>%
    dplyr::mutate(location_period_id = dplyr::case_when(
      location == "AFR::SSD::Unity::Rubkona"   ~ "5624",
      location == "AFR::SSD::Upper Nile::Renk" ~ "7859",
      location == "EMR::SOM::Hiiraan"          ~ "9373",
      TRUE                                     ~ location_period_id
    ))
}
