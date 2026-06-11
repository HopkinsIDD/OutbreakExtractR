#!/usr/bin/env Rscript
# shiny/data_prep.R ─────────────────────────────────────────────────────────
# Run ONCE from the project root to build the centroid lookup used by the app.
#
#   Rscript shiny/data_prep.R
#
# Output: shiny/data/centroids.rds
#   A data frame: location (chr) | lon (dbl) | lat (dbl)
#   One row per unique location string found across all stage1_geo GeoJSONs.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(here)
})

cat("=== Cholera Outbreak Explorer — centroid build ===\n")
t0 <- proc.time()

geojson_dir   <- here("analysis/generated_data")
geojson_files <- list.files(geojson_dir,
                             pattern    = "^stage1_geo_.*\\.geojson$",
                             full.names = TRUE)
cat(sprintf("Found %d GeoJSON files in %s\n", length(geojson_files), geojson_dir))

if (length(geojson_files) == 0) {
  stop("No stage1_geo_*.geojson files found. Run 01_pull_data.R first.")
}

# ── One representative file per country prefix ────────────────────────────────
# Naming: stage1_geo_{REGION}_{ISO3}_TL{date}_TR{date}_thresh-*.geojson
# Extract everything up to the first "_TL" as the "country key".
country_key <- str_extract(basename(geojson_files), "^stage1_geo_.+?(?=_TL\\d)")
country_key[is.na(country_key)] <- basename(geojson_files)[is.na(country_key)]

# Pick the largest file per key — larger files tend to have more admin levels
files_df <- tibble(path = geojson_files, key = country_key,
                   size = file.size(geojson_files))

# For each country-key, pick the largest file in each 4-year time window.
# This ensures that administrative boundary changes over time are captured
# (e.g. DRC reorganised from 11 → 26 provinces in 2015; older files carry the
# old names, 2017-2018 files carry the new names, recent files may only have
# admin3 data).  Within-window deduplication, then global dedup, collapses
# overlapping centroids without double-counting.
rep_files_by_key <- files_df %>%
  filter(!is.na(key)) %>%
  mutate(
    tl_year   = as.integer(str_extract(basename(path), "(?<=_TL)[0-9]{4}")),
    window    = (tl_year %/% 4L) * 4L        # 2010, 2014, 2018, 2022, …
  ) %>%
  group_by(key, window) %>%
  slice_max(size, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(key, desc(size)) %>%
  group_by(key) %>%
  summarise(candidates = list(path), .groups = "drop") %>%
  { setNames(.$candidates, .$key) }

rep_files <- sapply(rep_files_by_key, `[[`, 1)   # still used for progress counter

cat(sprintf("Reading %d representative files (one per country, with fallback on error)...\n", length(rep_files)))

# ── Extract (location, centroid) from each file ───────────────────────────────
read_centroids <- function(path) {
  tryCatch({
    obj <- suppressWarnings(sf::st_read(path, quiet = TRUE))
    if (!inherits(obj, "sf") || nrow(obj) == 0) return(NULL)
    if (!"location" %in% names(obj))           return(NULL)

    # Deduplicate on location BEFORE centroid computation.
    # Large files (e.g. COD 380 MB / 306K rows) have many time-repeated geometries
    # for the same location; deduplication makes st_centroid tractable.
    obj <- obj[!duplicated(obj$location), ]

    # Repair any invalid geometries (e.g. duplicate vertices, self-intersections)
    # before computing centroids.
    obj <- suppressWarnings(sf::st_make_valid(obj))

    suppressWarnings(cents <- sf::st_centroid(obj))
    coords <- sf::st_coordinates(cents)

    tibble(
      location = as.character(obj$location),
      lon      = coords[, "X"],
      lat      = coords[, "Y"]
    )
  }, error = function(e) {
    message(sprintf("  [WARN] %s: %s", basename(path), conditionMessage(e)))
    NULL
  })
}

# Process one file per 4-year window per country; merge results.
# All windows are attempted; failures are skipped without aborting the country.
keys <- names(rep_files_by_key)
results <- vector("list", length(keys))
for (i in seq_along(keys)) {
  if (i %% 10 == 0 || i == length(keys))
    cat(sprintf("  [%d/%d] %s  (%d window files)\n",
                i, length(keys), keys[i], length(rep_files_by_key[[keys[i]]])))
  per_country <- lapply(rep_files_by_key[[keys[i]]], function(cand) {
    res <- read_centroids(cand)
    if (is.null(res) || nrow(res) == 0) {
      message(sprintf("  [skip] %s", basename(cand)))
      return(NULL)
    }
    res
  })
  combined <- bind_rows(per_country)
  results[[i]] <- if (nrow(combined) > 0) combined else NULL
}

centroids <- bind_rows(results) %>%
  filter(!is.na(lon), !is.na(lat), is.finite(lon), is.finite(lat)) %>%
  distinct(location, .keep_all = TRUE)

cat(sprintf("\nUnique locations with centroids (from GeoJSONs): %d\n", nrow(centroids)))

# ── Back-propagate parent centroids ──────────────────────────────────────────
# Some location strings used in the outbreak CSV are *aggregate* nodes that
# have no direct geometry in the GeoJSONs (e.g. "AFR::TZA::Mainland").
# We approximate their centroid as the mean of their children's centroids,
# repeating up the hierarchy until no new parents can be inferred.
add_parent_centroids <- function(df) {
  repeat {
    existing <- df$location
    parents <- df %>%
      mutate(parent = str_replace(location, "::[^:]+$", "")) %>%
      filter(parent != location, !parent %in% existing) %>%
      group_by(parent) %>%
      summarise(lon = mean(lon, na.rm = TRUE),
                lat = mean(lat, na.rm = TRUE),
                .groups = "drop") %>%
      rename(location = parent)

    if (nrow(parents) == 0) break
    df <- bind_rows(df, parents)
  }
  df
}

centroids <- add_parent_centroids(centroids)
cat(sprintf("Unique locations after parent back-propagation: %d\n", nrow(centroids)))

# ── Save ──────────────────────────────────────────────────────────────────────
out_path <- here("shiny/data/centroids.rds")
saveRDS(centroids, out_path)

elapsed <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("Saved to %s  (%.0f s)\n", out_path, elapsed))
