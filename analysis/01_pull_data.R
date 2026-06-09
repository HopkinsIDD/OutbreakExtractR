# 01_pull_data.R  —  Batch 1: pull + normalize data for one country × time window
#
# Reads a YAML config, calls the Cholera Taxonomy API via taxdat, runs the
# full OutbreakExtractR normalization pipeline, and writes two GeoParquet files:
#   stage1_geo_{run_id}.parquet   — sf object (retains geometry, for spatial use)
#   stage1_flat_{run_id}.parquet  — flat dataframe (no geometry, input to Batch 2)
#
# Skips gracefully if outputs already exist (re-run with --redo TRUE to force).
#
# Usage:
#   Rscript analysis/01_pull_data.R -c analysis/configs/pull_set/pull_set_1.yml
#   Rscript analysis/01_pull_data.R -c analysis/configs/pull_set/pull_set_1.yml --redo TRUE
#
# Required environment variables:
#   CHOLERA_API_USERNAME   — Cholera Taxonomy API username
#   CHOLERA_API_KEY        — Cholera Taxonomy API key

library(here)
library(optparse)
library(dplyr)
library(lubridate)
library(sf)

source(here("analysis/utils.R"))

# ---------------------------------------------------------------------------
# CLI / config parsing
# ---------------------------------------------------------------------------

option_list <- list(
  make_option(c("-c", "--config"),
              default = NULL, type = "character",
              help    = "Path to YAML config file (required)"),
  make_option(c("--redo"),
              default = FALSE, type = "logical",
              help    = "Force re-pull even if output already exists [default: FALSE]")
)

opt <- make_options_from_config(option_list, enforce_options = "redo")
print_options(opt)

# ---------------------------------------------------------------------------
# Skip if already done
# ---------------------------------------------------------------------------

out_geo      <- make_stage1_geo_filename(opt)
out_flat     <- make_stage1_flat_filename(opt)
out_api_cache <- file.path(
  here(opt$output_dir),
  paste0("raw_api_cache_", make_run_id(opt), ".rds")
)
dir.create(dirname(out_flat), recursive = TRUE, showWarnings = FALSE)

if (file.exists(out_flat) && !isTRUE(opt$redo)) {
  message("Stage 1 output already exists, skipping: ", out_flat)
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Credentials — only required when no API cache exists
# ---------------------------------------------------------------------------

api_user <- Sys.getenv("CHOLERA_API_USERNAME", unset = NA_character_)
api_key  <- Sys.getenv("CHOLERA_API_KEY",      unset = NA_character_)

if (!file.exists(out_api_cache) && (is.na(api_user) || is.na(api_key))) {
  stop("CHOLERA_API_USERNAME and CHOLERA_API_KEY environment variables must be set.")
}

# ---------------------------------------------------------------------------
# Stage 1a: pull raw data from Cholera Taxonomy API
# ---------------------------------------------------------------------------

# Patch taxdat::flatten_json_result to handle list columns that contain nested
# data frames or raw vectors — these cause jsonlite::flatten() to throw
# "list columns are only allowed with raw vector contents". The fix drops any
# such column before flattening; they carry no information used downstream.
utils::assignInNamespace(
  "flatten_json_result",
  function(json_results) {
    if (!is.data.frame(json_results)) json_results <- as.data.frame(json_results)

    # jsonlite::flatten() fails when nested data frames contain raw-vector
    # list columns ("list columns are only allowed with raw vector contents").
    # Fix: walk every list-of-data-frame column recursively and strip only the
    # raw-vector leaf columns — do NOT remove the parent data frame columns,
    # because jsonlite::flatten() needs them to produce the attributes.* names.
    clean_df <- function(df) {
      for (col in names(df)) {
        v <- df[[col]]
        if (is.data.frame(v)) {
          # Nested data frame column: recurse directly — do NOT lapply over it,
          # which would iterate columns (not rows) and produce wrong-length output.
          df[[col]] <- clean_df(v)
        } else if (is.list(v)) {
          # Pure list column: remove if any element is a raw vector
          if (any(vapply(v, is.raw, logical(1L)))) {
            message("  [flatten_json_result patch] removing raw-vector column: ", col)
            df[[col]] <- NULL
          }
        }
      }
      df
    }

    json_results <- clean_df(json_results)
    json_results <- jsonlite::flatten(json_results)

    for (colname in names(json_results)) {
      if (mode(json_results[[colname]]) == "list") {
        if (max(sapply(json_results[[colname]], length)) == 1) {
          json_results[[colname]] <- sapply(json_results[[colname]], function(x) {
            ifelse(length(x) == 1, x, NA)
          })
        }
      }
    }
    json_results
  },
  ns = "taxdat"
)

location_str <- make_taxdat_location(opt$who_region, opt$country_iso3)

# Pull raw data from API — use cache if available to skip the network call on
# debug reruns.  Delete raw_api_cache_*.rds manually (or with --redo-api) to
# force a fresh pull.
if (file.exists(out_api_cache)) {
  message("Loading cached API response: ", basename(out_api_cache))
  raw_api <- readRDS(out_api_cache)
} else {
  message("Pulling data: ", location_str,
          "  [", opt$time_lower_bound, " → ", opt$time_upper_bound, "]")
  raw_api <- taxdat::read_taxonomy_data_api(
    username   = api_user,
    api_key    = api_key,
    locations  = location_str,
    time_left  = as.Date(opt$time_lower_bound),
    time_right = as.Date(opt$time_upper_bound)
  )
  saveRDS(raw_api, out_api_cache)
  message("Cached API response: ", basename(out_api_cache))
}

# Select and rename API columns to OutbreakExtractR conventions.
# rename_database_fields() maps attributes.id → locationPeriod_id, but the
# correct LP identifier in the API response is attributes.location_period_id.
# Using direct column selection based on actual API response structure.
# Selecting only needed columns also drops list columns with raw-vector elements
# that would cause sf::st_write to fail.
raw_sf <- raw_api %>%
  dplyr::select(
    dplyr::any_of(c(
      "relationships.observation_collection.data.id",
      "attributes.time_left",
      "attributes.time_right",
      "attributes.fields.suspected_cases",
      "attributes.fields.confirmed_cases",
      "attributes.fields.deaths",
      "attributes.location_period_id",
      "attributes.primary",
      "attributes.location_name"
    ))
  ) %>%
  dplyr::rename(
    observation_collection_id = relationships.observation_collection.data.id,
    TL                        = attributes.time_left,
    TR                        = attributes.time_right,
    sCh                       = attributes.fields.suspected_cases,
    cCh                       = attributes.fields.confirmed_cases,
    deaths                    = attributes.fields.deaths,
    location_period_id        = attributes.location_period_id,
    primary                   = attributes.primary,
    location                  = attributes.location_name
  )

if (is.null(raw_sf) || nrow(raw_sf) == 0) {
  warning("API returned no data for: ", location_str,
          "  [", opt$time_lower_bound, " → ", opt$time_upper_bound, "]")
  # Write empty sentinel files so Batch 2 can detect and skip gracefully
  write_tabular(data.frame(), out_flat, opt$use_geoparquet)
  quit(status = 0)
}

message("Pulled ", nrow(raw_sf), " raw observations.")

# Save raw sf with geometry as GeoParquet (useful for spatial visualisation)
write_spatial(raw_sf, out_geo, opt$use_geoparquet)
message("Saved raw geo file: ", basename(out_geo))

# ---------------------------------------------------------------------------
# Stage 1b: normalize through the OutbreakExtractR pipeline
# ---------------------------------------------------------------------------

# Drop geometry — OutbreakExtractR functions operate on flat dataframes
raw_df <- sf::st_drop_geometry(raw_sf)

# Clean: standardize types, identify spatial/temporal scale, clean location names
clean_data <- OutbreakExtractR::clean_psql_data(raw_df)

# Filter by time, scale, and case thresholds
filtered_data <- OutbreakExtractR::observation_filter(
  outbreak_data            = clean_data,
  time_lower_bound_filter  = lubridate::ymd(opt$time_lower_bound),
  time_upper_bound_filter  = lubridate::ymd(opt$time_upper_bound),
  temporal_scale_filter    = opt$temporal_scale_filter,
  who_regions              = opt$who_region,
  spatial_scale_filter     = opt$spatial_scale_filter,
  remove_na_sCh            = opt$remove_na_sCh,
  remove_na_cCh            = opt$remove_na_cCh,
  remove_na_locationperiod = opt$remove_na_locationperiod,
  minimum_daily_cases      = opt$minimum_daily_cases
)

# Separate daily and weekly; aggregate daily → weekly
daily_data  <- dplyr::filter(filtered_data, temporal_scale == "daily")
weekly_data <- dplyr::filter(filtered_data, temporal_scale == "weekly")

if (nrow(daily_data) > 0) {
  aggregated_daily <- OutbreakExtractR::observation_aggregator(daily_data)
  weekly_data <- dplyr::bind_rows(weekly_data, aggregated_daily)
}

# Normalize weekly data: deduplicate, align week-start day, fill zeros
normalized <- weekly_data %>%
  dplyr::ungroup() %>%
  OutbreakExtractR::average_duplicate_observations() %>%
  OutbreakExtractR::set_uniform_wday_start() %>%
  OutbreakExtractR::fill_phantom_zeroes() %>%
  OutbreakExtractR::fill_missing_lps()

# Attach WorldPop population estimates (one value per location_period_id).
# Required downstream by get_outbreak_threshold() and identify_epidemic_start()
# for incidence-based threshold modes.
# Rasters are downloaded once into opt$raster_dir and cached for subsequent runs.
normalized <- OutbreakExtractR::add_population(
  normalized_data = normalized,
  raw_sf          = raw_sf,    # has location_period_id + geometry (sf select preserves geom)
  country_iso3    = opt$country_iso3,
  raster_dir      = here::here(opt$raster_dir)
)

# ---------------------------------------------------------------------------
# Save flat parquet for Batch 2
# ---------------------------------------------------------------------------

write_tabular(normalized, out_flat, opt$use_geoparquet)

message("Stage 1 complete.")
message("  Rows:      ", nrow(normalized))
message("  Locations: ", length(unique(normalized$location)))
message("  Saved:     ", basename(out_flat))
