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

out_geo  <- make_stage1_geo_filename(opt)
out_flat <- make_stage1_flat_filename(opt)
dir.create(dirname(out_flat), recursive = TRUE, showWarnings = FALSE)

if (file.exists(out_flat) && !isTRUE(opt$redo)) {
  message("Stage 1 output already exists, skipping: ", out_flat)
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Credentials
# ---------------------------------------------------------------------------

api_user <- Sys.getenv("CHOLERA_API_USERNAME", unset = NA_character_)
api_key  <- Sys.getenv("CHOLERA_API_KEY",      unset = NA_character_)

if (is.na(api_user) || is.na(api_key)) {
  stop("CHOLERA_API_USERNAME and CHOLERA_API_KEY environment variables must be set.")
}

# ---------------------------------------------------------------------------
# Stage 1a: pull raw data from Cholera Taxonomy API
# ---------------------------------------------------------------------------

location_str <- make_taxdat_location(opt$who_region, opt$country_iso3)
message("Pulling data: ", location_str,
        "  [", opt$time_lower_bound, " → ", opt$time_upper_bound, "]")

# NOTE: The server certificate covers the base domain only, not the api.
# subdomain — both ssl_verifypeer and ssl_verifyhost are disabled until the
# cert is fixed. ssl_verifypeer controls CA trust; ssl_verifyhost (must be 0L,
# not FALSE) controls the SAN/CN hostname match — this is the specific check
# that fails here.
httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
raw_sf <- taxdat::read_taxonomy_data_api(
  username   = api_user,
  api_key    = api_key,
  locations  = location_str,
  time_left  = as.character(opt$time_lower_bound),
  time_right = as.character(opt$time_upper_bound),
  website    = opt$api_website
) %>%
  taxdat::rename_database_fields(source = "api")
httr::reset_config()

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
  raw_sf          = raw_sf,
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
