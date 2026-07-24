# 02_run_outbreak_detection.R  —  Batch 2: outbreak detection for one country
#
# Reads a per-country YAML config (detection_set), concatenates all Stage 1
# flat files for that country (one per 4-month pull window), and runs the
# full reference processing pipeline + identify_outbreaks() + trigger_alert()
# once over the entire per-country time series.
#
# Processing pipeline matches Step2_Extract_outbreak.R from
# GenevaIDD/global-cholera-surveillance-timeseries:
#   filter (daily → aggregate; weekly) → fill_missing_lps ×3 →
#   average_duplicate_observations → set_uniform_wday_start →
#   filter(n_obs > 1) → fill_phantom_zeroes → add_population →
#   identify_outbreaks (threshold over full series, no customized_TL/TR)
#
# Outputs one file per country:
#   stage2_{who_region}_{country_iso3}.{rds,parquet}
#
# SLURM parallelism happens at the country level (one job per country).
#
# Usage:
#   Rscript analysis/02_run_outbreak_detection.R \
#     -c analysis/configs/detection_set/detection_set_1.yml

library(here)
library(optparse)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(sf)
sf_use_s2(FALSE)

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
              help    = "Force re-run even if output already exists [default: FALSE]")
)

opt <- make_options_from_config(option_list, enforce_options = "redo")
print_options(opt)

# ---------------------------------------------------------------------------
# Discover Stage 1 flat parquet files for this country
# ---------------------------------------------------------------------------

stage1_dir <- here("analysis/generated_data")
stage1_ext <- if (isTRUE(opt$use_geoparquet)) "\\.parquet" else "\\.rds"
pattern    <- paste0("^stage1_flat_", opt$who_region, "_", opt$country_iso3, "_.*", stage1_ext, "$")
stage1_files <- list.files(stage1_dir, pattern = pattern, full.names = TRUE)

if (length(stage1_files) == 0) {
  stop("No Stage 1 flat parquet files found for ",
       opt$who_region, "::", opt$country_iso3,
       " in: ", stage1_dir,
       "\nRun Batch 1 (01_pull_data.R) first.")
}

message("Found ", length(stage1_files), " Stage 1 file(s) for ",
        opt$who_region, "::", opt$country_iso3)

# ---------------------------------------------------------------------------
# Skip if already done
# ---------------------------------------------------------------------------

out_file <- make_stage2_filename(opt$who_region, opt$country_iso3, opt$use_geoparquet)

if (file.exists(out_file) && !isTRUE(opt$redo)) {
  message("Stage 2 output already exists, skipping: ", out_file)
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Concatenate all per-window cleaned observations for this country
# ---------------------------------------------------------------------------

clean_list <- lapply(stage1_files, function(f) {
  tryCatch({
    df <- read_tabular(f, opt$use_geoparquet)
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    warning("Failed to read Stage 1 flat file: ", basename(f), " — ", conditionMessage(e))
    NULL
  })
})

clean_all <- purrr::list_rbind(purrr::keep(clean_list, \(x) !is.null(x)))

if (nrow(clean_all) == 0) {
  warning("No Stage 1 observations for: ", opt$who_region, "::", opt$country_iso3)
  write_tabular(data.frame(), out_file, opt$use_geoparquet)
  quit(status = 0)
}

message("Loaded ", nrow(clean_all), " cleaned observations across ",
        length(stage1_files), " window(s).")

# ---------------------------------------------------------------------------
# Load per-window geo files → raw_sf for population attachment
# ---------------------------------------------------------------------------

geo_ext     <- if (isTRUE(opt$use_geoparquet)) "\\.parquet" else "\\.geojson"
# Use opt$country_iso3 directly (with :: as-is) to match actual filenames.
# gsub("::", "_", ...) was a bug: geo files retain :: in their names just like
# stage1 flat files, so the underscore-substituted pattern never matched.
geo_pattern <- paste0("^stage1_geo_", opt$who_region, "_",
                      opt$country_iso3, "_.*", geo_ext, "$")
geo_files   <- list.files(stage1_dir, pattern = geo_pattern, full.names = TRUE)
# Exclude composite sidecars written by a prior run of this script — they have
# a different schema (only location_period_id + area_per_1km2 + geometry) and
# must not be rbind-ed with the full Stage 1 geo files.
geo_files   <- geo_files[!grepl("_composite\\.geojson$|_composite\\.parquet$",
                                basename(geo_files))]

if (length(geo_files) == 0) {
  warning("No Stage 1 geo files found for population attachment — pop will be NA.")
  raw_sf <- NULL
} else {
  geo_list <- lapply(geo_files, function(f) {
    tryCatch(
      if (isTRUE(opt$use_geoparquet)) sfarrow::st_read_parquet(f)
      else sf::st_read(f, quiet = TRUE),
      error = function(e) {
        warning("Failed to read geo file: ", basename(f), " — ", conditionMessage(e))
        NULL
      }
    )
  })
  raw_sf <- do.call(rbind, Filter(Negate(is.null), geo_list))
  message("Loaded geometry from ", length(geo_files), " geo file(s).")
}

# ---------------------------------------------------------------------------
# Derive full time range from window filenames
# ---------------------------------------------------------------------------

tl_strings <- str_extract(basename(stage1_files), "(?<=_TL)\\d{8}")
tr_strings <- str_extract(basename(stage1_files), "(?<=_TR)\\d{8}")
tl_all <- min(lubridate::ymd(tl_strings), na.rm = TRUE)
tr_all <- max(lubridate::ymd(tr_strings), na.rm = TRUE)

message("Full time range: ", tl_all, " → ", tr_all)

# ---------------------------------------------------------------------------
# Reference processing pipeline (matches Step2_Extract_outbreak.R:26-67)
# ---------------------------------------------------------------------------

# Daily branch: filter then aggregate to weekly
daily_data <- OutbreakExtractR::observation_filter(
  outbreak_data            = clean_all,
  time_lower_bound_filter  = tl_all,
  time_upper_bound_filter  = tr_all,
  temporal_scale_filter    = "daily",
  who_regions              = opt$who_region,
  spatial_scale_filter     = opt$spatial_scale_filter,
  remove_na_sCh            = opt$remove_na_sCh,
  remove_na_cCh            = opt$remove_na_cCh,
  remove_na_locationperiod = opt$remove_na_locationperiod,
  minimum_daily_cases      = opt$minimum_daily_cases
)
if (nrow(daily_data) > 0) {
  daily_data <- OutbreakExtractR::observation_aggregator(daily_data)
}

# Weekly branch: filter then coerce id columns to character (avoids bind_rows type conflicts)
weekly_data <- OutbreakExtractR::observation_filter(
  outbreak_data            = clean_all,
  time_lower_bound_filter  = tl_all,
  time_upper_bound_filter  = tr_all,
  temporal_scale_filter    = "weekly",
  who_regions              = opt$who_region,
  spatial_scale_filter     = opt$spatial_scale_filter,
  remove_na_sCh            = opt$remove_na_sCh,
  remove_na_cCh            = opt$remove_na_cCh,
  remove_na_locationperiod = opt$remove_na_locationperiod,
  minimum_daily_cases      = opt$minimum_daily_cases
) %>%
  dplyr::mutate(
    observation_collection_id = as.character(observation_collection_id),
    dplyr::across(dplyr::any_of("original_location_name"), as.character)
  )

combined_filtered <- dplyr::bind_rows(weekly_data, daily_data)

if (nrow(combined_filtered) == 0) {
  warning("No observations after filtering for: ", opt$who_region, "::", opt$country_iso3)
  write_tabular(data.frame(), out_file, opt$use_geoparquet)
  quit(status = 0)
}

# Normalization: fill_missing_lps (×3), dedup, wday alignment, singleton drop, phantom zeros
normalized <- combined_filtered %>%
  OutbreakExtractR::fill_missing_lps() %>%
  OutbreakExtractR::average_duplicate_observations() %>%
  OutbreakExtractR::fill_missing_lps() %>%
  OutbreakExtractR::set_uniform_wday_start() %>%
  dplyr::group_by(location) %>%
  dplyr::add_count(name = "n_obs") %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_obs > 1) %>%
  dplyr::select(-n_obs) %>%
  OutbreakExtractR::fill_phantom_zeroes() %>%
  OutbreakExtractR::fill_missing_lps()

message("Normalized: ", nrow(normalized), " rows, ",
        length(unique(normalized$location)), " location(s).")

if (nrow(normalized) == 0) {
  warning("No data after normalization for: ", opt$who_region, "::", opt$country_iso3)
  write_tabular(data.frame(), out_file, opt$use_geoparquet)
  quit(status = 0)
}

# Population attachment (WorldPop, keyed by location_period_id + geometry from geo files)
if (!is.null(raw_sf)) {
  normalized <- OutbreakExtractR::add_population(
    normalized_data = normalized,
    raw_sf          = raw_sf,
    country_iso3    = opt$country_iso3,
    raster_dir      = here::here(opt$raster_dir)
  )
} else {
  normalized$pop <- NA_real_
  message("Skipping add_population() — no geo files found; pop set to NA.")
}

# ---------------------------------------------------------------------------
# Resolve composite locations (NA location_period_id, "|"-joined names) into
# composite_loc_<ISO3>_* pseudo-LPs with unioned child geometry and a
# WorldPop-on-geometry population (raster_dir passed below), so they survive
# detection (otherwise NA pop drops them).
# ---------------------------------------------------------------------------

composite_geom <- NULL
if (!is.null(raw_sf)) {
  comp <- tryCatch(
    OutbreakExtractR::build_composite_locations(
      normalized = normalized,
      raw_sf     = raw_sf,
      iso3       = opt$country_iso3,
      raster_dir = here::here(opt$raster_dir)
    ),
    error = function(e) {
      warning("build_composite_locations() failed for ",
              opt$who_region, "::", opt$country_iso3, ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(comp)) {
    normalized     <- comp$data
    composite_geom <- comp$geometry
  }
}

# ---------------------------------------------------------------------------
# Outbreak detection over full per-country series (no customized_TL/TR)
# Threshold = mean weekly incidence over the entire time series, matching reference
# ---------------------------------------------------------------------------

outbreak_list <- tryCatch(
  OutbreakExtractR::identify_outbreaks(
    threshold_type                  = opt$threshold_type,
    original_data                   = normalized,
    zero_case_assumption            = opt$zero_case_assumption,
    outbreak_start_definition       = opt$outbreak_start_definition,
    min_weeks_above                 = opt$min_weeks_above,
    require_increasing_trend        = opt$require_increasing_trend,
    window_weeks                    = opt$window_weeks,
    cumulative_windows              = opt$cumulative_windows,
    cumulative_case_threshold_ratio = opt$cumulative_case_threshold_ratio,
    cumulative_trigger_type         = opt$cumulative_trigger_type,
    use_cumulative_trigger          = opt$use_cumulative_trigger,
    cumulative_min_cases            = opt$cumulative_min_cases,
    nonzero_windows                 = opt$nonzero_windows,
    tail_period                     = opt$tail_period,
    filter_outbreaks_by_size        = isTRUE(opt$filter_outbreaks_by_size)
  ),
  error = function(e) {
    warning("identify_outbreaks() failed for ",
            opt$who_region, "::", opt$country_iso3, ": ", conditionMessage(e))
    NULL
  }
)

if (is.null(outbreak_list)) {
  write_tabular(data.frame(), out_file, opt$use_geoparquet)
  quit(status = 0)
}

outbreaks_df <- purrr::list_rbind(
  purrr::keep(outbreak_list, \(x) is.data.frame(x) && nrow(x) > 0)
)

if (nrow(outbreaks_df) == 0) {
  message("No outbreaks detected for: ", opt$who_region, "::", opt$country_iso3)
  write_tabular(data.frame(), out_file, opt$use_geoparquet)
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Alerts
# ---------------------------------------------------------------------------

alerts_df <- tryCatch(
  OutbreakExtractR::trigger_alert(original_data = normalized),
  error = function(e) {
    warning("trigger_alert() failed: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(alerts_df) && nrow(alerts_df) > 0) {
  alert_cols <- names(alerts_df)[str_detect(names(alerts_df), "^alert")]
  join_keys  <- intersect(c("location", "TL", "TR"), names(alerts_df))
  if (length(join_keys) > 0 && length(alert_cols) > 0) {
    outbreaks_df <- dplyr::left_join(
      outbreaks_df,
      dplyr::select(alerts_df, dplyr::all_of(c(join_keys, alert_cols))),
      by = join_keys
    )
  }
}

# ---------------------------------------------------------------------------
# Add metadata and save
# ---------------------------------------------------------------------------

outbreaks_df <- dplyr::mutate(
  outbreaks_df,
  who_region       = opt$who_region,
  country_iso3     = opt$country_iso3,
  time_lower_bound = as.character(tl_all),
  time_upper_bound = as.character(tr_all)
)

n_outbreak_rows <- sum(outbreaks_df$outbreak_number > 0, na.rm = TRUE)

write_tabular(outbreaks_df, out_file, opt$use_geoparquet)

# ---------------------------------------------------------------------------
# Composite geometry sidecar
#
# The converter (00_ingest_outbreakextractr.R) builds outbreak_shapefiles.rds
# by globbing stage1_geo_(AFR|EMR)_*.geojson and computing area itself. Composite
# pseudo-LPs have no geometry in the per-window geo files, so emit a sidecar that
# matches that glob, keyed by location_period_id = composite_loc_<ISO3>_*.
# ---------------------------------------------------------------------------

if (!is.null(composite_geom) && nrow(composite_geom) > 0) {
  composite_geo_file <- file.path(
    stage1_dir,
    paste0("stage1_geo_", opt$who_region, "_", opt$country_iso3, "_composite.geojson")
  )
  tryCatch({
    sf::st_write(
      composite_geom %>% dplyr::rename(location_period_id = lctn_pr),
      composite_geo_file,
      delete_dsn = TRUE,
      quiet      = TRUE
    )
    message("  Composite geometries: ", nrow(composite_geom),
            " → ", basename(composite_geo_file))
  }, error = function(e) {
    warning("Failed to write composite geometry sidecar: ", conditionMessage(e))
  })
}

message("\nStage 2 complete.")
message("  Country:          ", opt$who_region, "::", opt$country_iso3)
message("  Full time range:  ", tl_all, " → ", tr_all)
message("  Total rows:       ", nrow(outbreaks_df))
message("  Outbreak-period rows: ", n_outbreak_rows)
message("  Saved:            ", basename(out_file))
