# 02_run_outbreak_detection.R  —  Batch 2: outbreak detection for one country
#
# Reads a per-country YAML config (detection_set), discovers all Stage 1 flat
# parquet files for that country (across all time windows), and runs
# identify_outbreaks() + trigger_alert() for each time window sequentially.
#
# Outputs one parquet file per country containing results across all windows:
#   stage2_{who_region}_{country_iso3}.parquet
#
# This script is intentionally single-threaded — the per-country loop is fast
# relative to data pull. SLURM parallelism happens at the country level.
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
library(arrow)

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
pattern    <- paste0("^stage1_flat_", opt$who_region, "_", opt$country_iso3, "_.*\\.parquet$")
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

out_file <- make_stage2_filename(opt$who_region, opt$country_iso3)

if (file.exists(out_file) && !isTRUE(opt$redo)) {
  message("Stage 2 output already exists, skipping: ", out_file)
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Run outbreak detection for each time window
# ---------------------------------------------------------------------------

results_list <- lapply(stage1_files, function(f) {

  # Parse time bounds from the filename (encoded as TL{YYYYMMDD}_TR{YYYYMMDD})
  fname  <- basename(f)
  tl_str <- str_extract(fname, "(?<=_TL)\\d{8}")
  tr_str <- str_extract(fname, "(?<=_TR)\\d{8}")

  if (is.na(tl_str) || is.na(tr_str)) {
    warning("Could not parse time bounds from filename: ", fname, " — skipping.")
    return(NULL)
  }

  tl <- lubridate::ymd(tl_str)
  tr <- lubridate::ymd(tr_str)
  run_id <- str_remove(str_remove(fname, "^stage1_flat_"), "\\.parquet$")

  message("Processing: ", run_id)

  normalized <- arrow::read_parquet(f)
  if (nrow(normalized) == 0) {
    message("  Empty Stage 1 file — skipping.")
    return(NULL)
  }

  # --- identify_outbreaks() ---
  outbreak_list <- tryCatch(
    OutbreakExtractR::identify_outbreaks(
      threshold_type                  = opt$threshold_type,
      original_data                   = normalized,
      zero_case_assumption            = opt$zero_case_assumption,
      customized_TL                   = tl,
      customized_TR                   = tr,
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
      tail_period                     = opt$tail_period
    ),
    error = function(e) {
      warning("identify_outbreaks() failed for ", run_id, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(outbreak_list)) return(NULL)

  # Flatten list → dataframe (one row per location-week)
  outbreaks_df <- purrr::list_rbind(
    purrr::keep(outbreak_list, \(x) is.data.frame(x) && nrow(x) > 0)
  )

  if (nrow(outbreaks_df) == 0) {
    message("  No outbreaks detected.")
    return(NULL)
  }

  # --- trigger_alert() ---
  alerts_df <- tryCatch(
    OutbreakExtractR::trigger_alert(original_data = normalized),
    error = function(e) {
      warning("trigger_alert() failed for ", run_id, ": ", conditionMessage(e))
      NULL
    }
  )

  # Attach alert columns if available
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

  # Attach run metadata for later aggregation
  outbreaks_df <- dplyr::mutate(
    outbreaks_df,
    who_region       = opt$who_region,
    country_iso3     = opt$country_iso3,
    time_lower_bound = as.character(tl),
    time_upper_bound = as.character(tr),
    run_id           = run_id
  )

  n_outbreak_rows <- sum(outbreaks_df$outbreak_number > 0, na.rm = TRUE)
  message("  Rows: ", nrow(outbreaks_df),
          "  |  Outbreak-period rows: ", n_outbreak_rows)

  outbreaks_df
})

# ---------------------------------------------------------------------------
# Combine and save
# ---------------------------------------------------------------------------

combined <- purrr::list_rbind(purrr::keep(results_list, \(x) !is.null(x)))

if (nrow(combined) == 0) {
  warning("No outbreak results to save for: ",
          opt$who_region, "::", opt$country_iso3)
  # Write empty parquet so post-processor can detect this gracefully
  arrow::write_parquet(data.frame(), out_file)
  quit(status = 0)
}

arrow::write_parquet(combined, out_file)

message("\nStage 2 complete.")
message("  Country:       ", opt$who_region, "::", opt$country_iso3)
message("  Time windows:  ", length(stage1_files))
message("  Total rows:    ", nrow(combined))
message("  Saved:         ", basename(out_file))
