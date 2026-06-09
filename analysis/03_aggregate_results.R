# 03_aggregate_results.R  —  Post-processing: aggregate all Stage 2 outputs
#
# Run locally (not on SLURM) after all Batch 2 jobs complete.
# Loads every stage2_{who_region}_{country_iso3}.{rds,parquet} file in generated_data/,
# binds rows, and saves a combined CSV (plus Parquet when --format geoparquet).
#
# Uses furrr::future_map for parallel file loading across workers.
#
# Usage:
#   Rscript analysis/03_aggregate_results.R
#   Rscript analysis/03_aggregate_results.R --workers 4 --set_name cholera_v2
#   Rscript analysis/03_aggregate_results.R --format geoparquet

library(here)
library(optparse)
library(purrr)
library(furrr)
library(future)
library(dplyr)
library(stringr)

# ---------------------------------------------------------------------------
# CLI args
# ---------------------------------------------------------------------------

option_list <- list(
  make_option(c("-w", "--workers"),
              default = 8L, type = "integer",
              help    = "Number of parallel workers for loading files [default: 8]"),
  make_option(c("-s", "--set_name"),
              default = "cholera", type = "character",
              help    = "Label for the combined output filename [default: cholera]"),
  make_option(c("-o", "--out_dir"),
              default = here("analysis/generated_data"), type = "character",
              help    = "Directory containing stage2_* files"),
  make_option(c("-f", "--format"),
              default = "geojson", type = "character",
              help    = "File format: 'geojson' (default, RDS tabular) or 'geoparquet' [default: geojson]")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (!opt$format %in% c("geojson", "geoparquet")) {
  stop("--format must be 'geojson' or 'geoparquet', got: ", opt$format)
}
use_geoparquet <- opt$format == "geoparquet"

cat("Workers:", opt$workers, "\n")
cat("Set name:", opt$set_name, "\n")
cat("Output dir:", opt$out_dir, "\n")
cat("Format:", opt$format, "\n\n")

# ---------------------------------------------------------------------------
# Discover Stage 2 parquet files
# ---------------------------------------------------------------------------

stage2_ext   <- if (use_geoparquet) "\\.parquet" else "\\.rds"
stage2_files <- list.files(opt$out_dir,
                            pattern    = paste0("^stage2_.*", stage2_ext, "$"),
                            full.names = TRUE)

if (length(stage2_files) == 0) {
  stop("No stage2_* ", opt$format, " files found in: ", opt$out_dir,
       "\nRun Batch 2 (02_run_outbreak_detection.R) first.")
}

cat("Found", length(stage2_files), "Stage 2 file(s):\n")
cat(paste0("  ", basename(stage2_files), collapse = "\n"), "\n\n")

# ---------------------------------------------------------------------------
# Load in parallel
# ---------------------------------------------------------------------------

plan(multisession, workers = opt$workers)

results_list <- future_map(stage2_files, function(f) {
  tryCatch({
    df <- if (use_geoparquet) arrow::read_parquet(f) else readRDS(f)
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    warning("Failed to read: ", basename(f), " — ", conditionMessage(e))
    NULL
  })
}, .options = furrr_options(seed = TRUE))

plan(sequential)

# ---------------------------------------------------------------------------
# Combine
# ---------------------------------------------------------------------------

combined <- purrr::list_rbind(purrr::keep(results_list, \(x) !is.null(x)))

cat("Combined result:\n")
cat("  Total rows:    ", nrow(combined), "\n")
cat("  Countries:     ", length(unique(combined$country_iso3)), "\n")
cat("  WHO regions:   ", length(unique(combined$who_region)), "\n")
cat("  Time windows:  ",
    length(unique(paste0(combined$time_lower_bound, "_", combined$time_upper_bound))), "\n\n")

# ---------------------------------------------------------------------------
# Save combined outputs
# ---------------------------------------------------------------------------

out_csv <- file.path(opt$out_dir,
                     paste0("combined_outbreaks_", opt$set_name, ".csv"))

if (use_geoparquet) {
  out_parquet <- file.path(opt$out_dir,
                           paste0("combined_outbreaks_", opt$set_name, ".parquet"))
  arrow::write_parquet(combined, out_parquet)
  message("Saved parquet: ", out_parquet)
}

write.csv(combined, out_csv, row.names = FALSE)
message("Saved CSV:     ", out_csv)
