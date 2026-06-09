# 00_make_configs.R
# Generates YAML config files for the two SLURM batches:
#   pull_set      — one config per country × time_window  (Batch 1)
#   detection_set — one config per country                (Batch 2)
#
# After running, the script prints the --array bounds to use in the SLURM
# submission scripts (submit_01_pull_data.sh, submit_02_detection.sh).
#
# Usage: Rscript analysis/00_make_configs.R

library(here)
library(tibble)
library(tidyr)

source(here("analysis/utils.R"))

# ---------------------------------------------------------------------------
# 1. Define the country × WHO region grid
# ---------------------------------------------------------------------------
# Add or remove rows to control which countries are analysed.
# country_iso3 must match the ISO3 code used by the Cholera Taxonomy API.

countries <- tibble::tribble(
  ~who_region, ~country_iso3,
  "AFR",       "COD",
  "AFR",       "NGA",
  "AFR",       "ETH",
  "AFR",       "MOZ",
  "AFR",       "ZMB",
  "EMR",       "SOM",
  "EMR",       "SDN"
)

# ---------------------------------------------------------------------------
# 2. Define time windows
# ---------------------------------------------------------------------------
# Each row is one pull window. Each window is a given month to make data download manageable.

# time_windows <- tibble::tribble(
#   ~time_lower_bound,  ~time_upper_bound,
#   "2010-01-01",       "2015-12-31",
#   "2013-01-01",       "2018-12-31",
#   "2016-01-01",       "2021-12-31",
#   "2018-01-01",       "2023-12-31"
# )

tstart <- as.Date("2010-01-01")
tend <- as.Date("2024-12-31")
tseq <- seq.Date(tstart, tend, by = "4 months")

time_windows <- tibble(
  time_lower_bound = tseq + 1,
  time_upper_bound = c(tseq[-1], tend)
)


# ---------------------------------------------------------------------------
# 3. (Optional) outbreak-detection parameter variants
# ---------------------------------------------------------------------------
# Uncomment to generate separate jobs per outbreak_start_definition variant.
# param_variants <- tibble::tribble(
#   ~threshold_type,               ~outbreak_start_definition,
#   "mean weekly incidence rate",  "consecutive",
#   "mean weekly incidence rate",  "dual_window"
# )

# ---------------------------------------------------------------------------
# 4. Generate config sets
# ---------------------------------------------------------------------------

# pull_set: country × time_window (for Batch 1 data pull)
pull_specs <- tidyr::crossing(countries, time_windows) %>% 
  dplyr::arrange(time_lower_bound)
# pull_specs <- tidyr::crossing(countries, time_windows, param_variants)
write_configs(pull_specs, "pull_set")

# detection_set: country only (for Batch 2 outbreak detection)
# Each Batch 2 task globs all Stage 1 parquet files for that country.
write_configs(countries, "detection_set")

# ---------------------------------------------------------------------------
# 5. Print SLURM array bounds
# ---------------------------------------------------------------------------
cat("\n=== SLURM array bounds ===\n")
cat("submit_01_pull_data.sh   → --array=0-", nrow(pull_specs) - 1,    "%25\n", sep = "")
cat("submit_02_detection.sh   → --array=0-", nrow(countries)  - 1,    "%10\n", sep = "")
cat("\n")
cat("Total Batch 1 jobs:", nrow(pull_specs), "\n")
cat("Total Batch 2 jobs:", nrow(countries),  "\n")

# ---------------------------------------------------------------------------
# 6. Minimal test config (1 country, 1 window — for local dry runs)
# ---------------------------------------------------------------------------
test_specs <- tibble::tribble(
  ~who_region, ~country_iso3, ~time_lower_bound, ~time_upper_bound,
  "AFR",       "ETH",         "2020-01-01",      "2020-03-01"
)
write_configs(test_specs, "test_pull")
write_configs(dplyr::select(test_specs, who_region, country_iso3), "test_detection")
cat("\nTest configs written to analysis/configs/test_pull/ and test_detection/\n")
