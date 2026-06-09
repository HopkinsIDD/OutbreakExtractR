#!/bin/bash
# install_r_packages.sh — install all R packages required by the OutbreakExtractR pipeline
#
# Run ONCE from a LOGIN NODE (not a compute node — internet access needed for GitHub):
#
#   cd /path/to/OutbreakExtractR
#   bash analysis/bash/install_r_packages.sh
#
# What this installs:
#   - All DESCRIPTION Imports + analysis-layer Suggests (from CRAN)
#   - taxdat (from GitHub: HopkinsIDD/cholera-taxonomy)
#   - OutbreakExtractR itself (from the current directory)
#
# Before running, verify the libdeflate module name for your cluster:
#   module spider libdeflate
# Then set LIBDEFLATE_MODULE below to the version matching GCCcore-11.3.0.

set -euo pipefail


# ---------------------------------------------------------------------------
# Modules — toolchain must match R/4.2.1-foss-2022a (built with GCCcore-11.3.0)
# ---------------------------------------------------------------------------
module purge
module load GCCcore/13.3.0 GCC/13.3.0 libdeflate/1.20 Abseil/20240722.0 OpenMPI/5.0.3 R/4.4.2

echo "R:       $(Rscript --version 2>&1)"
echo "Library: $(Rscript -e 'cat(.libPaths()[1])' 2>/dev/null)"
echo ""

# ---------------------------------------------------------------------------
# Run R install script
# Must be executed from the OutbreakExtractR project root.
# ---------------------------------------------------------------------------
REPO_ROOT="$(pwd)"

Rscript - "$REPO_ROOT" <<'REOF'

repo_root <- commandArgs(trailingOnly = TRUE)[1]
repo      <- "https://cloud.r-project.org"

# ---- CRAN packages ---------------------------------------------------------
pkgs <- c(
  # OutbreakExtractR Imports (DESCRIPTION)
  "DBI", "RPostgres", "glue", "sf",
  "dplyr", "purrr", "tidyr", "stringr", "tibble",
  "lubridate", "zoo", "tidyselect", "rlang", "magrittr",
  "slider", "curl", "raster", "exactextractr",

  # analysis/ layer: configs, CLI, parquet I/O, parallelism
  "yaml", "optparse", "here",
  "arrow", "sfarrow",
  "furrr", "future",

  # dev / testing
  "testthat", "remotes"
)

missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(missing_pkgs) > 0) {
  message("Installing ", length(missing_pkgs), " CRAN package(s): ",
          paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = repo)
} else {
  message("All CRAN packages already installed.")
}

# ---- taxdat (private GitHub package) ---------------------------------------
if (!"taxdat" %in% rownames(installed.packages())) {
  message("Installing taxdat from GitHub (HopkinsIDD/cholera-taxonomy) ...")
  remotes::install_github("HopkinsIDD/cholera-taxonomy", upgrade = "never")
} else {
  message("taxdat already installed.")
}

# ---- OutbreakExtractR from source ------------------------------------------
message("Installing OutbreakExtractR from source: ", repo_root)
install.packages(repo_root, repos = NULL, type = "source")

message("\n===== Installation complete =====")
message("Packages installed in: ", .libPaths()[1])

REOF
