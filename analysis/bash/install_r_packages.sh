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
# !! This takes a while to complete


set -euo pipefail


# ---------------------------------------------------------------------------
# Modules — toolchain
# ---------------------------------------------------------------------------
module purge
module load GCCcore/12.3.0 GCC/12.3.0 libdeflate/1.18 Abseil/20230125.3 OpenMPI/4.1.5 R/4.3.2  GDAL/3.7.1 CMake

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

  # population estimation (add_population / get_pop)
  "rgeoboundaries",

  # analysis/ layer: configs, CLI, parquet I/O, parallelism
  "yaml", "optparse", "here",
  "arrow", "sfarrow",
  "furrr", "future",
  
  # for taxdat
  "ISOcodes", "readr", "reshape2",

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
# NOTE: Needed to rebuild the documentation rm -rf man/ NAMESPACE && Rscript -e "devtools::document()"
  message("Installing taxdat from GitHub (HopkinsIDD/cholera-taxonomy) ...")
  remotes::install_version("Matrix", version = "1.6-5", repos = "https://cran.r-project.org")
  remotes::install_github("HopkinsIDD/cholera-taxonomy/packages/taxdat", upgrade = "never")
} else {
  message("taxdat already installed.")
}

# ---- OutbreakExtractR from source ------------------------------------------
message("Installing OutbreakExtractR from source: ", repo_root)
install.packages(repo_root, repos = NULL, type = "source")

message("\n===== Installation complete =====")
message("Packages installed in: ", .libPaths()[1])

REOF
