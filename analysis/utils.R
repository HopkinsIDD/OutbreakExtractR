# utils.R — shared helpers for the OutbreakExtractR analysis pipeline
# Sourced by 00_make_configs.R, 01_pull_data.R, 02_run_outbreak_detection.R,
# and 03_aggregate_results.R.

library(yaml)
library(here)
library(stringr)
library(purrr)
library(optparse)

# ---------------------------------------------------------------------------
# Config I/O
# ---------------------------------------------------------------------------

get_default_config <- function() {
  yaml::read_yaml(here("analysis/config_defaults.yml"))
}

get_config_dir <- function() {
  here("analysis/configs")
}

#' Write one YAML config file per row of config_specs.
#' Loads config_defaults.yml, then overwrites each field present in config_specs.
write_configs <- function(config_specs, set_name) {
  out_dir <- file.path(get_config_dir(), set_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  walk(seq_len(nrow(config_specs)), function(x) {
    cfg <- get_default_config()
    for (nm in names(as.list(config_specs[x, ]))) {
      cfg[[nm]] <- config_specs[[nm]][x]
    }
    yaml::write_yaml(cfg, file.path(out_dir, paste0(set_name, "_", x, ".yml")))
  })
  message("Wrote ", nrow(config_specs), " configs to: ", out_dir)
}

#' Parse arguments from a YAML config file (-c flag) or direct CLI flags.
#' enforce_options: character vector of option names that should always come
#'   from the CLI (overriding the config), useful for --redo.
make_options_from_config <- function(option_list, enforce_options = NULL) {
  opt_ <- parse_args(OptionParser(option_list = option_list))
  opt  <- if (!is.null(opt_$config)) yaml::read_yaml(opt_$config) else opt_
  if (!is.null(enforce_options)) {
    for (i in enforce_options) opt[[i]] <- opt_[[i]]
  }
  opt
}

# ---------------------------------------------------------------------------
# taxdat location string helper
# ---------------------------------------------------------------------------

#' Build the location string expected by taxdat::pull_taxonomy_data().
#' Format: "CT-World::{who_region}::{country_iso3}"
make_taxdat_location <- function(who_region, country_iso3) {
  paste("CT-World", who_region, country_iso3, sep = "::")
}

# ---------------------------------------------------------------------------
# Filename helpers — encode key params in output names for reproducibility
# ---------------------------------------------------------------------------

#' Canonical run identifier string shared by Stage 1 and Stage 2 filenames.
make_run_id <- function(opt) {
  str_glue(
    "{opt$who_region}_{opt$country_iso3}",
    "_TL{str_remove_all(opt$time_lower_bound, '-')}",
    "_TR{str_remove_all(opt$time_upper_bound, '-')}",
    "_thresh-{str_replace_all(opt$threshold_type, ' ', '_')}",
    "_start-{opt$outbreak_start_definition}"
  )
}

#' Stage 1 geo file — raw sf object pulled from API (retains geometry).
#' Extension is .parquet (GeoParquet) when use_geoparquet is TRUE, else .geojson.
make_stage1_geo_filename <- function(opt) {
  ext <- if (isTRUE(opt$use_geoparquet)) ".parquet" else ".geojson"
  file.path(here(opt$output_dir), paste0("stage1_geo_", make_run_id(opt), ext))
}

#' Stage 1 flat file — normalized tabular data (geometry dropped).
#' Extension is .parquet when use_geoparquet is TRUE, else .rds.
#' This is the input to Stage 2 outbreak detection.
make_stage1_flat_filename <- function(opt) {
  ext <- if (isTRUE(opt$use_geoparquet)) ".parquet" else ".rds"
  file.path(here(opt$output_dir), paste0("stage1_flat_", make_run_id(opt), ext))
}

#' Stage 2 file — outbreak detection results for one country (all windows).
#' Extension is .parquet when use_geoparquet is TRUE, else .rds.
make_stage2_filename <- function(who_region, country_iso3, use_geoparquet = FALSE) {
  ext <- if (isTRUE(use_geoparquet)) ".parquet" else ".rds"
  file.path(
    here("analysis/generated_data"),
    paste0("stage2_", who_region, "_", country_iso3, ext)
  )
}

# ---------------------------------------------------------------------------
# Format-aware I/O helpers
# ---------------------------------------------------------------------------

#' Write a flat data frame to .rds (default) or .parquet (use_geoparquet = TRUE).
write_tabular <- function(df, path, use_geoparquet = FALSE) {
  if (isTRUE(use_geoparquet)) arrow::write_parquet(df, path)
  else saveRDS(df, path)
}

#' Read a flat data frame from .rds (default) or .parquet (use_geoparquet = TRUE).
read_tabular <- function(path, use_geoparquet = FALSE) {
  if (isTRUE(use_geoparquet)) arrow::read_parquet(path)
  else readRDS(path)
}

#' Write a spatial sf object to .geojson (default) or .parquet (use_geoparquet = TRUE).
#' delete_dsn = TRUE is required: sf::st_write refuses to overwrite by default.
write_spatial <- function(sf_obj, path, use_geoparquet = FALSE) {
  if (isTRUE(use_geoparquet)) sfarrow::st_write_parquet(sf_obj, path)
  else sf::st_write(sf_obj, path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
}

# ---------------------------------------------------------------------------
# Logging helper
# ---------------------------------------------------------------------------

print_options <- function(opt) {
  cat("---------- Run config ----------\n")
  str(opt)
  cat("--------------------------------\n")
}


# Taxdat patch ------------------------------------------------------------

#' @title Rename cholera data columns
#' @description Renames the columns of the data pulled either from the the 
#' API staging database or by SQL from taxdat 
#'
#' @param database_df Data who's columns are to be modified
#' @param source Whether the source is the staging database (sing the API) or taxdat (using SQL).
#' @details source is one of 'api' or 'sql'
#' @return the renamed dataframe
rename_database_fields <- function(database_df,
                                   source = "api") {
  
  if (source == "api") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = attributes.time_left,
        TR = attributes.time_right,
        is_primary = attributes.primary,
        is_phantom = attributes.phantom,
        locationPeriod_id = attributes.id,
        OC_UID = relationships.observation_collection.data.id,
        location_name = attributes.location_name
      )
  } else if (source == "sql") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = time_left,
        TR = time_right,
        is_primary = primary,
        is_phantom = phantom,
        locationPeriod_id = location_period_id,
        OC_UID = observation_collection_id,
        location_name = location_name
      )
  } else {
    stop("Source needs to be one of 'api', 'sql', found ", source)
  }
  # names(new_database_df) <- gsub("attributes.fields.", "", names(new_database_df))
  # names(new_database_df) <- gsub("attributes.", "", names(new_database_df))
  return(new_database_df)
}
