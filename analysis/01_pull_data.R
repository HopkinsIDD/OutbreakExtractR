# 01_pull_data.R  —  Batch 1: pull + normalize data for one country × time window
#
# Reads a YAML config, calls the Cholera Taxonomy API via taxdat, runs the
# full OutbreakExtractR normalization pipeline, and writes two GeoParquet files:
#   stage1_geo_{run_id}.{geojson,parquet}  — sf object (retains geometry, for spatial use)
#   stage1_flat_{run_id}.{rds,parquet}     — cleaned observations, geometry dropped (input to Batch 2)
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

# Patch taxdat::read_taxonomy_data_api to guard against shape IDs that are
# absent from the API response's `included` list.  match() returns NA/NULL when
# the shape isn't found, and the subsequent [[NA]] index crashes with
# "attempt to select less than one element in get1index" before the existing
# is.null(unformatted_geojson) guard can fire.  The fix skips to the empty-
# point fallback whenever this_shape_index is missing, mirroring what the
# original null-check was already trying to do.
utils::assignInNamespace(
  "read_taxonomy_data_api",
  function(username, api_key, locations = NULL, time_left = NULL,
           time_right = NULL, uids = NULL,
           website = "https://cholera-taxonomy.middle-distance.com/") {
    api_type <- ""
    if (is.null(uids)) {
      api_type <- "by_location"
      if (length(locations == 1)) {
        locations <- c(locations, locations)
      }
      if (any(!grepl("::", locations))) {
        stop("Trying to pull data for a continent is not allowed")
      }
      if ((sum(stringr::str_count(string = unique(locations), pattern = "::") == 1) > 2)) {
        stop("Trying to pull data for more than 2 countries at a time is not allowed")
      }
      https_post_argument_list <- list(
        email    = username, api_key = api_key,
        locations = gsub("::", " ", locations),
        time_left = time_left, time_right = time_right
      )
    } else if (is.null(locations) && is.null(time_left) && is.null(time_right)) {
      api_type <- "by_observation_collections"
      https_post_argument_list <- list(
        email = username, api_key = api_key,
        observation_collection_ids = uids
      )
    } else {
      stop("Not supported")
    }
    website <- paste0(website, "/api/v1/observations/", api_type)
    json    <- jsonlite::toJSON(https_post_argument_list, auto_unbox = T)
    message("Fetching results from JSON API")
    results <- httr::POST(website,
                          httr::add_headers(`Content-Type` = "application/json"),
                          body = json, encode = "form")
    code <- httr::status_code(results)
    if (code != 200) stop(paste("Error: Status Code", code))

    original_results_data <- httr::content(results)
    jsondata <- rjson::toJSON(original_results_data)
    if (!jsonlite::validate(jsondata)) stop("Could not validate json response")
    results_data <- jsonlite::fromJSON(jsondata)

    if ((!("observations" %in% names(results_data))) |
        (!("data" %in% names(results_data[["observations"]]))) |
        (length(results_data[["observations"]]) > 1)) {
      stop("Could not parse results properly.  Contact package maintainer")
    }
    results_data[["observations"]] <- taxdat:::flatten_json_result(results_data[["observations"]][["data"]])

    observation_collections_present <- FALSE
    if (("observation_collections" %in% names(results_data)) &&
        ("data" %in% names(results_data[["observation_collections"]])) &&
        (length(results_data[["observation_collections"]]) == 1)) {
      results_data[["observation_collections"]] <- taxdat:::flatten_json_result(
        results_data[["observation_collections"]][["data"]]
      )
      observation_collections_present <- TRUE
    }

    if (!length(unique(results_data$observations$id)) == nrow(results_data$observations)) {
      stop("Could not parse results properly.  Contact package maintainer")
    }

    tmp_results   <- original_results_data[["location_periods"]][["data"]]
    all_shape_ids <- sapply(original_results_data$location_periods$included, function(x) x$id)
    all_locations <- list()

    if (length(tmp_results) > 0) {
      for (idx in 1:length(tmp_results)) {
        message(paste(idx, "/", length(tmp_results)))
        shape_id         <- tmp_results[[idx]][["relationships"]][["shape"]][["data"]][["id"]]
        this_shape_index <- match(shape_id, all_shape_ids)
        # PATCH: guard — match() returns NA/NULL when shape_id is absent from
        # `included`; [[NA]] crashes before the is.null check below can fire.
        if (is.null(this_shape_index) || length(this_shape_index) == 0 || is.na(this_shape_index)) {
          message("  [read_taxonomy_data_api patch] shape ID not found in included, skipping geometry: ", shape_id)
          all_locations[[idx]] <- sf::st_sf(geometry = sf::st_sfc(sf::st_point()))
          next
        }
        unformatted_geojson <- original_results_data[["location_periods"]][["included"]][[this_shape_index]][["attributes"]][["simple_shape"]]
        if (is.null(unformatted_geojson)) {
          all_locations[[idx]] <- sf::st_sf(geometry = sf::st_sfc(sf::st_point()))
          next
        }
        sf_geojson           <- geojsonsf::geojson_sf(unformatted_geojson)
        all_locations[[idx]] <- sf_geojson
      }
    }

    locations_sf <- taxdat::reduce_sf_vector(all_locations)
    results_data$location_periods$data$geojson            <- NULL
    results_data$location_periods$data$attributes$geojson <- NULL
    results_data$location_periods <- taxdat:::flatten_json_result(results_data$location_periods$data)
    if (nrow(results_data$location_periods) > 0) {
      results_data$location_periods$sf_id <- seq_len(nrow(results_data$location_periods))
    }

    results_data$observations$attributes.location_period_id <- as(
      results_data$observations$attributes.location_period_id,
      class(results_data$location_periods$id)
    )
    all_results <- results_data$observations
    if (observation_collections_present &&
        (nrow(all_results) > 0) &&
        (nrow(results_data$observation_collections) > 0)) {
      all_results <- dplyr::left_join(
        results_data$observations, results_data$observation_collections,
        by = c(relationships.observation_collection.data.id = "id")
      )
    }
    if ((nrow(all_results) > 0) && (nrow(results_data$location_periods) > 0)) {
      all_results <- dplyr::left_join(
        all_results, results_data$location_periods,
        by = c(attributes.location_period_id = "id")
      )
    }

    geoinput <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(1 * c(NA, NA))))$geometry
    if (nrow(all_results) == 0) geoinput <- geoinput[0]
    all_results$geojson <- geoinput
    all_results$geojson[!is.na(all_results$sf_id)] <-
      locations_sf$geometry[all_results[!is.na(all_results$sf_id), ][["sf_id"]]]

    return(sf::st_sf(all_results, sf_column_name = "geojson"))
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

# Guard: empty API response means no observations for this location/window.
# Must happen before select/rename — an empty sf has only a geometry column,
# which would cause rename(TL = attributes.time_left) to crash.
if (is.null(raw_api) || nrow(raw_api) == 0) {
  warning("API returned no data for: ", location_str,
          "  [", opt$time_lower_bound, " → ", opt$time_upper_bound, "]")
  write_tabular(data.frame(), out_flat, opt$use_geoparquet)
  quit(status = 0)
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
    TL      = attributes.time_left,
    TR      = attributes.time_right,
    primary = attributes.primary,
    location = attributes.location_name
  )

# Optional columns: rename if present, else add as NA.
# Mirrors the confirmed_cases guard below — some API responses omit these fields.
optional_col_map <- list(
  observation_collection_id = "relationships.observation_collection.data.id",
  sCh                       = "attributes.fields.suspected_cases",
  cCh                       = "attributes.fields.confirmed_cases",
  deaths                    = "attributes.fields.deaths",
  location_period_id        = "attributes.location_period_id"
)
for (new_name in names(optional_col_map)) {
  old_name <- optional_col_map[[new_name]]
  if (old_name %in% names(raw_sf)) {
    raw_sf <- dplyr::rename(raw_sf, !!new_name := !!old_name)
  } else {
    raw_sf[[new_name]] <- NA
  }
}

message("Pulled ", nrow(raw_sf), " raw observations.")

# Save raw sf with geometry as GeoParquet (useful for spatial visualisation)
write_spatial(raw_sf, out_geo, opt$use_geoparquet)
message("Saved raw geo file: ", basename(out_geo))

# ---------------------------------------------------------------------------
# Stage 1b: clean raw observations and save per-window flat file
#
# Filtering, aggregation, normalization, and population attachment all happen
# in Batch 2 (02_run_outbreak_detection.R) once the full per-country series
# has been assembled from all windows, matching the reference pipeline.
# ---------------------------------------------------------------------------

# Drop geometry — OutbreakExtractR functions operate on flat dataframes
raw_df <- sf::st_drop_geometry(raw_sf)

# Clean: standardize types, identify spatial/temporal scale, clean location names
clean_data <- OutbreakExtractR::clean_psql_data(raw_df)

# ---------------------------------------------------------------------------
# Save cleaned flat file for Batch 2
# ---------------------------------------------------------------------------

write_tabular(clean_data, out_flat, opt$use_geoparquet)

message("Stage 1 complete.")
message("  Rows:      ", nrow(clean_data))
message("  Locations: ", length(unique(clean_data$location)))
message("  Saved:     ", basename(out_flat))
