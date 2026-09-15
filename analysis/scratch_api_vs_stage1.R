# scratch_api_vs_stage1.R — Compare raw API cache to Stage 1 flat output
#
# For every country × time window that has a raw_api_cache_*.rds file, this
# script counts rows at each filtering stage and compares to the corresponding
# stage1_flat_*.rds to identify where data is being lost (or is genuinely absent).
#
# Filtering stages tracked (in pipeline order):
#   1. n_api_raw         — total rows returned by the API (before any processing)
#   2. n_api_primary     — rows where attributes.primary == TRUE
#   3. n_stage1_all      — all rows in Stage 1 flat (including phantom zeroes)
#   4. n_stage1_obs      — non-phantom rows in Stage 1 flat (real observations)
#   5. n_stage1_sCh      — non-phantom rows with sCh > 0
#
# Between stage 2 and 3 the pipeline also applies observation_filter() (temporal
# scale, spatial scale, NA-sCh removal, minimum daily cases) and aggregation,
# so n_stage1_obs ≤ n_api_primary is expected. The script flags windows where
# n_api_primary > 0 but n_stage1_obs == 0 as "potential data loss" for review.
#
# Output:
#   analysis/generated_data/api_vs_stage1.csv  — full row-by-row table
#   console summary table                      — printed at end

library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)

stage1_dir <- here("analysis/generated_data")

# ---------------------------------------------------------------------------
# 1. Discover raw API cache files and parse metadata
# ---------------------------------------------------------------------------

cache_files <- list.files(stage1_dir,
                           pattern = "^raw_api_cache_.*\\.rds$",
                           full.names = TRUE)

m <- str_match(basename(cache_files),
               "^raw_api_cache_([A-Z]+)_([^_]+)_TL(\\d{8})_TR(\\d{8})")

cache_meta <- tibble(
  cache_path = cache_files,
  run_id     = str_remove(basename(cache_files), "^raw_api_cache_") |>
                 str_remove("\\.rds$"),
  who_region = m[, 2],
  iso3       = m[, 3],
  tl_str     = m[, 4],
  tr_str     = m[, 5]
) |>
  filter(!is.na(who_region), !is.na(iso3)) |>
  mutate(
    TL = ymd(tl_str),
    TR = ymd(tr_str),
    flat_path = file.path(stage1_dir,
                          paste0("stage1_flat_", run_id, ".rds"))
  )

message("Found ", nrow(cache_meta), " API cache files across ",
        n_distinct(cache_meta$iso3), " countries")
message("  Stage 1 flat file present: ",
        sum(file.exists(cache_meta$flat_path)), " / ", nrow(cache_meta))

# ---------------------------------------------------------------------------
# 2. Per-window comparison function
# ---------------------------------------------------------------------------

compare_window <- function(cache_path, flat_path) {

  # --- Raw API cache ---
  api <- tryCatch(readRDS(cache_path), error = function(e) NULL)

  if (is.null(api) || nrow(api) == 0) {
    return(tibble(
      n_api_raw     = 0L,
      n_api_primary = 0L,
      flat_exists   = file.exists(flat_path),
      n_stage1_all  = NA_integer_,
      n_stage1_obs  = NA_integer_,
      n_stage1_sCh  = NA_integer_
    ))
  }

  n_api_raw <- nrow(api)

  # "primary" lives as attributes.primary in raw API sf objects
  primary_col <- intersect(c("primary", "attributes.primary"), names(api))
  if (length(primary_col) == 0) {
    # Fallback: assume all primary if column absent
    n_api_primary <- n_api_raw
  } else {
    n_api_primary <- sum(api[[primary_col[1]]] == TRUE, na.rm = TRUE)
  }

  # --- Stage 1 flat file ---
  if (!file.exists(flat_path)) {
    return(tibble(
      n_api_raw     = n_api_raw,
      n_api_primary = n_api_primary,
      flat_exists   = FALSE,
      n_stage1_all  = NA_integer_,
      n_stage1_obs  = NA_integer_,
      n_stage1_sCh  = NA_integer_
    ))
  }

  flat <- tryCatch(readRDS(flat_path), error = function(e) NULL)

  if (is.null(flat) || nrow(flat) == 0) {
    return(tibble(
      n_api_raw     = n_api_raw,
      n_api_primary = n_api_primary,
      flat_exists   = TRUE,
      n_stage1_all  = 0L,
      n_stage1_obs  = 0L,
      n_stage1_sCh  = 0L
    ))
  }

  obs <- filter(flat, !phantom)
  tibble(
    n_api_raw     = n_api_raw,
    n_api_primary = n_api_primary,
    flat_exists   = TRUE,
    n_stage1_all  = nrow(flat),
    n_stage1_obs  = nrow(obs),
    n_stage1_sCh  = sum(obs$sCh > 0, na.rm = TRUE)
  )
}

# ---------------------------------------------------------------------------
# 3. Run comparison (with progress bar)
# ---------------------------------------------------------------------------

message("Comparing API cache vs Stage 1 flat files …")

results <- map2(
  cache_meta$cache_path,
  cache_meta$flat_path,
  compare_window,
  .progress = TRUE
)

comparison <- bind_cols(cache_meta, list_rbind(results)) |>
  mutate(
    # Classify each window
    status = case_when(
      n_api_raw == 0                             ~ "api_empty",
      n_api_primary == 0                         ~ "api_nonprimary_only",
      !flat_exists                               ~ "stage1_missing",
      n_stage1_obs == 0 & n_api_primary > 0      ~ "data_lost",
      n_stage1_sCh == 0 & n_stage1_obs > 0       ~ "zero_sCh",
      TRUE                                       ~ "ok"
    ),
    # Retention rate: what fraction of primary API rows appear as Stage 1 obs
    # (can be > 1 after zero-filling / aggregation, but usually ≤ 1)
    retention = if_else(n_api_primary > 0,
                        n_stage1_obs / n_api_primary,
                        NA_real_)
  )

# ---------------------------------------------------------------------------
# 4. Save full table
# ---------------------------------------------------------------------------

out_csv <- here("analysis/generated_data/api_vs_stage1.csv")
readr::write_csv(comparison |> select(-cache_path, -flat_path), out_csv)
message("Saved: ", basename(out_csv))

# ---------------------------------------------------------------------------
# 5. Console summary
# ---------------------------------------------------------------------------

cat("\n=== API cache vs Stage 1 — window status counts ===\n")
comparison |>
  count(status, sort = TRUE) |>
  print()

cat("\n=== 'data_lost' windows: primary API rows > 0 but Stage 1 obs == 0 ===\n")
lost <- comparison |>
  filter(status == "data_lost") |>
  select(who_region, iso3, TL, TR, n_api_raw, n_api_primary,
         n_stage1_all, n_stage1_obs)

if (nrow(lost) == 0) {
  cat("None — no data loss detected.\n")
} else {
  cat(nrow(lost), "windows affected across",
      n_distinct(lost$iso3), "countries\n\n")
  print(lost, n = 50)
}

cat("\n=== 'stage1_missing' windows: cache exists but no Stage 1 file ===\n")
missing_s1 <- comparison |>
  filter(status == "stage1_missing") |>
  select(who_region, iso3, TL, TR, n_api_raw, n_api_primary)

if (nrow(missing_s1) == 0) {
  cat("None.\n")
} else {
  cat(nrow(missing_s1), "windows missing Stage 1 output\n\n")
  missing_s1 |>
    count(who_region, iso3, name = "n_missing") |>
    arrange(desc(n_missing)) |>
    print(n = 30)
}

cat("\n=== Countries with ≥1 'data_lost' window ===\n")
comparison |>
  filter(status == "data_lost") |>
  count(who_region, iso3, name = "n_lost_windows") |>
  arrange(desc(n_lost_windows)) |>
  print(n = 30)

cat("\n=== Retention rate summary (n_stage1_obs / n_api_primary) ===\n")
comparison |>
  filter(status == "ok") |>
  summarise(
    median_retention = median(retention, na.rm = TRUE),
    p05_retention    = quantile(retention, 0.05, na.rm = TRUE),
    p95_retention    = quantile(retention, 0.95, na.rm = TRUE),
    n_windows        = n()
  ) |>
  print()
