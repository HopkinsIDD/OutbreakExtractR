#!/usr/bin/env Rscript
# parse_detect_logs.R
# Parses all Stage 2 (detection) SLURM logs, classifies outcomes, and writes
# summary CSVs.
#
# Supports two log formats:
#   OLD (batch ≤ 44825740): per-window Processing: lines with Rows: summaries
#   NEW (batch ≥ 44869016): single-pass pipeline with Loaded N observations …
#
# Usage:
#   Rscript analysis/parse_detect_logs.R [--log-dir analysis/logs] \
#                                        [--out analysis/detect_log_summary.csv] \
#                                        [--latest-only]

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tidyr)
})

# ── CLI args ──────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default) {
  idx <- which(args == flag)
  if (length(idx) && length(args) >= idx + 1) args[[idx + 1]] else default
}

log_dir     <- get_arg("--log-dir",  "analysis/logs")
out_csv     <- get_arg("--out",      "analysis/detect_log_summary.csv")
latest_only <- any(args == "--latest-only")

# ── Fatal-error classification (new format) ───────────────────────────────────
# Applied to the tail of logs whose outcome is "error".
fatal_error_classes <- list(
  exactextractr_error = "exactextractr|CPP_stats",
  purrr_map_error     = "Error in `purrr::map",
  r_error_generic     = "^Error in |^Error:"
)

classify_fatal_error <- function(tail_lines) {
  combined <- paste(tail_lines, collapse = "\n")
  for (cls in names(fatal_error_classes)) {
    if (grepl(fatal_error_classes[[cls]], combined, perl = TRUE)) return(cls)
  }
  "unknown_error"
}

# ── Per-window warning classification (old format) ────────────────────────────
window_error_classes <- list(
  missing_value_logical  = "missing value where TRUE/FALSE needed",
  too_long_vector        = "result would be too long a vector",
  subscript_out_of_range = "subscript out of bounds|attempt to select less than one",
  no_non_missing_min     = "no non-missing arguments to min",
  trigger_alert_failed   = "trigger_alert\\(\\) failed",
  identify_failed_other  = "identify_outbreaks\\(\\) failed"
)

classify_window_error <- function(msg) {
  for (cls in names(window_error_classes)) {
    if (grepl(window_error_classes[[cls]], msg, perl = TRUE)) return(cls)
  }
  "unknown_window_error"
}

# ── Per-log parser ─────────────────────────────────────────────────────────────
parse_log <- function(path) {

  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))

  empty_row <- tibble(
    log_file           = basename(path),
    slurm_job_id       = NA_character_,
    array_task_id      = NA_integer_,
    config             = NA_character_,
    who_region         = NA_character_,
    country_iso3       = NA_character_,
    outcome            = "unreadable",
    error_class        = NA_character_,
    # --- new-format fields ---
    n_obs_loaded       = NA_integer_,
    total_rows         = NA_integer_,
    outbreak_rows      = NA_integer_,
    no_results_reason  = NA_character_,
    # --- old-format per-window fields (NA for new-format logs) ---
    n_stage1_files     = NA_integer_,
    n_windows_empty    = NA_integer_,
    n_windows_no_ob    = NA_integer_,
    n_windows_ran      = NA_integer_,
    n_windows_failed   = NA_integer_,
    window_error_types = NA_character_,
    failed_run_ids     = NA_character_
  )

  if (length(lines) == 0) return(empty_row)

  # ── Helper ───────────────────────────────────────────────────────────────────
  grab <- function(pattern) {
    m <- str_match(lines, pattern)
    hit <- m[!is.na(m[, 2]), 2, drop = TRUE]
    if (length(hit)) hit[[1]] else NA_character_
  }

  # ── Header fields ────────────────────────────────────────────────────────────
  slurm_job_id  <- str_match(basename(path), "^cholera_detect_(\\d+)_")[, 2]
  array_task_id <- as.integer(grab("SLURM_ARRAY_TASK_ID:\\s*(\\d+)"))
  config        <- grab("^Config:\\s*(.+)")
  who_region    <- grab("\\$\\s*who_region\\s*:\\s*chr\\s*\"([^\"]+)\"")
  country_iso3  <- grab("\\$\\s*country_iso3\\s*:\\s*chr\\s*\"([^\"]+)\"")

  # ── Detect log format ────────────────────────────────────────────────────────
  # New format loads sf (→ "Linking to GEOS" appears even for early-exit logs).
  # Old format never loads sf.
  new_format <- any(grepl("Linking to GEOS",                lines, fixed = TRUE)) ||
                any(grepl("Loaded \\d+ cleaned observations", lines))

  # ── New-format fields ────────────────────────────────────────────────────────
  n_obs_loaded <- as.integer(grab("Loaded (\\d+) cleaned observations"))

  total_rows <- {
    m <- str_match(lines, "Total rows:\\s*(\\d+)")
    hit <- m[!is.na(m[, 2]), 2]
    if (length(hit)) as.integer(hit[[1]]) else NA_integer_
  }
  outbreak_rows <- {
    m <- str_match(lines, "Outbreak-period rows:\\s*(\\d+)")
    hit <- m[!is.na(m[, 2]), 2]
    if (length(hit)) as.integer(hit[[1]]) else NA_integer_
  }

  # ── Old-format per-window fields ─────────────────────────────────────────────
  n_stage1_files  <- as.integer(grab("Found (\\d+) Stage 1 file"))
  n_windows_empty <- if (!new_format) sum(grepl("Empty Stage 1 file",     lines, fixed = TRUE)) else NA_integer_
  n_windows_no_ob <- if (!new_format) sum(grepl("No outbreaks detected",   lines, fixed = TRUE)) else NA_integer_
  n_windows_ran   <- if (!new_format) sum(grepl("Rows:\\s*\\d+\\s*\\|\\s*Outbreak-period rows", lines)) else NA_integer_

  warn_lines       <- lines[grepl("(identify_outbreaks|trigger_alert)\\(\\) failed for", lines)]
  n_windows_failed <- if (!new_format) length(warn_lines) else NA_integer_

  window_error_types <- if (!new_format && length(warn_lines) > 0) {
    warn_lines |> map_chr(classify_window_error) |> unique() |> sort() |> paste(collapse = ";")
  } else {
    NA_character_
  }

  failed_run_ids <- if (!new_format && length(warn_lines) > 0) {
    m <- str_match(warn_lines,
                   "(?:identify_outbreaks|trigger_alert)\\(\\) failed for ([^:]+):")
    m[!is.na(m[, 2]), 2] |> paste(collapse = ";")
  } else {
    NA_character_
  }

  # ── Outcome sentinels ─────────────────────────────────────────────────────────
  halted     <- any(grepl("Execution halted",              lines, fixed = TRUE))
  error_line <- any(grepl("^ERROR:",                       lines))
  skipped    <- any(grepl("Stage 2 output already exists", lines, fixed = TRUE))
  stage2_ok  <- any(grepl("Stage 2 complete.",             lines, fixed = TRUE))
  batch_end  <- any(grepl("Batch 2 end:",                  lines, fixed = TRUE))
  completed  <- stage2_ok || batch_end

  # "no results" covers both old and new warning messages
  no_results <- any(grepl("No outbreak results to save",         lines, fixed = TRUE)) ||
                any(grepl("No outbreaks detected for:",           lines, fixed = TRUE)) ||
                any(grepl("No Stage 1 observations for:",         lines, fixed = TRUE)) ||
                any(grepl("No observations after filtering for:", lines, fixed = TRUE)) ||
                any(grepl("No data after normalization for:",     lines, fixed = TRUE))

  # Extract the specific reason (first match wins; ordered most-specific first)
  no_results_reason <- NA_character_
  if (no_results) {
    for (pat in c(
      "No Stage 1 observations for:",
      "No observations after filtering for:",
      "No data after normalization for:",
      "No outbreaks detected for:",
      "No outbreak results to save"
    )) {
      hit <- lines[grepl(pat, lines, fixed = TRUE)]
      if (length(hit)) { no_results_reason <- str_trim(hit[[1]]); break }
    }
  }

  # ── Outcome ───────────────────────────────────────────────────────────────────
  n_failed <- if (!is.na(n_windows_failed)) n_windows_failed else 0L

  outcome <- case_when(
    halted | error_line                             ~ "error",
    skipped & !completed                            ~ "skipped",
    completed & no_results & n_failed > 0           ~ "success_no_results_with_warnings",
    completed & no_results                          ~ "success_no_results",
    completed & n_failed > 0                        ~ "success_with_warnings",
    completed                                       ~ "success",
    TRUE                                            ~ "incomplete"
  )

  # ── Fatal-error classification ────────────────────────────────────────────────
  error_class <- if (outcome == "error") {
    classify_fatal_error(tail(lines, 80))
  } else {
    NA_character_
  }

  tibble(
    log_file           = basename(path),
    slurm_job_id       = slurm_job_id,
    array_task_id      = array_task_id,
    config             = config,
    who_region         = who_region,
    country_iso3       = country_iso3,
    outcome            = outcome,
    error_class        = error_class,
    n_obs_loaded       = n_obs_loaded,
    total_rows         = total_rows,
    outbreak_rows      = outbreak_rows,
    no_results_reason  = no_results_reason,
    n_stage1_files     = n_stage1_files,
    n_windows_empty    = n_windows_empty,
    n_windows_no_ob    = n_windows_no_ob,
    n_windows_ran      = n_windows_ran,
    n_windows_failed   = n_windows_failed,
    window_error_types = window_error_types,
    failed_run_ids     = failed_run_ids
  )
}

# ── Main ───────────────────────────────────────────────────────────────────────
log_files <- list.files(log_dir,
                        pattern = "^cholera_detect.*\\.log$",
                        full.names = TRUE)

if (length(log_files) == 0) stop("No detection .log files found in: ", log_dir)

if (latest_only) {
  batch_ids <- str_match(basename(log_files), "^cholera_detect_(\\d+)_")[, 2]
  latest_id <- max(batch_ids, na.rm = TRUE)
  log_files  <- log_files[!is.na(batch_ids) & batch_ids == latest_id]
  message(sprintf("--latest-only: restricting to batch %s (%d files)",
                  latest_id, length(log_files)))
}

message(sprintf("Parsing %d detection log files from '%s' ...",
                length(log_files), log_dir))

results <- map(log_files, parse_log, .progress = TRUE) |> list_rbind()

# ── Console summary ────────────────────────────────────────────────────────────
batches <- sort(unique(results$slurm_job_id))
cat(sprintf("\n%d batch(es) found: %s\n", length(batches), paste(batches, collapse = ", ")))

print_batch_summary <- function(df, label) {
  cat(sprintf("\n========== %s ==========\n", label))

  cat("--- Outcome ---\n")
  df |>
    count(outcome, sort = TRUE) |>
    mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
    print(n = Inf)

  # Error details
  err_df <- df |> filter(outcome == "error")
  if (nrow(err_df) > 0) {
    cat(sprintf("--- Error class (%d job(s)) ---\n", nrow(err_df)))
    err_df |>
      count(error_class, sort = TRUE) |>
      mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
      print(n = Inf)
    cat("--- Errored jobs ---\n")
    err_df |>
      select(log_file, array_task_id, who_region, country_iso3, error_class) |>
      print(n = Inf)
  }

  # No-results breakdown
  nr_df <- df |> filter(str_starts(outcome, "success_no_results"))
  if (nrow(nr_df) > 0) {
    cat(sprintf("--- No-results reasons (%d country/ies) ---\n", nrow(nr_df)))
    nr_df |>
      # trim to just the prefix before ":" to group by type
      mutate(reason_type = str_extract(no_results_reason,
                                       "^No [^:]+")) |>
      count(reason_type, sort = TRUE) |>
      print(n = Inf)
  }

  # Row counts (new format)
  new_fmt <- df |> filter(!is.na(n_obs_loaded))
  if (nrow(new_fmt) > 0) {
    cat("--- Output rows (new-format jobs) ---\n")
    new_fmt |>
      summarise(
        countries_ok        = sum(outcome == "success"),
        total_obs_loaded    = sum(n_obs_loaded,    na.rm = TRUE),
        total_output_rows   = sum(total_rows,      na.rm = TRUE),
        total_outbreak_rows = sum(outbreak_rows,   na.rm = TRUE)
      ) |> print()
  }

  # Window counts (old format)
  old_fmt <- df |> filter(!is.na(n_windows_ran))
  if (nrow(old_fmt) > 0) {
    cat("--- Window counts (old-format jobs) ---\n")
    old_fmt |>
      summarise(
        countries_run      = sum(outcome %in% c("success", "success_with_warnings",
                                                "success_no_results",
                                                "success_no_results_with_warnings")),
        total_stage1_files = sum(n_stage1_files,   na.rm = TRUE),
        windows_empty      = sum(n_windows_empty,  na.rm = TRUE),
        windows_no_ob      = sum(n_windows_no_ob,  na.rm = TRUE),
        windows_ran        = sum(n_windows_ran,     na.rm = TRUE),
        windows_failed     = sum(n_windows_failed,  na.rm = TRUE),
        total_output_rows  = sum(total_rows,        na.rm = TRUE)
      ) |> print()

    failed_df <- old_fmt |> filter(!is.na(n_windows_failed) & n_windows_failed > 0)
    if (nrow(failed_df) > 0) {
      cat(sprintf("--- Per-window error types (%d countries affected) ---\n",
                  nrow(failed_df)))
      failed_df |>
        mutate(error_type = str_split(window_error_types, ";")) |>
        unnest(error_type) |>
        count(error_type, sort = TRUE) |>
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
        print(n = Inf)

      cat("--- Countries with per-window failures ---\n")
      failed_df |>
        select(who_region, country_iso3, n_windows_failed, window_error_types,
               n_windows_ran, total_rows) |>
        arrange(desc(n_windows_failed)) |>
        print(n = 40)
    }
  }

  incomplete <- df |> filter(outcome == "incomplete")
  if (nrow(incomplete) > 0) {
    cat(sprintf("--- Incomplete jobs (%d) ---\n", nrow(incomplete)))
    incomplete |>
      select(log_file, array_task_id, who_region, country_iso3) |>
      print(n = Inf)
  }
}

# Per-batch summaries
for (b in batches) {
  print_batch_summary(results |> filter(slurm_job_id == b),
                      paste("Batch", b))
}

# Combined summary across all batches (latest per country if duplicated)
cat("\n========== Combined (latest batch per country) ==========\n")
latest <- results |>
  arrange(desc(slurm_job_id)) |>
  distinct(who_region, country_iso3, .keep_all = TRUE)
print_batch_summary(latest, "Latest run per country")

# ── Write CSV ─────────────────────────────────────────────────────────────────
write_csv(results, out_csv)
message(sprintf("\nSummary written to: %s", out_csv))
