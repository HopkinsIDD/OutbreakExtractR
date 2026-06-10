#!/usr/bin/env Rscript
# parse_detect_logs.R
# Parses all Stage 2 (detection) SLURM logs, classifies outcomes, and writes
# summary CSVs parallel to parse_pull_logs.R.
#
# Key differences from pull logs:
#   - One log = one country (not one country × window)
#   - Outcomes are at country level; per-window failures surface as warnings
#   - Extracts Stage 2 row counts and window tallies from log body
#
# Usage:
#   Rscript analysis/parse_detect_logs.R [--log-dir logs] \
#                                        [--out analysis/detect_log_summary.csv]

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

log_dir <- get_arg("--log-dir", "logs")
out_csv <- get_arg("--out",     "analysis/detect_log_summary.csv")

# ── Per-window warning error classification ───────────────────────────────────
# Matches the warning body emitted by identify_outbreaks() / trigger_alert()
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
    log_file          = basename(path),
    array_task_id     = NA_integer_,
    config            = NA_character_,
    who_region        = NA_character_,
    country_iso3      = NA_character_,
    outcome           = "unreadable",
    n_stage1_files    = NA_integer_,
    n_windows_empty   = NA_integer_,
    n_windows_no_ob   = NA_integer_,
    n_windows_ob      = NA_integer_,
    n_windows_failed  = NA_integer_,
    total_rows        = NA_integer_,
    window_error_types = NA_character_
  )

  if (length(lines) == 0) return(empty_row)

  # ── Header fields ────────────────────────────────────────────────────────────
  grab <- function(pattern) {
    m <- str_match(lines, pattern)
    hit <- m[!is.na(m[, 2]), 2, drop = TRUE]
    if (length(hit)) hit[[1]] else NA_character_
  }

  array_task_id <- as.integer(grab("SLURM_ARRAY_TASK_ID:\\s*(\\d+)"))
  config        <- grab("^Config:\\s*(.+)")
  who_region    <- grab("\\$\\s*who_region\\s*:\\s*chr\\s*\"([^\"]+)\"")
  country_iso3  <- grab("\\$\\s*country_iso3\\s*:\\s*chr\\s*\"([^\"]+)\"")

  # ── Window-level counts ──────────────────────────────────────────────────────
  n_stage1_files   <- as.integer(grab("Found (\\d+) Stage 1 file"))
  n_windows_empty  <- sum(grepl("Empty Stage 1 file", lines, fixed = TRUE))
  n_windows_no_ob  <- sum(grepl("No outbreaks detected", lines, fixed = TRUE))
  n_windows_ob     <- sum(grepl("Rows:\\s*\\d+\\s*\\|\\s*Outbreak-period rows", lines))
  total_rows       <- {
    m <- str_match(lines, "Total rows:\\s*(\\d+)")
    hit <- m[!is.na(m[, 2]), 2]
    if (length(hit)) as.integer(hit[[1]]) else NA_integer_
  }

  # ── Per-window failure warnings ──────────────────────────────────────────────
  # Warning lines look like:
  #   identify_outbreaks() failed for <run_id>: <message>
  #   trigger_alert() failed for <run_id>: <message>
  warn_lines <- lines[grepl("(identify_outbreaks|trigger_alert)\\(\\) failed for", lines)]
  n_windows_failed <- length(warn_lines)

  window_error_types <- if (n_windows_failed > 0) {
    warn_lines |>
      map_chr(classify_window_error) |>
      unique() |>
      sort() |>
      paste(collapse = ";")
  } else {
    NA_character_
  }

  # ── Outcome ───────────────────────────────────────────────────────────────────
  completed  <- any(grepl("Batch 2 end:", lines, fixed = TRUE))
  no_results <- any(grepl("No outbreak results to save", lines, fixed = TRUE))
  skipped    <- any(grepl("Stage 2 output already exists, skipping", lines, fixed = TRUE))
  halted     <- any(grepl("Execution halted", lines, fixed = TRUE))
  error_line <- any(grepl("^ERROR:", lines))

  outcome <- case_when(
    halted | error_line ~ "error",
    skipped & !completed ~ "skipped",
    completed & no_results ~ "success_no_results",
    completed ~ "success",
    TRUE ~ "incomplete"
  )

  # Extract SLURM job ID from filename (cholera_detect_{job_id}_{task}.log)
  slurm_job_id <- str_match(basename(path), "^cholera_detect_(\\d+)_")[, 2]

  tibble(
    log_file          = basename(path),
    slurm_job_id      = slurm_job_id,
    array_task_id     = array_task_id,
    config            = config,
    who_region        = who_region,
    country_iso3      = country_iso3,
    outcome           = outcome,
    n_stage1_files    = n_stage1_files,
    n_windows_empty   = n_windows_empty,
    n_windows_no_ob   = n_windows_no_ob,
    n_windows_ob      = n_windows_ob,
    n_windows_failed  = n_windows_failed,
    total_rows        = total_rows,
    window_error_types = window_error_types
  )
}

# ── Main ───────────────────────────────────────────────────────────────────────
log_files <- list.files(log_dir,
                        pattern = "^cholera_detect.*\\.log$",
                        full.names = TRUE)

if (length(log_files) == 0) stop("No detection .log files found in: ", log_dir)

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

  cat("--- Window counts ---\n")
  df |>
    summarise(
      countries_run      = sum(outcome %in% c("success", "success_no_results")),
      total_stage1_files = sum(n_stage1_files,   na.rm = TRUE),
      windows_empty      = sum(n_windows_empty,  na.rm = TRUE),
      windows_no_ob      = sum(n_windows_no_ob,  na.rm = TRUE),
      windows_with_ob    = sum(n_windows_ob,     na.rm = TRUE),
      windows_failed     = sum(n_windows_failed, na.rm = TRUE),
      total_output_rows  = sum(total_rows,        na.rm = TRUE)
    ) |> print()

  failed_df <- df |> filter(n_windows_failed > 0)
  if (nrow(failed_df) > 0) {
    cat(sprintf("--- Per-window error types (%d countries affected) ---\n",
                nrow(failed_df)))
    failed_df |>
      mutate(error_type = str_split(window_error_types, ";")) |>
      unnest(error_type) |>
      count(error_type, sort = TRUE) |>
      mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
      print(n = Inf)

    cat("--- Countries with failures ---\n")
    failed_df |>
      select(who_region, country_iso3, n_windows_failed, window_error_types,
             n_windows_ob, total_rows) |>
      arrange(desc(n_windows_failed)) |>
      print(n = 40)
  }

  incomplete <- df |> filter(outcome == "incomplete")
  if (nrow(incomplete) > 0) {
    cat(sprintf("--- Incomplete jobs (%d) ---\n", nrow(incomplete)))
    incomplete |>
      select(log_file, array_task_id, who_region, country_iso3,
             n_windows_ob, n_windows_failed) |>
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
