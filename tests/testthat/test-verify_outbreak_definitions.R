library(dplyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a minimal weekly outbreak dataframe with known structure.
# 14 weeks: weeks 1-8 high-risk, 9-10 low-risk tail (in outbreak), 11-14 washout.
# By default, all definition checks should PASS with:
#   outbreak_start_definition = "consecutive", min_weeks_above = 2, tail_period = 6
make_valid_df <- function(
    location         = "TestCountry",
    pop              = 1000,
    threshold        = 0.005,   # risk = "high" when sCh/pop >= threshold (sCh >= 5)
    sCh_outbreak     = c(10, 15, 12, 20, 18, 14, 8, 6, 3, 2),  # weeks 1-10
    sCh_washout      = c(1, 0, 2, 1)                             # weeks 11-14
) {
  n_out  <- length(sCh_outbreak)
  n_wash <- length(sCh_washout)
  n      <- n_out + n_wash
  tl     <- as.Date("2014-01-06") + 7L * seq(0L, n - 1L)

  sCh <- c(sCh_outbreak, sCh_washout)

  tibble::tibble(
    location        = location,
    TL              = tl,
    TR              = tl + 6L,
    sCh             = sCh,
    pop             = pop,
    threshold       = threshold,
    risk            = ifelse(sCh > 0 & sCh / pop >= threshold, "high", "low"),
    epidemic_start  = c(TRUE, rep(FALSE, n - 1L)),
    epidemic_tail   = c(rep(FALSE, n_out - 2L), TRUE, FALSE, rep(FALSE, n_wash)),
    outbreak_number = c(rep(1L, n_out), rep(0L, n_wash)),
    `Time Period`   = factor(
      ifelse(c(rep(1L, n_out), rep(0L, n_wash)) > 0,
             "outbreak period", "non-outbreak period"),
      levels = c("outbreak period", "non-outbreak period")
    )
  )
}

# Shortcut: run verify and filter to one check name
check_status <- function(results, check_name) {
  results$status[results$check == check_name]
}

# ---------------------------------------------------------------------------
# Baseline: all checks pass
# ---------------------------------------------------------------------------

test_that("all checks pass on a structurally valid dataset (consecutive mode)", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(
    outbreak_list             = df,
    outbreak_start_definition = "consecutive",
    min_weeks_above           = 2L,
    tail_period               = 6L,
    cumulative_min_cases      = 10
  )

  non_info <- results[results$status != "INFO" & results$status != "SKIP", ]
  expect_true(all(non_info$status == "PASS"),
    info = paste("Unexpected FAIL rows:\n",
                 paste(non_info[non_info$status == "FAIL", ]$detail, collapse = "\n")))
})

test_that("function accepts a pre-bound dataframe (not a list)", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_s3_class(results, "tbl_df")
  expect_true(nrow(results) > 0)
})

test_that("function returns an empty tibble when given no data", {
  results <- verify_outbreak_definitions(data.frame())
  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 0L)
})

test_that("result tibble has expected columns", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_true(all(c("location", "outbreak_number", "check", "status", "value", "detail")
                  %in% names(results)))
})

# ---------------------------------------------------------------------------
# CHECK 1 — risk_classification_consistency
# ---------------------------------------------------------------------------

test_that("risk_classification_consistency FAILS when risk label contradicts sCh/pop >= threshold", {
  df           <- make_valid_df()
  df$risk[3]   <- "low"   # row 3 has sCh=12, sCh/pop=0.012 >= threshold=0.005 → should be "high"
  results      <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "risk_classification_consistency"), "FAIL")
  expect_equal(results$value[results$check == "risk_classification_consistency"], 1)
})

test_that("risk_classification_consistency PASSES when all labels are correct", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "risk_classification_consistency"), "PASS")
})

# ---------------------------------------------------------------------------
# CHECK 2 — no_zero_case_epidemic_start
# ---------------------------------------------------------------------------

test_that("no_zero_case_epidemic_start FAILS when epidemic start has sCh == 0", {
  df          <- make_valid_df()
  df$sCh[1]   <- 0L
  df$risk[1]  <- "low"   # adjust risk to match sCh=0
  results     <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "no_zero_case_epidemic_start"), "FAIL")
  expect_equal(results$value[results$check == "no_zero_case_epidemic_start"], 1)
})

test_that("no_zero_case_epidemic_start PASSES when epidemic start has sCh > 0", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "no_zero_case_epidemic_start"), "PASS")
})

# ---------------------------------------------------------------------------
# CHECK 3 — epidemic_start_in_outbreak_period
# ---------------------------------------------------------------------------

test_that("epidemic_start_in_outbreak_period FAILS when start is outside outbreak", {
  df                  <- make_valid_df()
  df$outbreak_number[1] <- 0L   # epidemic_start=TRUE but outbreak_number=0
  results             <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "epidemic_start_in_outbreak_period"), "FAIL")
})

test_that("epidemic_start_in_outbreak_period PASSES when all starts are inside outbreak", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "epidemic_start_in_outbreak_period"), "PASS")
})

# ---------------------------------------------------------------------------
# CHECK 4 — consecutive_start_validity
# ---------------------------------------------------------------------------

test_that("consecutive_start_validity FAILS when the week after start is low-risk", {
  df          <- make_valid_df()
  # Break week 2 so the start (week 1) is not followed by 2 consecutive high-risk weeks
  df$sCh[2]   <- 1L
  df$risk[2]  <- "low"
  results     <- verify_outbreak_definitions(df,
    outbreak_start_definition = "consecutive",
    min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "consecutive_start_validity"), "FAIL")
})

test_that("consecutive_start_validity PASSES with min_weeks_above consecutive high-risk starts", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df,
    outbreak_start_definition = "consecutive",
    min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "consecutive_start_validity"), "PASS")
})

test_that("consecutive_start_validity PASSES with min_weeks_above = 3", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df,
    outbreak_start_definition = "consecutive",
    min_weeks_above = 3L, tail_period = 6L)
  # Weeks 1-3 are all high-risk
  expect_equal(check_status(results, "consecutive_start_validity"), "PASS")
})

# ---------------------------------------------------------------------------
# CHECK 5 — dual_window_start_validity  (dual_window mode)
# ---------------------------------------------------------------------------

test_that("dual_window_start_validity PASSES when sliding window trigger is satisfied", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df,
    outbreak_start_definition       = "dual_window",
    min_weeks_above                  = 2L,
    window_weeks                     = 3L,
    use_cumulative_trigger           = FALSE,
    tail_period                      = 6L)
  # Weeks 1-3: all high → sliding window (3 weeks, min 2 high) satisfied
  expect_equal(check_status(results, "dual_window_start_validity"), "PASS")
})

test_that("dual_window_start_validity FAILS when neither trigger is satisfied", {
  df <- make_valid_df()
  # Make all weeks low-risk but keep epidemic_start flag — force a contradiction
  df$risk[1:3]  <- "low"
  df$sCh[1:3]   <- 1L
  # No cumulative trigger either (sCh too low)
  results <- verify_outbreak_definitions(df,
    outbreak_start_definition       = "dual_window",
    min_weeks_above                  = 2L,
    window_weeks                     = 3L,
    use_cumulative_trigger           = TRUE,
    cumulative_trigger_type          = "cumulative_case_threshold",
    cumulative_windows               = 3L,
    cumulative_case_threshold_ratio  = 1.5,
    tail_period                      = 6L)
  expect_equal(check_status(results, "dual_window_start_validity"), "FAIL")
})

test_that("dual_window_start_validity PASSES when only the cumulative trigger is satisfied", {
  df <- make_valid_df()
  # Make weeks 2-3 low-risk so sliding window fails (only 1 high in 3)
  df$risk[2:3]  <- "low"
  df$sCh[2:3]   <- 2L
  # But make cumulative cases in weeks 1-3 = 10+2+2 = 14, threshold = 0.005*1000*1.5 = 7.5 → meets it
  results <- verify_outbreak_definitions(df,
    outbreak_start_definition       = "dual_window",
    min_weeks_above                  = 2L,
    window_weeks                     = 3L,
    use_cumulative_trigger           = TRUE,
    cumulative_trigger_type          = "cumulative_case_threshold",
    cumulative_windows               = 3L,
    cumulative_case_threshold_ratio  = 1.5,
    tail_period                      = 6L)
  expect_equal(check_status(results, "dual_window_start_validity"), "PASS")
})

# ---------------------------------------------------------------------------
# CHECK 6 — epidemic_tail_validity
# ---------------------------------------------------------------------------

test_that("epidemic_tail_validity PASSES when tail rows are followed by tail_period low-risk weeks", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "epidemic_tail_validity"), "PASS")
})

test_that("epidemic_tail_validity FAILS when a tail row is followed by a high-risk week", {
  df <- make_valid_df()
  # epidemic_tail is TRUE at row 9; make row 10 high-risk (violates the 6-week low-risk run)
  df$sCh[10]  <- 50L
  df$risk[10] <- "high"
  results     <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "epidemic_tail_validity"), "FAIL")
})

# ---------------------------------------------------------------------------
# CHECK 7 — min_high_risk_weeks_per_outbreak
# ---------------------------------------------------------------------------

test_that("min_high_risk_weeks_per_outbreak FAILS when outbreak has too few high-risk weeks", {
  df <- make_valid_df()
  # Keep only 1 high-risk week in the outbreak (< min_weeks_above = 2)
  df$risk[2:8]  <- "low"
  df$sCh[2:8]   <- 1L
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "min_high_risk_weeks_per_outbreak"), "FAIL")
  expect_equal(results$value[results$check == "min_high_risk_weeks_per_outbreak"], 1)
})

test_that("min_high_risk_weeks_per_outbreak PASSES when outbreak has enough high-risk weeks", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "min_high_risk_weeks_per_outbreak"), "PASS")
})

# ---------------------------------------------------------------------------
# CHECK 8 — cumulative_cases_at_start
# ---------------------------------------------------------------------------

test_that("cumulative_cases_at_start PASSES when first cumulative_windows weeks >= cumulative_min_cases", {
  df      <- make_valid_df()  # first 3 weeks: 10+15+12 = 37 >= 30
  results <- verify_outbreak_definitions(df,
    min_weeks_above      = 2L,
    tail_period          = 6L,
    cumulative_min_cases = 30,
    cumulative_windows   = 3L)
  expect_equal(check_status(results, "cumulative_cases_at_start"), "PASS")
  expect_equal(results$value[results$check == "cumulative_cases_at_start"], 37)
})

test_that("cumulative_cases_at_start FAILS when first weeks sum below cumulative_min_cases", {
  df      <- make_valid_df()  # first 3 weeks: 10+15+12 = 37 < 50
  results <- verify_outbreak_definitions(df,
    min_weeks_above      = 2L,
    tail_period          = 6L,
    cumulative_min_cases = 50,
    cumulative_windows   = 3L)
  expect_equal(check_status(results, "cumulative_cases_at_start"), "FAIL")
})

test_that("cumulative_cases_at_start is absent from results when cumulative_min_cases is NULL", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df,
    min_weeks_above      = 2L,
    tail_period          = 6L,
    cumulative_min_cases = NULL)
  expect_false("cumulative_cases_at_start" %in% results$check)
})

# ---------------------------------------------------------------------------
# CHECK 9 — outbreak_weekly_continuity
# ---------------------------------------------------------------------------

test_that("outbreak_weekly_continuity PASSES when all within-outbreak weeks are 7 days apart", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "outbreak_weekly_continuity"), "PASS")
})

test_that("outbreak_weekly_continuity FAILS when a gap exists within an outbreak", {
  df      <- make_valid_df()
  # Jump week 5 forward by 14 days instead of 7.
  # This creates two violations: gap before row 5 (14 d) and after row 5 (0 d).
  df$TL[5]  <- df$TL[5] + 7L
  df$TR[5]  <- df$TR[5] + 7L
  results   <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "outbreak_weekly_continuity"), "FAIL")
  expect_equal(results$value[results$check == "outbreak_weekly_continuity"], 2)
})

# ---------------------------------------------------------------------------
# CHECK 10 — tail_period_after_outbreak
# ---------------------------------------------------------------------------

test_that("tail_period_after_outbreak PASS when enough low-risk non-outbreak weeks follow", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "tail_period_after_outbreak"), "PASS")
})

test_that("tail_period_after_outbreak is SKIP when no data follows the outbreak", {
  df      <- make_valid_df()
  # Remove washout rows so outbreak is the last observation
  df      <- df[df$outbreak_number > 0, ]
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "tail_period_after_outbreak"), "SKIP")
})

test_that("tail_period_after_outbreak FAILS when week immediately after outbreak is high-risk", {
  df            <- make_valid_df()
  # Make the first non-outbreak week high-risk (week 11 → sCh=50)
  df$sCh[11]    <- 50L
  df$risk[11]   <- "high"
  results       <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_equal(check_status(results, "tail_period_after_outbreak"), "FAIL")
  expect_equal(results$value[results$check == "tail_period_after_outbreak"], 0)
})

# ---------------------------------------------------------------------------
# CHECK 12 — inter_outbreak_gap_weeks
# ---------------------------------------------------------------------------

test_that("inter_outbreak_gap_weeks PASSES when gap between two outbreaks >= tail_period", {
  ob1 <- make_valid_df()
  # Place ob2 so that TL[1] of ob2 is 8 weeks + 1 day after max(TR) of ob1's outbreak
  ob2         <- make_valid_df()
  ob1_end_tr  <- max(ob1$TR[ob1$outbreak_number > 0])
  shift       <- as.integer(ob1_end_tr - min(ob2$TL)) + 7L * 8L + 1L
  ob2$TL      <- ob2$TL + shift
  ob2$TR      <- ob2$TR + shift
  ob2$outbreak_number[ob2$outbreak_number > 0] <- 2L

  df      <- dplyr::bind_rows(ob1, ob2) %>% dplyr::arrange(TL)
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  gap_rows <- results[results$check == "inter_outbreak_gap_weeks", ]
  expect_equal(nrow(gap_rows), 1L)
  expect_equal(gap_rows$status, "PASS")
})

test_that("inter_outbreak_gap_weeks FAILS when gap between outbreaks < tail_period", {
  ob1 <- make_valid_df()
  # Place ob2 so that TL[1] of ob2 is 2 weeks + 1 day after max(TR) of ob1's outbreak (<tail_period=6)
  ob2         <- make_valid_df()
  ob1_end_tr  <- max(ob1$TR[ob1$outbreak_number > 0])
  shift       <- as.integer(ob1_end_tr - min(ob2$TL)) + 7L * 2L + 1L
  ob2$TL      <- ob2$TL + shift
  ob2$TR      <- ob2$TR + shift
  ob2$outbreak_number[ob2$outbreak_number > 0] <- 2L

  df      <- dplyr::bind_rows(ob1, ob2) %>% dplyr::arrange(TL)
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  gap_rows <- results[results$check == "inter_outbreak_gap_weeks", ]
  expect_equal(nrow(gap_rows), 1L)
  expect_equal(gap_rows$status, "FAIL")
})

# ---------------------------------------------------------------------------
# CHECK — outbreak_summary (INFO rows)
# ---------------------------------------------------------------------------

test_that("outbreak_summary INFO row is present and correct for a single outbreak", {
  df      <- make_valid_df()
  results <- verify_outbreak_definitions(df,
    min_weeks_above = 2L, tail_period = 6L, cumulative_min_cases = NULL)
  info_row <- results[results$check == "outbreak_summary" & results$status == "INFO", ]
  expect_equal(nrow(info_row), 1L)
  # Total cases in outbreak (weeks 1-10): 10+15+12+20+18+14+8+6+3+2 = 108
  expect_equal(info_row$value, 108)
})

# ---------------------------------------------------------------------------
# Multi-location
# ---------------------------------------------------------------------------

test_that("function handles multiple locations and returns results for each", {
  df1 <- make_valid_df(location = "CountryA")
  df2 <- make_valid_df(location = "CountryB")
  df  <- dplyr::bind_rows(df1, df2)
  results <- verify_outbreak_definitions(df, min_weeks_above = 2L, tail_period = 6L)
  expect_true("CountryA" %in% results$location)
  expect_true("CountryB" %in% results$location)
})
