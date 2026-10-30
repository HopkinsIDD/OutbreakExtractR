# Tests for the post-detection outbreak size filter (filter_small_outbreaks),
# the internal helper used by identify_outbreaks() when
# filter_outbreaks_by_size = TRUE.

make_outbreak_df <- function() {
  # Two numbered outbreaks plus surrounding non-outbreak (0) weeks.
  #   outbreak 1: 40 + 50 + 30          = 120 cases  (large)
  #   outbreak 2:  5 +  4 +  3          =  12 cases  (small)
  data.frame(
    location       = "TestCountry",
    outbreak_number = c(0, 1, 1, 1, 0, 0, 2, 2, 2, 0),
    sCh            = c(0, 40, 50, 30, 0, 0, 5, 4, 3, 0),
    stringsAsFactors = FALSE
  )
}

test_that("filter_small_outbreaks drops only outbreaks below the threshold", {
  df  <- make_outbreak_df()
  out <- filter_small_outbreaks(df, min_total_cases = 50)

  # Outbreak 1 (120 cases) retained, outbreak 2 (12 cases) zeroed.
  expect_equal(out$outbreak_number, c(0, 1, 1, 1, 0, 0, 0, 0, 0, 0))
  # sCh column is untouched.
  expect_equal(out$sCh, df$sCh)
})

test_that("filter_small_outbreaks keeps all outbreaks when threshold is below both", {
  df  <- make_outbreak_df()
  out <- filter_small_outbreaks(df, min_total_cases = 10)
  expect_equal(out$outbreak_number, df$outbreak_number)
})

test_that("filter_small_outbreaks drops all outbreaks when threshold exceeds both", {
  df  <- make_outbreak_df()
  out <- filter_small_outbreaks(df, min_total_cases = 1000)
  expect_true(all(out$outbreak_number == 0))
})

test_that("filter_small_outbreaks returns all-zero input unchanged", {
  df <- data.frame(
    location       = "TestCountry",
    outbreak_number = c(0, 0, 0),
    sCh            = c(0, 2, 1),
    stringsAsFactors = FALSE
  )
  out <- filter_small_outbreaks(df, min_total_cases = 50)
  expect_equal(out$outbreak_number, df$outbreak_number)
})

test_that("filter_small_outbreaks handles NA cases via na.rm", {
  df <- data.frame(
    location       = "TestCountry",
    outbreak_number = c(1, 1, 1, 2, 2),
    sCh            = c(60, NA, 70, 1, NA),   # ob1 = 130 (keep), ob2 = 1 (drop)
    stringsAsFactors = FALSE
  )
  out <- filter_small_outbreaks(df, min_total_cases = 50)
  expect_equal(out$outbreak_number, c(1, 1, 1, 0, 0))
})

# ---------------------------------------------------------------------------
# identify_outbreaks(): keep_nonoutbreak_locations
# ---------------------------------------------------------------------------

# A location with zero cases every week never reaches risk == "high" (which
# requires sCh > 0), so it never gets an epidemic_start. This exercises the
# "no epidemic start found for this location" branch directly.
make_no_outbreak_data <- function(location = "QuietCountry", n_weeks = 10, pop = 1000) {
  tl <- as.Date("2014-01-06") + 7L * seq(0L, n_weeks - 1L)
  data.frame(
    location = location,
    TL       = tl,
    TR       = tl + 6L,
    sCh      = rep(0, n_weeks),
    pop      = pop,
    stringsAsFactors = FALSE
  )
}

test_that("keep_nonoutbreak_locations = FALSE (default) drops locations with no epidemic start", {
  df  <- make_no_outbreak_data()
  out <- identify_outbreaks(
    threshold_type        = "mean weekly incidence rate",
    original_data         = df,
    zero_case_assumption  = TRUE,
    outbreak_start_definition = "consecutive",
    min_weeks_above       = 2,
    window_weeks          = 3,
    cumulative_windows    = 3,
    cumulative_case_threshold_ratio = 1.5,
    cumulative_trigger_type = "cumulative_case_threshold",
    use_cumulative_trigger  = FALSE,
    cumulative_min_cases    = NULL,
    nonzero_windows         = NULL,
    tail_period             = 6
  )
  expect_equal(nrow(out[["QuietCountry"]]), 0L)
})

test_that("keep_nonoutbreak_locations = TRUE retains the full series as a non-outbreak period", {
  df  <- make_no_outbreak_data()
  out <- identify_outbreaks(
    threshold_type        = "mean weekly incidence rate",
    original_data         = df,
    zero_case_assumption  = TRUE,
    outbreak_start_definition = "consecutive",
    min_weeks_above       = 2,
    window_weeks          = 3,
    cumulative_windows    = 3,
    cumulative_case_threshold_ratio = 1.5,
    cumulative_trigger_type = "cumulative_case_threshold",
    use_cumulative_trigger  = FALSE,
    cumulative_min_cases    = NULL,
    nonzero_windows         = NULL,
    tail_period             = 6,
    keep_nonoutbreak_locations = TRUE
  )
  loc_out <- out[["QuietCountry"]]
  expect_equal(nrow(loc_out), nrow(df))
  expect_true(all(loc_out$outbreak_number == 0))
  expect_true(all(as.character(loc_out$`Time Period`) == "non-outbreak period"))
})

test_that("keep_nonoutbreak_locations = TRUE does not affect a location that does have an outbreak", {
  outbreak_df <- data.frame(
    location = "OutbreakCountry",
    TL       = as.Date("2014-01-06") + 7L * seq(0L, 9L),
    TR       = as.Date("2014-01-06") + 7L * seq(0L, 9L) + 6L,
    sCh      = c(50, 60, 55, 40, 30, 20, 5, 3, 2, 1),
    pop      = 1000,
    stringsAsFactors = FALSE
  )
  args <- list(
    threshold_type        = "mean weekly incidence rate",
    original_data         = outbreak_df,
    zero_case_assumption  = TRUE,
    outbreak_start_definition = "consecutive",
    min_weeks_above       = 2,
    window_weeks          = 3,
    cumulative_windows    = 3,
    cumulative_case_threshold_ratio = 1.5,
    cumulative_trigger_type = "cumulative_case_threshold",
    use_cumulative_trigger  = FALSE,
    cumulative_min_cases    = NULL,
    nonzero_windows         = NULL,
    tail_period             = 6
  )
  out_default <- do.call(identify_outbreaks, args)
  out_keep    <- do.call(identify_outbreaks, c(args, list(keep_nonoutbreak_locations = TRUE)))
  expect_equal(out_default[["OutbreakCountry"]], out_keep[["OutbreakCountry"]])
  expect_true(any(out_keep[["OutbreakCountry"]]$outbreak_number > 0))
})

# ---------------------------------------------------------------------------
# identify_outbreaks(): exclude_closing_tail
# ---------------------------------------------------------------------------

# Weekly series with pop = 1 under the mean-weekly-incidence threshold: weeks
# of 100 cases are "high", zero weeks are "low". tail_period = 6.
make_tail_series <- function(sCh, location = "TailCountry") {
  tl <- as.Date("2014-01-06") + 7L * seq(0L, length(sCh) - 1L)
  data.frame(location = location, TL = tl, TR = tl + 6L, sCh = sCh, pop = 1,
             stringsAsFactors = FALSE)
}

run_tail_outbreaks <- function(df, exclude_closing_tail, cumulative_min_cases = NULL, ...) {
  out <- identify_outbreaks(
    threshold_type        = "mean weekly incidence rate",
    original_data         = df,
    zero_case_assumption  = TRUE,
    outbreak_start_definition = "consecutive",
    min_weeks_above       = 2,
    window_weeks          = 3,
    cumulative_windows    = 3,
    cumulative_case_threshold_ratio = 1.5,
    cumulative_trigger_type = "cumulative_case_threshold",
    use_cumulative_trigger  = FALSE,
    cumulative_min_cases    = cumulative_min_cases,
    nonzero_windows         = NULL,
    tail_period             = 6,
    exclude_closing_tail    = exclude_closing_tail,
    ...
  )
  out[[unique(df$location)]]
}

# Rows of each numbered outbreak, as a list ordered by first row.
outbreak_rows <- function(loc_out) {
  rows <- split(seq_len(nrow(loc_out)), loc_out$outbreak_number)
  rows <- rows[names(rows) != "0"]
  unname(rows[order(vapply(rows, min, numeric(1)))])
}

test_that("a closed outbreak ends before its closing run only when exclude_closing_tail = TRUE", {
  # rows 4-7 high, then 8 low weeks (closing run starts at row 8)
  df <- make_tail_series(c(0, 0, 0, 100, 100, 100, 100, rep(0, 8)))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, FALSE)), list(4:13))
  trimmed <- run_tail_outbreaks(df, TRUE)
  expect_equal(outbreak_rows(trimmed), list(4:7))
  expect_true(all(as.character(trimmed$`Time Period`[8:15]) == "non-outbreak period"))
})

test_that("exclude_closing_tail trims every outbreak separated by a closing run", {
  # outbreaks at rows 3-5 and 13-15, each followed by 7 low weeks
  df <- make_tail_series(c(0, 0, 100, 100, 100, rep(0, 7), 100, 100, 100, rep(0, 7)))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, FALSE)), list(3:11, 13:21))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, TRUE)), list(3:5, 13:15))
})

test_that("starts not separated by a closing run stay one outbreak under exclude_closing_tail", {
  # highs at rows 3-5 and 9-11 with only 3 low weeks between them
  df <- make_tail_series(c(0, 0, 100, 100, 100, 0, 0, 0, 100, 100, 100, rep(0, 7)))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, FALSE)), list(3:17))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, TRUE)), list(3:11))
})

test_that("an outbreak with no full closing run runs to the series end either way", {
  # only 3 low weeks after the last high week: no closing run
  df <- make_tail_series(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, FALSE)), list(4:10))
  expect_equal(outbreak_rows(run_tail_outbreaks(df, TRUE)), list(4:10))
})

test_that("the size filter counts only the trimmed window when exclude_closing_tail = TRUE", {
  # 2 high weeks (40 cases) then 6 low weeks of 2 cases (12 cases) in the
  # closing run: 52 cases with the closing run, 40 without it.
  df <- make_tail_series(c(rep(0, 10), 20, 20, rep(2, 6), rep(0, 6)))
  keep <- run_tail_outbreaks(df, FALSE, cumulative_min_cases = 50,
                             filter_outbreaks_by_size = TRUE)
  expect_equal(outbreak_rows(keep), list(11:18))
  drop <- run_tail_outbreaks(df, TRUE, cumulative_min_cases = 50,
                             filter_outbreaks_by_size = TRUE)
  expect_true(all(drop$outbreak_number == 0))
})
