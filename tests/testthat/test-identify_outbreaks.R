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
