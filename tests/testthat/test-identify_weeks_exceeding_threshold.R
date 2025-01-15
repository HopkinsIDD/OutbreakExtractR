#test if the correct high risk weeks are assigned
test_that("identify_weeks_exceeding_threshold works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts$threshold=0
  outbreak_ts$population=1000
  
  outbreak_ts<-identify_weeks_exceeding_threshold(
    outbreak_data = outbreak_ts,
    threshold_col = "threshold",
    threshold_type = "rate",
    pop_col = "population"
  )
  testthat::expect_true(all(outbreak_ts$risk=="high"))
})

#test when the population column is invalid
test_that("identify_weeks_exceeding_threshold works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  testthat::expect_error(
    identify_weeks_exceeding_threshold(
      outbreak_data = outbreak_ts,
      threshold_col = "threshold",
      threshold_type = "rate",
      pop_col = "pop"
    )
  )
})

#test whether the low risk groups are assigned correctly
test_that("identify_weeks_exceeding_threshold works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts$threshold=10^7
    outbreak_ts<-identify_weeks_exceeding_threshold(
      outbreak_data = outbreak_ts,
      threshold_col = "threshold",
      threshold_type = "case",
      pop_col = NULL
  )
  testthat::expect_true(all(outbreak_ts$risk=="low"))
})