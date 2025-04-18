library(tidyverse)
outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% 
  mutate(TL = as.Date(TL),TR = as.Date(TR))

#test whether the fixed outbreak threshold works
test_that("get_outbreak_threshold works", {
  outbreak_ts$threshold=NA
  args(get_outbreak_threshold)
  outbreak_ts[outbreak_ts$location_period_id==1,]$threshold <- get_outbreak_threshold (
    threshold_type = c("fixed threshold"),
    fixed_outbreak_threshold = 100,
    surveillance_data = outbreak_ts%>%subset(location_period_id==1), #daily or weekly cholera case data by location period
    zero_case_assumption = TRUE
  ) 
  testthat::expect_equal(unique(outbreak_ts[outbreak_ts$location_period_id==1,]$threshold), 100)
})

#when the fixed outbreak threshold is smaller than 0 which is invalid
test_that("get_outbreak_threshold works", {
  outbreak_ts$threshold=NA
  testthat::expect_error(
    get_outbreak_threshold (
      threshold_type = c("fixed threshold"),
      fixed_outbreak_threshold = -10,
      surveillance_data = outbreak_ts%>%subset(location_period_id==1), #daily or weekly cholera case data by location period
      zero_case_assumption = TRUE
    ) 
  )
})

#when the fixed outbreak threshold is not assigned appropriately
test_that("get_outbreak_threshold works", {
  outbreak_ts$threshold=NA
  testthat::expect_error(
    get_outbreak_threshold (
      threshold_type = c("fixed threshold"),
      fixed_outbreak_threshold = NULL,
      surveillance_data = outbreak_ts%>%subset(location_period_id==1), #daily or weekly cholera case data by location period
      zero_case_assumption = TRUE
    ) 
  )
})

#when the mean weekly incidence threshold is not assigned appropriately
test_that("get_outbreak_threshold works", {
  outbreak_ts$threshold = NA
  outbreak_ts$pop = 100
  outbreak_ts_thre <- get_outbreak_threshold(
    threshold_type = c("mean weekly incidence rate"),
    surveillance_data = outbreak_ts%>%subset(location_period_id==2), #daily or weekly cholera case data by location period
    zero_case_assumption = TRUE) 
  testthat::expect_true(all(!is.na(outbreak_ts_thre$risk)))
  # if the weekly incidence rate is smaller than the threshold, it should be classified as low risk.
  testthat::expect_true(all(any(outbreak_ts_thre[outbreak_ts_thre$sCh/outbreak_ts_thre$pop < outbreak_ts_thre$threshold,]$risk == "low")))
  # if the weekly incidence rate is equal or greater than the threshold, it should be classified as high risk.
  testthat::expect_true(all(any(outbreak_ts_thre[outbreak_ts_thre$sCh/outbreak_ts_thre$pop >= outbreak_ts_thre$threshold,]$risk == "high")))
})