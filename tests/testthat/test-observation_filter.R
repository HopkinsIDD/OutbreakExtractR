outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% 
  mutate(TL = as.Date(TL), TR = as.Date(TR),
         temporal_scale = "weekly")

#test whether time filter works
test_that("observation_filter works", {
  lower_time_bound<-lubridate::ymd("2014-03-24")
  upper_time_bound<-lubridate::ymd("2014-05-18")
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           time_lower_bound_filter = lower_time_bound,
                                           time_upper_bound_filter = upper_time_bound
                                           )
  testthat::expect_true(all(filtered_outbreak_ts$TL>=lower_time_bound))
})

#test whether time filter works
test_that("observation_filter works", {
  lower_time_bound<-lubridate::ymd("2014-03-24")
  upper_time_bound<-lubridate::ymd("2014-05-18")
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           time_lower_bound_filter = lower_time_bound,
                                           time_upper_bound_filter = upper_time_bound
  )
  testthat::expect_true(all(filtered_outbreak_ts$TR<=upper_time_bound))
})

#test whether temporal_scale filter works
test_that("observation_filter works", {
  outbreak_ts[1:3,]$temporal_scale<-"daily"
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           temporal_scale_filter = "weekly"
  )
  testthat::expect_true(all(filtered_outbreak_ts$temporal_scale=="weekly"))
})

#test whether who_region filter works
test_that("observation_filter works", {
  outbreak_ts[1:3,]$who_region<-"SEAR"
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           who_regions=c("AFR")
  )
  testthat::expect_true(all(filtered_outbreak_ts$who_region=="AFR"))
})

#test whether spatial_scale filter works
test_that("observation_filter works", {
  outbreak_ts[1:3,]$spatial_scale<-c("admin3","admin1","admin2")
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           spatial_scale_filter = "country")
  testthat::expect_true(all(filtered_outbreak_ts$spatial_scale=="country"))
})

#test whether na sCh/cCh filter works
test_that("observation_filter works", {
  outbreak_ts[1:3,]$sCh<-NA
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           remove_na_sCh=FALSE)
  testthat::expect_true(any(is.na(filtered_outbreak_ts$sCh)))
})

test_that("observation_filter works", {
  outbreak_ts[1:3,]$cCh<-NA
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           remove_na_cCh=TRUE)
  testthat::expect_true(all(is.na(filtered_outbreak_ts$cCh)==F))
})

#test whether minimum_daily_case filter works
test_that("observation_filter works", {
  filtered_outbreak_ts<-observation_filter(outbreak_data=outbreak_ts,
                                           minimum_daily_cases = 10^10)
  testthat::expect_equal(nrow(filtered_outbreak_ts),0)
})
