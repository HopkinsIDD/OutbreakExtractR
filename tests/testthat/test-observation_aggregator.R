#test if all daily data has been averaged to weekly
test_that("observation_aggregator works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts$TL<-lubridate::ymd("2014-02-03")+1:2089
  outbreak_ts$TR<-outbreak_ts$TL
  cleaned_outbreak_ts<-observation_aggregator(daily_outbreak_data=outbreak_ts)
  testthat::expect_true(all(cleaned_outbreak_ts$TR-cleaned_outbreak_ts$TL+1==7))
})
test_that("observation_aggregator keeps cCh NA for a week with no confirmed counts", {
  days <- lubridate::ymd("2014-02-03") + 0:13   # two epiweeks, Monday start
  daily <- data.frame(
    location = "AggCountry",
    TL = days, TR = days,
    epiweek = OutbreakExtractR::get_epiweek(days),
    sCh = 1,
    cCh = c(rep(NA, 7), 1, NA, 2, NA, NA, NA, NA),
    deaths = 0,
    original_location_name = "AggCountry",
    observation_collection_id = "1",
    stringsAsFactors = FALSE
  )
  weekly <- observation_aggregator(daily_outbreak_data = daily) %>% dplyr::arrange(TL)
  expect_equal(nrow(weekly), 2L)
  expect_true(is.na(weekly$cCh[1]))
  expect_equal(weekly$cCh[2], 3)
  expect_equal(weekly$sCh, c(7, 7))
})
