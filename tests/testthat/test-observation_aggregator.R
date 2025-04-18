#test if all daily data has been averaged to weekly
test_that("observation_aggregator works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts$TL<-lubridate::ymd("2014-02-03")+1:2089
  outbreak_ts$TR<-outbreak_ts$TL
  cleaned_outbreak_ts<-observation_aggregator(daily_outbreak_data=outbreak_ts)
  testthat::expect_true(all(cleaned_outbreak_ts$TR-cleaned_outbreak_ts$TL+1==7))
})