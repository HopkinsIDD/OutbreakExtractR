#test if all daily data has been averaged to weekly
test_that("observation_aggregator works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  daily_outbreak_ts<-outbreak_ts%>%subset(location_period_id==9931)
  daily_outbreak_ts<-clean_psql_data(original_data=daily_outbreak_ts)
  cleaned_outbreak_ts<-observation_aggregator(daily_outbreak_data=daily_outbreak_ts)
  testthat::expect_true(all(cleaned_outbreak_ts$TR-cleaned_outbreak_ts$TL+1==7))
})