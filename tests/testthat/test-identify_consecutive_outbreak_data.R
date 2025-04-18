outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))

#test if the consecutive weeks can be identified correctly
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts,
    minimum_consecutive_reports = 3,
    temporal_scale="weekly"
  )
  testthat::expect_true(all(outbreak_ts_cons$consecutive_obs))
})

#test if the consecutive weeks are shorter than the minimum consecutive weeks can be identified correctly
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts_tmp<-outbreak_ts[1:2,]
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3
  )
  testthat::expect_true(all(!outbreak_ts_cons$consecutive_obs))
})

#test if inconsecutive weeks among consecutive weeks could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts$TL[4]<-lubridate::ymd("2000-01-01")
  outbreak_ts$TR[4]<- outbreak_ts$TL[4]+6 
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts,
    minimum_consecutive_reports = 3,
    temporal_scale="weekly"
  )
  testthat::expect_true(any(!outbreak_ts_cons$consecutive_obs))
})

#test if consecutive days could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts$TL<-lubridate::ymd("2014-02-03")+1:376
  outbreak_ts$TR<-outbreak_ts$TL
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts,
    minimum_consecutive_reports = 3,
    temporal_scale="daily"
  )
  testthat::expect_true(all(outbreak_ts_cons$consecutive_obs))
})

#test if the inconsecutive days could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts$TL<-lubridate::ymd("2014-02-03")+1:376
  outbreak_ts$TR<-outbreak_ts$TL
  outbreak_ts$TL[2] = outbreak_ts$TL[2]+1
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts,
    minimum_consecutive_reports = 3,
    temporal_scale="daily"
  )
  testthat::expect_true(any(!outbreak_ts_cons$consecutive_obs))
})

#test if the consecutive days with shorter than minimum reports could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-read.csv("clean_outbreak_testing_data.csv") %>% subset(location_period_id==1) %>% mutate(TL = as.Date(TL), TR = as.Date(TR))
  outbreak_ts$TL<-lubridate::ymd("2014-02-03")+1:376
  outbreak_ts$TR<-outbreak_ts$TL
  outbreak_ts_tmp<-outbreak_ts[1:2,]
  outbreak_ts_tmp$TR<-outbreak_ts_tmp$TL
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3,
    temporal_scale="daily"
  )
  testthat::expect_true(all(!outbreak_ts_cons$consecutive_obs))
})