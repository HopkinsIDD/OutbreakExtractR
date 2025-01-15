#test if the consecutive weeks can be identified correctly
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts_tmp<-outbreak_ts%>%subset(location_period_id==1123)
  outbreak_ts_tmp<-outbreak_ts_tmp[1:10,]
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3,
    temporal_scale="weekly"
  )
  testthat::expect_true(all(outbreak_ts_cons$consecutive_obs))
})

#test if the consecutive weeks are shorter than the minimum consecutive weeks can be identified correctly
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts_tmp<-outbreak_ts%>%subset(location_period_id==1123)
  outbreak_ts_tmp<-outbreak_ts_tmp[1:2,]
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3
  )
  testthat::expect_true(all(!outbreak_ts_cons$consecutive_obs))
})

#test if inconsecutive weeks among consecutive weeks could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts_tmp<-outbreak_ts%>%subset(location_period_id==1123)
  outbreak_ts_tmp<-outbreak_ts_tmp[1:10,]
  outbreak_ts_tmp$TL[4]<-lubridate::ymd("2000-01-01")
  outbreak_ts_tmp$TR[4]<- outbreak_ts_tmp$TL[4]+6 
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3,
    temporal_scale="weekly"
  )
  testthat::expect_true(any(!outbreak_ts_cons$consecutive_obs))
})

#test if consecutive days could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts_tmp<-outbreak_ts%>%subset(location_period_id==1123)
  outbreak_ts_tmp<-outbreak_ts_tmp[1:10,]
  outbreak_ts_tmp$TL<-lubridate::ymd("2014-03-10")+1:10
  outbreak_ts_tmp$TR<-outbreak_ts_tmp$TL
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3,
    temporal_scale="daily"
  )
  testthat::expect_true(all(outbreak_ts_cons$consecutive_obs))
})

#test if the inconsecutive days could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts_tmp<-outbreak_ts%>%subset(location_period_id==9931)
  outbreak_ts_tmp<-outbreak_ts_tmp[1:10,]
  outbreak_ts_tmp$TL[5]<-outbreak_ts_tmp$TL[5]+10
  outbreak_ts_tmp$TR<-outbreak_ts_tmp$TL
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3,
    temporal_scale="daily"
  )
  testthat::expect_true(any(!outbreak_ts_cons$consecutive_obs))
})

#test if the consecutive days with shorter than minimum reports could be identified
test_that("identify_consecutive_outbreak_data works", {
  outbreak_ts<-readRDS("data/outbreak_data.rds")
  outbreak_ts_tmp<-outbreak_ts%>%subset(location_period_id==1123)
  outbreak_ts_tmp<-outbreak_ts_tmp[1:2,]
  outbreak_ts_tmp$TR<-outbreak_ts_tmp$TL
  outbreak_ts_cons<-identify_consecutive_outbreak_data(
    outbreak_data = outbreak_ts_tmp,
    minimum_consecutive_reports = 3,
    temporal_scale="daily"
  )
  testthat::expect_true(all(!outbreak_ts_cons$consecutive_obs))
})