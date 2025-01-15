outbreak_ts<-readRDS("data/outbreak_data.rds")
total_shp <-sf::st_read("data/total_shp.shp")

#test whether the fixed outbreak threshold works
test_that("get_outbreak_threshold works", {
  outbreak_ts$threshold=NA
  outbreak_ts[outbreak_ts$location_period_id==1123,]$threshold <- get_outbreak_threshold (
    threshold_type = c("fixed threshold"),
    fixed_outbreak_threshold = 100,
    country = "NGA",
    shp = total_shp%>%subset(lctn_pr==1123),
    surveillance_data = outbreak_ts%>%subset(location_period_id==1123), #daily or weekly cholera case data by location period
    zero_case_assumption = TRUE
  ) 
  testthat::expect_equal(unique(outbreak_ts[outbreak_ts$location_period_id==1123,]$threshold), 100)
})

#when the fixed outbreak threshold is smaller than 0 which is invalid
test_that("get_outbreak_threshold works", {
  outbreak_ts$threshold=NA
  testthat::expect_error(
    get_outbreak_threshold (
      threshold_type = c("fixed threshold"),
      fixed_outbreak_threshold = -10,
      country = "NGA",
      shp = total_shp%>%subset(lctn_pr==1123),
      surveillance_data = outbreak_ts%>%subset(location_period_id==1123), #daily or weekly cholera case data by location period
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
      country = "NGA",
      shp = total_shp%>%subset(lctn_pr==1123),
      surveillance_data = outbreak_ts%>%subset(location_period_id==1123), #daily or weekly cholera case data by location period
      zero_case_assumption = TRUE
    ) 
  )
})

#test mean weekly incidence method
