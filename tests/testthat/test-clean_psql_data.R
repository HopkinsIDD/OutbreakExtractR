#test if there are only primary observations
test_that("clean_psql_data works", {
  outbreak_ts<-read.csv("outbreak_testing_data.csv")
  clean_outbreak_ts<-clean_psql_data(original_data=outbreak_ts)
  testthat::expect_true(nrow(clean_outbreak_ts) == nrow(outbreak_ts[outbreak_ts$primary=="t",]))
})

#test if the temporal scale columns are factors
test_that("clean_psql_data works", {
  outbreak_ts<-read.csv("outbreak_testing_data.csv")
  clean_outbreak_ts<-clean_psql_data(original_data=outbreak_ts)
  testthat::expect_true(all(unique(clean_outbreak_ts$temporal_scale) %in% c("multiyear", "yearly", "multimonth", "monthly", 'multiweek', 'weekly', 'multiday', 'daily')))
  testthat::expect_true(all(!is.na(clean_outbreak_ts$temporal_scale)))
})

#test if the spatial scale columns are factors
test_that("clean_psql_data works", {
  outbreak_ts<-read.csv("outbreak_testing_data.csv")
  clean_outbreak_ts<-clean_psql_data(original_data=outbreak_ts)
  testthat::expect_true(all(unique(clean_outbreak_ts$spatial_scale) %in% c("country", "admin1", "admin2", "admin3", "admin4 or lower")))
  testthat::expect_true(all(!is.na(clean_outbreak_ts$spatial_scale)))
})
