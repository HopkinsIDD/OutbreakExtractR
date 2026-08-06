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

# Composite locations ("|"-joined names) that only ever appear as non-primary
# must be retained; non-composite non-primary rows are still dropped.
test_that("clean_psql_data retains non-primary composite locations", {
  df <- data.frame(
    TL = c("2018-01-01", "2018-01-08", "2018-01-01", "2018-01-01"),
    TR = c("2018-01-07", "2018-01-14", "2018-01-07", "2018-01-07"),
    sCh = c(5, 3, 10, 7),
    cCh = c(NA, NA, NA, NA),
    deaths = c(0, 0, 0, 0),
    location_period_id = c(NA, NA, 100, 101),
    primary = c("f", "f", "f", "t"),
    phantom = c(FALSE, FALSE, FALSE, FALSE),
    location = c(
      "AFR::SEN::Saint-Louis::Dagana::Mbane|Ross-Bethio",  # composite, non-primary -> keep
      "AFR::SEN::Saint-Louis::Dagana::Mbane|Ross-Bethio",  # composite, non-primary -> keep
      "AFR::SEN::Saint-Louis::Podor",                       # atomic, non-primary   -> drop
      "AFR::SEN::Saint-Louis::Dagana"                       # atomic, primary       -> keep
    ),
    observation_collection_id = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  cleaned <- clean_psql_data(original_data = df)
  testthat::expect_equal(sum(cleaned$composite_loc), 2)
  testthat::expect_true(all(grepl("\\|", cleaned$location[cleaned$composite_loc])))
  testthat::expect_false(any(cleaned$location == "AFR::SEN::Saint-Louis::Podor"))
  testthat::expect_true(any(cleaned$location == "AFR::SEN::Saint-Louis::Dagana"))
})

# A composite that also appears as primary keeps ONLY its primary rows
# (no double-counting of the non-primary duplicate).
test_that("clean_psql_data does not duplicate composites that have a primary version", {
  df <- data.frame(
    TL = c("2018-01-01", "2018-01-01"),
    TR = c("2018-01-07", "2018-01-07"),
    sCh = c(5, 5),
    cCh = c(NA, NA),
    deaths = c(0, 0),
    location_period_id = c(NA, 200),
    primary = c("f", "t"),
    phantom = c(FALSE, FALSE),
    location = c(
      "AFR::BDI::Cankuzo::Cankuzo|Cendajuru",
      "AFR::BDI::Cankuzo::Cankuzo|Cendajuru"
    ),
    observation_collection_id = c("a", "b"),
    stringsAsFactors = FALSE
  )
  cleaned <- clean_psql_data(original_data = df)
  testthat::expect_equal(nrow(cleaned), 1)
  testthat::expect_true(all(cleaned$composite_loc))
})
