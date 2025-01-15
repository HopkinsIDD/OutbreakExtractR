testthat::test_that("fill_phantom_zeroes works", {
  ## test that the 8 weeks before and 8 weeks with 0s cases are added
  test <- data.frame(location = c("A", "A"), 
                     location_period_id = c(12, 23), 
                     TR = c(as.Date("2024-10-27"),as.Date("2024-11-01")),
                     TL = c(as.Date("2024-10-21"),as.Date("2024-10-28")),
                     start_weekday = "Monday",
                     sCh = c(167,140),
                     date_range = 7,
                     spatial_scale = "admin1", composite_loc = FALSE, who_region = "AFR", country = "C", admin1 = "A", admin2 = NA, admin3 = NA, admin4 = NA, admin5 = NA, 
                     cCh = NA, deaths = NA, epiweek = c("2024-01","2024-02"))
  testthat::expect_equal(nrow(fill_phantom_zeroes(test)),18)
  testthat::expect_equal(sum(fill_phantom_zeroes(test)$sCh), 307)
  testthat::expect_equal(min(fill_phantom_zeroes(test)$TL), as.Date("2024-08-26"))
  testthat::expect_equal(max(fill_phantom_zeroes(test)$TL), as.Date("2024-12-23"))
  testthat::expect_true(all(fill_phantom_zeroes(test)[!fill_phantom_zeroes(test)$TL %in% test$TL,]$phantom))
  testthat::expect_true(all(fill_phantom_zeroes(test)[!fill_phantom_zeroes(test)$TL %in% test$TL,]$sCh == 0))
  testthat::expect_true(all(is.na(fill_phantom_zeroes(test)[!fill_phantom_zeroes(test)$TL %in% test$TL,]$location_period_id)))
  
  ## test the weeks with missing observations are filled with 0s observations
  test1 <- data.frame(location = c("A", "A"), 
                     location_period_id = c(12, 23), 
                     TR = c(as.Date("2024-10-27"),as.Date("2024-11-17")),
                     TL = c(as.Date("2024-10-21"),as.Date("2024-11-11")),
                     start_weekday = "Monday",
                     sCh = c(167,140),
                     date_range = 7,
                     spatial_scale = "admin1", composite_loc = FALSE, who_region = "AFR", country = "C", admin1 = "A", admin2 = NA, admin3 = NA, admin4 = NA, admin5 = NA, 
                     cCh = NA, deaths = NA, epiweek = c("2024-01","2024-04"))
  testthat::expect_equal(nrow(fill_phantom_zeroes(test1)),20)
  testthat::expect_equal(sum(fill_phantom_zeroes(test1)$sCh), 307)
  testthat::expect_equal(min(fill_phantom_zeroes(test1)$TL), as.Date("2024-08-26"))
  testthat::expect_equal(max(fill_phantom_zeroes(test1)$TL), as.Date("2025-01-06"))  
  testthat::expect_true(all(fill_phantom_zeroes(test1)[!fill_phantom_zeroes(test1)$TL %in% test1$TL,]$phantom))
  testthat::expect_true(all(fill_phantom_zeroes(test1)[!fill_phantom_zeroes(test1)$TL %in% test1$TL,]$sCh == 0))
  testthat::expect_true(all(is.na(fill_phantom_zeroes(test1)[!fill_phantom_zeroes(test1)$TL %in% test1$TL,]$location_period_id)))
  
})