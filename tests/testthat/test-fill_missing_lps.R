testthat::test_that("fill_missing_lps works", {
    ## test that nothing changes when no lps are available
    test <- data.frame(location = c("A", "A"), location_period_id = c(NA, NA), TR = as.Date("2023-02-01"))
    testthat::expect_message(fill_missing_lps(test), regexp = "0 empty location periods were successfully filled in.")
    testthat::expect_identical(fill_missing_lps(test), test)

    ## test that if the cloest observations with non NA location periods before and after the observation with NA location period id are different, most recent location period is the one that gets filled in
    test1 <- data.frame(location = c("A", "A", "A"), 
                        location_period_id = c("222", NA, "234"), 
                        TR = c(as.Date("2023-02-03"), as.Date("2023-02-02"), as.Date("2023-02-01")))
    testthat::expect_equal(fill_missing_lps(test1)[which(test1$TR == as.Date("2023-02-02")),]$location_period_id, "222")
    
    ## test that if the cloest observations with non NA location periods before and after the observation with NA location period id are the same, that unique location period is the one that gets filled in
    test2 <- data.frame(location = c("A", "A", "A"), location_period_id = c("222", NA, "222"), TR = c(as.Date("2023-02-01"), as.Date("2023-02-02"), as.Date("2023-02-03")))
    testthat::expect_equal(fill_missing_lps(test2)[which(test2$TR == as.Date("2023-02-02")),]$location_period_id, "222")

    ## test that no changes are made if all lps are available
    test3 <- data.frame(location = c("A", "A"), location_period_id = c("22", "12"), TR = c(as.Date("2023-02-01"), as.Date("2023-02-02")))
    testthat::expect_identical(fill_missing_lps(test3), test3)

})