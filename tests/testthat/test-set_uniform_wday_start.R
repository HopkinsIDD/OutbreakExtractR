testthat::test_that("set_uniform_wday_start and average_duplicate_observations work together", {
  
  reftab <- tibble::tibble(
        daynum = lubridate::wday(1:7, week_start=1), 
        daylabel = lubridate::wday(1:7, label = TRUE, week_start=1))

  fakedata <- tibble::tibble(
        location = rep("A", 4),
        TL = as.Date(c("2023-01-01", "2023-01-03", "2023-01-05", "2023-01-07")),
        TR = as.Date(c("2023-01-01", "2023-01-03", "2023-01-05", "2023-01-07"))+6,
        start_weekday = lubridate::wday(TL, label = TRUE, abbr = FALSE),
        epiweek = paste(as.character(lubridate::isoyear(TL)), 
                             dplyr::if_else(lubridate::isoweek(TL)<10, 
                                            paste0("0", lubridate::isoweek(TL)), 
                                            as.character(lubridate::isoweek(TL))), 
                             sep="-"),
        sCh = c(2, 10, 40, 20),
        cCh = c(0, 2, 1, 1),
        deaths = c(0, NA, 2, NA),
        original_location_name = rep("A",4),
        observation_collection_id = rep(1,4)
  )

  avgdata <- fakedata %>%
    average_duplicate_observations() ## averages observations with the same location-epiweek
  
  unifdata <- avgdata %>% 
    set_uniform_wday_start() %>% ## sets standard TL and TR and weekday start
    dplyr::mutate(status_ok = dplyr::if_else(as.numeric((max(TL)-min(TL))) %% 7 == 0, TRUE, FALSE))

  ## there should only be one observation per location-epiweek
  testthat::expect_equal(nrow(avgdata), nrow(dplyr::distinct(fakedata, location, epiweek)))

  ## all observations should be weekly
  testthat::expect_equal(sum(unifdata$status_ok), nrow(unifdata))

  ## start_weekday should be Monday by default
  testthat::expect_equal(as.character(unifdata$start_weekday), rep("Monday", length(unifdata$start_weekday)))

  ## start_weekday should be as specified in set_uniform_wday_start settings
  unifdata2 <- avgdata %>%
    set_uniform_wday_start(week_start = "Tue")
  testthat::expect_equal(as.character(unifdata2$start_weekday), rep("Tuesday", length(unifdata2$start_weekday)))

})

