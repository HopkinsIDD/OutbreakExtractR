test_that("get_uniform_wday_startdate works", {
  
  reftab <- tibble::tibble(
        daynum = lubridate::wday(1:7, week_start=1), 
        daylabel = lubridate::wday(1:7, label = TRUE, week_start=1))

  x = as.Date("2023-09-29")
  weekstart = 2

  ## the date returned should be the same day of the week as the number in the reference table
  testthat::expect_equal(as.character(lubridate::wday(get_uniform_wday_startdate(x, weekstart), label = TRUE)), 
                        as.character(reftab[reftab$daynum == weekstart,]$daylabel))

})

