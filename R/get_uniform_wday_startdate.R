#' @export
#' @title get_uniform_wday_startdate
#' @name get_uniform_wday_startdate
#' @description Returns the week's starting date for the date that is provided
#' @param x date
#' @param wstart number representing the day of the week that should be considered the week start (1 = Monday)
get_uniform_wday_startdate <- function(
    x,
    wstart
){
    x - (lubridate::wday(x, week_start = wstart) - 1)
}

