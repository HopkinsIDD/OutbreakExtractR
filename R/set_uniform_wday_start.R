#' @export
#' @title set_uniform_wday_start
#' @name set_uniform_wday_start
#' @description Change TL, TR and start_weekday to user-specified uniform day of the week
#' @param original_data dataframe with location, epiweek, TL, TR, sCh, cCh, deaths
#' @param wk_start day of the week that should be considered the week start (default: "Mon")
set_uniform_wday_start <- function(
    original_data,
    week_start = "Mon", 
    ...
){
    reftab <- tibble::tibble(
        daynum = lubridate::wday(1:7, week_start=1), 
        daylabel = lubridate::wday(1:7, label = TRUE, week_start=1))
    wstart = reftab[reftab$daylabel == week_start,]$daynum

   original_data %>%
        dplyr::rename(TL_orig = TL, TR_orig = TR, start_weekday_orig = start_weekday) %>%
        dplyr::mutate(TL = get_uniform_wday_startdate(TL_orig, wstart),
                    TR = TL+6,
                    start_weekday = lubridate::wday(TL, label = TRUE, abbr = FALSE)) %>%
        dplyr::select(-c(TL_orig, TR_orig, start_weekday_orig))

}
