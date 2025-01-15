#' @export
#' @title observation_aggregator
#' @name observation_aggregator
#' @description aggregate daily cholera data to weekly cholera data
observation_aggregator <- function (
    daily_outbreak_data
){
  
  #average duplicated daily data
    nodup_daily_outbreak_data <- daily_outbreak_data %>% 
      group_by_at(vars(-sCh, -cCh, -deaths)) %>% 
      dplyr::mutate(dplyr::across(c(sCh, cCh, deaths), ~ mean(.x, na.rm = TRUE))) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(dplyr::across(c(sCh, cCh, deaths), ~ ifelse(is.nan(.x), NA, .x))) %>% 
      dplyr::distinct() 
  
  #aggregate daily data
  aggregated_weekly_outbreak_data = nodup_daily_outbreak_data %>% 
    dplyr::arrange(TL) %>%
    dplyr::group_by(epiweek, location) %>%
    dplyr::mutate(
      TL = min(TL),
      TR = min(TL)+6,
      sCh = sum(sCh, na.rm = T),
      deaths = sum(deaths, na.rm = T),
      cCh = sum(cCh, na.rm = T),
      days = dplyr::n()
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      sCh = as.numeric(sCh),
      cCh = as.numeric(cCh),
      deaths = as.numeric(deaths),
      date_range = TR-TL+1,
      temporal_scale = "weekly"
    ) %>%
    dplyr::ungroup()

  if(!nrow(aggregated_weekly_outbreak_data) == 0){
    aggregated_weekly_outbreak_data <- aggregated_weekly_outbreak_data %>%
      dplyr::select(!c(days))
    return(aggregated_weekly_outbreak_data)
  } else {
    return(NULL)
  }
}
