#' @export
#' @title custom_paste
#' @name custom_paste
#' @description
#' This is an helper function to make sure observation_collection_id columns are pasted in order and only include a set of unique OC UIDs.
custom_paste <- function(x) {
  x <- as.character(unique(x))
  x <- x[x != ""]
  
  if (all(suppressWarnings(!is.na(as.numeric(x))))) {
    x <- sort(as.numeric(x))
  } else {
    x <- sort(x)
  }
  
  return(paste(x, collapse = ";"))
}

#' @export
#' @title observation_aggregator
#' @name observation_aggregator
#' @description aggregate daily cholera data to weekly cholera data
observation_aggregator <- function (
    daily_outbreak_data
){
  
  #average duplicated daily data
    nodup_daily_outbreak_data <- daily_outbreak_data %>% 
      group_by_at(vars(-sCh, -cCh, -deaths, -original_location_name, -observation_collection_id)) %>% 
      dplyr::mutate(dplyr::across(c(sCh, cCh, deaths), ~ mean(.x, na.rm = TRUE)),
                    original_location_name = custom_paste(original_location_name),
                    observation_collection_id = custom_paste(observation_collection_id)) %>% 
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
      days = dplyr::n(),
      original_location_name = custom_paste(original_location_name),
      observation_collection_id = custom_paste(observation_collection_id)
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      sCh = as.numeric(sCh),
      cCh = as.numeric(cCh),
      deaths = as.numeric(deaths),
      date_range = TR-TL+1,
      temporal_scale = "weekly",
      original_location_name = custom_paste(original_location_name),
      observation_collection_id = custom_paste(observation_collection_id)
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
