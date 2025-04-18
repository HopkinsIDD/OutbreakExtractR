#' @export
#' @title clean_psql_data
#' @name clean_psql_data
#' @description this function is used to clean the raw data pulled from psql database
#' @param original_data original dataset
clean_psql_data <- function(
    original_data,...
){
  
  library(tidyverse)

  # clean the location names (keep updated)
  # clean and add descriptive columns
  outbreak_data <- clean_location_names(original_data=original_data) %>%
    dplyr::mutate(
      sCh = as.numeric(sCh),
      cCh = as.numeric(cCh),
      deaths = as.numeric(deaths),
      TL = lubridate::ymd(TL),
      TR = lubridate::ymd(TR),
      primary = dplyr::case_when(
                  primary == "f" ~ FALSE,
                  primary == "t" ~ TRUE)
    ) %>% 
    dplyr::filter(primary) %>% ## always only keep primary data
    dplyr::mutate(
      date_range = TR-TL+1,
      temporal_scale = dplyr::case_when(
                  date_range == as.difftime(1, units = "days") ~ "daily",
                  date_range > as.difftime(1, units = "days") & date_range < as.difftime(7, units = "days") ~ "multiday",
                  date_range == as.difftime(7, units = "days") ~ "weekly",
                  date_range > as.difftime(7, units = "days") & date_range < as.difftime(28, units = "days") ~ "multiweek",
                  date_range %in% as.difftime(c(28, 29, 30, 31), units = "days") ~ "monthly",
                  date_range > as.difftime(31, units = "days") & date_range < as.difftime(365, units = "days") ~ "multimonth",
                  date_range %in% as.difftime(c(365, 366), units = "days") ~ "yearly",
                  date_range > as.difftime(366, units = "days") ~ "multiyear"),
      temporal_scale = factor(temporal_scale, levels = c("multiyear", "yearly", "multimonth", "monthly", 'multiweek', 'weekly', 'multiday', 'daily')),
      spatial_scale = dplyr::case_when(
                  stringr::str_count(location, pattern = "::") == 1 ~ "country",
                  stringr::str_count(location, pattern = "::") == 2 ~ "admin1",
                  stringr::str_count(location, pattern = "::") == 3 ~ "admin2",
                  stringr::str_count(location, pattern = "::") == 4 ~ "admin3",
                  stringr::str_count(location, pattern = "::") >= 5 ~ "admin4 or lower"),
      spatial_scale = factor(spatial_scale, levels = c("country", "admin1", "admin2", "admin3", "admin4 or lower")),
      start_weekday = lubridate::wday(TL, label = TRUE, abbr = FALSE),
      epiweek = OutbreakExtractR::get_epiweek(TL),
      composite_loc = dplyr::if_else(stringr::str_detect(location, "\\|"), TRUE, FALSE)
    ) %>%
    dplyr::select(location, TL, TR, sCh, cCh, deaths, spatial_scale, composite_loc, date_range, temporal_scale, start_weekday, epiweek, who_region, country, admin1, admin2, admin3, admin4, admin5, admin6, location_period_id,observation_collection_id,original_location_name)

  return(outbreak_data)
}
