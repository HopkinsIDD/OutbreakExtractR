#' @export
#' @title observation_filter
#' @name observation_filter
#' @description filter observations base on certain criteria
observation_filter <- function(
    outbreak_data,
    time_lower_bound_filter = lubridate::ymd("2000-01-01"),
    time_upper_bound_filter = lubridate::ymd("2023-12-31"),
    temporal_scale_filter = c("daily","weekly"),
    who_regions = c("AFR","EMR","AMR"),
    spatial_scale_filter = c("country","admin1","admin2","admin3"),
    remove_na_sCh = TRUE,
    remove_na_cCh = FALSE,
    remove_na_locationperiod = FALSE,
    minimum_daily_cases = 0,...
){
  filtered_outbreak_data <- outbreak_data %>%
    subset(
      TL >= time_lower_bound_filter &
      TR <= time_upper_bound_filter &
      temporal_scale %in% temporal_scale_filter &
      who_region %in% who_regions &
      spatial_scale %in% spatial_scale_filter &
      (sCh >= minimum_daily_cases|is.na(sCh))
    )
  if(remove_na_sCh){
    filtered_outbreak_data <- filtered_outbreak_data %>%
      subset(is.na(sCh)==F)
  }
  if(remove_na_cCh){
    filtered_outbreak_data <- filtered_outbreak_data %>%
      subset(is.na(cCh)==F)
  }
  if(remove_na_locationperiod){
    filtered_outbreak_data <- filtered_outbreak_data %>%
      subset(!is.na(location_period_id) & location_period_id != "")
  }
  return(filtered_outbreak_data)
}
