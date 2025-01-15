#' Title calculate_population_density
#' @name calculate_population_density
#' @param time_series time series data frame for each location/week inc. location period column
#' @param outbreak_shp sf dataframe with shapefiles per location period
#' @return time series data frame with added area per 1km^2 and population density per 1km^2 columns
#' @export
#' @import magrittr
calculate_population_density <- function(time_series, outbreak_shp){
  
  ## create table with shapefiles with join
  
  time_series_w_shapefiles <- dplyr::left_join(
    time_series,
    outbreak_shp,
    by = c("location_period_id" = "lctn_pr")
  )
  
  ## calculate population density
  time_series_w_shapefiles <- time_series_w_shapefiles %>%
    dplyr::mutate(pop_density = pop/area_per_1km2) %>%
    dplyr::select(-geometry)
  
  return(time_series_w_shapefiles)
}  

