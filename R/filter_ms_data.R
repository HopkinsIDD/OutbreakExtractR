#' filter_ms_data
#'
#' Filters a dataframe of outbreak alerts by spatial scale, endemic status, and trend-based alerts.
#' This function removes country-level locations, optionally removes trend-based alerts,
#' and allows keeping only endemic, epidemic, or all locations. Endemic locations are determined
#' based on the package dataset `endemic_locs_gte50nz_3ydata`.
#'
#' @param df A dataframe containing alert data. Must include columns:
#'   - `location`: character, name of the location
#'   - `spatial_scale`: character, e.g., "country", "admin1", etc...
#'   - `alert_type`: character, e.g., "trend" or other alert types
#' @param which_setting Character. One of `"endemic"`, `"epidemic"`, or `"all"`.
#'   Controls which subset of locations to keep. Default is `"epidemic"`.
#' @param incl_trend_alerts Logical. If `TRUE`, trend-based alerts are included; 
#'   if `FALSE`, they are removed. Default is `TRUE`.
#' @return A filtered dataframe with subnational locations and the requested subset of endemic/epidemic/all.
#' @export
filter_ms_data <- function(df, 
                           which_setting = c("epidemic", "endemic", "all"), 
                           incl_trend_alerts = TRUE) {
  
  which_setting <- match.arg(which_setting)
  
  ## access endemic locations from package data
  endemic_locs <- OutbreakExtractR::endemic_locs_gte50nz_3ydata
  
  ## messages
  if (incl_trend_alerts) {
    message("Removing country-level locations")
    tmp <- df %>%
      dplyr::filter(spatial_scale != "country")
  } else {
    message("Removing country-level locations and trend-based alerts")
    tmp <- df %>%
      dplyr::filter(spatial_scale != "country", alert_type != "trend")
  }
  
  ## indicate endemic locations
  tmp <- tmp %>%
    dplyr::mutate(is_endemic = location %in% endemic_locs)
  
  ## keep epidemic, endemic, or all locations
  if (which_setting == "endemic") {
    tmp <- tmp %>% dplyr::filter(is_endemic)
  } else if (which_setting == "epidemic") {
    tmp <- tmp %>% dplyr::filter(!is_endemic)
  } else if (which_setting == "all") {
    tmp <- tmp
  } else {
    stop("which_setting parameter is not valid. Must be 'endemic', 'epidemic', or 'all'.")
  }
  
  ## remove temporary column
  filtered <- tmp %>% dplyr::select(-is_endemic)
  return(filtered)
}
