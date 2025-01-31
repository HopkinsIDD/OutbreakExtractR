#' Title define_postprocessed_alerts
#' @name define_postprocessed_alerts
#' @param alerts_df each row represents a unique alert 
#' @param nweeks the number of weeks where an alert of a specific number has not been triggered so that this alert is considered postprocessed (starts a new alert group)
#' @return alerts df with an added column (logical) indicating whether the alert is postprocessed (marks the start of a new alert group)
#' The new column is used to create alert groups with OutbreakExtractR::create_alert_groups2
#' @export
define_postprocessed_alerts <- function(alerts_df, nweeks){
  
  alerts_df_postprocessed <- alerts_df %>%
    dplyr::group_by(alert_number, location) %>% 
    dplyr::mutate(postprocessed = dplyr::if_else(is.na(lag(TL)) | (TL - lag(TL)) > lubridate::weeks(nweeks), TRUE, FALSE)) %>%
    dplyr::ungroup() 
  
  return(alerts_df_postprocessed)
}
