#' Title create_alert_groups2
#' @name create_alert_groups2
#' @param alerts a df with formatted alerts
#' @param nweeks the number of weeks where an alert of a specific number has not been triggered so that this alert is considered postprocessed (starts a new alert group)
#' @return the alert group df, each row is a unique alert group. Includes the alert_id of the first alert in the group, the TL of the first and last alert in the group, the alert number and alert type
#' @export
create_alert_groups2 <- function(alerts, nweeks){
  
  ## call define_postprocessed_alerts with nweeks argument
  alerts_postprocessed <- OutbreakExtractR::define_postprocessed_alerts(alerts, nweeks)
  
  ## identify groups of consecutive non-postprocessed alerts
  alert_groups <- alerts_postprocessed %>% 
    dplyr::group_by(location, alert_number) %>% 
    dplyr::mutate(group = cumsum(postprocessed))
  
  ## count the number of alerts in each group
  alert_groups <- alert_groups %>% 
    dplyr::group_by(location, alert_number, group) %>% 
    dplyr::mutate(alert_count = n()) %>% 
    dplyr::ungroup()
  
  ## find the last non-postprocessed date in each group
  alert_groups <- alert_groups %>% 
    dplyr::group_by(location, alert_number, group) %>% 
    dplyr::mutate(reference_date = dplyr::if_else(postprocessed, last(TL[!postprocessed]), NA_Date_)) %>%
    dplyr::ungroup() %>%
    
    ## the grouping column is no longer useful so it is removed
    dplyr::select(-group) %>%
    
    ## if the postprocessed alert is the only alert in the group the first and last alert in the group fall on the same date
    dplyr::mutate(reference_date = dplyr::if_else(postprocessed & is.na(reference_date), TL, reference_date)) %>%
    
    ## keep only rows with postprocessed alerts - each row represents a unique alert group
    dplyr::filter(postprocessed) %>%  
    dplyr::rename(TL_first_alert = TL, TL_last_alert = reference_date) %>%
    dplyr::select(alert_id, location, TL_first_alert, TL_last_alert, alert_number, alert_type, alert_count)
  
  return(alert_groups)
}