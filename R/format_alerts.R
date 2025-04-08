#' Title format_alerts
#' @name format_alerts
#' @param alerts_df 
#' @description Creates a 'long' version of the alerts df (one row per alert) and removes redundant information (this info can be found in the preoutbreak time series file)
#' @return a 'long' version of the alerts dataframe with columns for the alert_id, location, TL, spatial scale, country, alert number (numeric), and alert type
#' @export
format_alerts <- function(alerts_df){
  alert_id_columns <- paste0("alert", 1:18, "_id")
  alerts_df <- tidyr::pivot_longer(alerts_df, cols = alert_id_columns,
                                   names_to = "alert_column",
                                   values_to = "alert_id") %>%
    dplyr::filter(!is.na(alert_id)) %>%
    dplyr::mutate(alert_number = as.numeric(stringr::str_extract(alert_id, "\\d+(?=_)"))) %>%
    dplyr::select(alert_id, location, TL, spatial_scale, country, alert_number) %>%
    dplyr::mutate(alert_type = dplyr::case_when(alert_number %in% c(1:3) ~ "trend",
                                                alert_number %in% c(4:10) ~ "case",
                                                alert_number %in% c(11:17) ~ "cumsum",
                                                alert_number %in% c(18) ~ "incid"))
  
  return(alerts_df)
}