#' Title format_alerts
#' @name format_alerts
#' @param alerts_df 
#' @description Creates a 'long' version of the alerts df (one row per alert) and removes redundant information (this info is in the preoutbreak time series file)
#' @return a 'long' version of the alerts dataframe with columns for the alert_id, location, TL, spatial scale, country, alert number (numeric), and alert type
#' @export
format_alerts <- function(alerts_df){
  alert_id_columns <- paste0("alert", 0:17, "_id")
  alerts_df <- tidyr::pivot_longer(alerts_df, cols = alert_id_columns,
                                   names_to = "alert_column",
                                   values_to = "unique_alert_id") %>%
    dplyr::filter(!is.na(unique_alert_id)) %>%
    dplyr::mutate(alert_number = as.numeric(stringr::str_extract(unique_alert_id, "\\d+(?=_)"))) %>%
    dplyr::select(unique_alert_id, location, TL, spatial_scale, country, alert_number) %>%
    dplyr::mutate(alert_type = dplyr::case_when(alert_number %in% c(1:3) ~ "trend",
                                                alert_number %in% c(4:10) ~ "case",
                                                alert_number %in% c(11:17) ~ "cumsum",
                                                alert_number == 0 ~ "alert 0"))
  
  return(alerts_df)
}