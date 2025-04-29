#' Title add_alert_columns
#' @name add_alert_columns
#' @description Attach alert unique identifier columns from the alert dataset matched by alert_id
#' @param basedf 
#' @param alertdf 
#' @return dataframe
#' @export
add_alert_columns <- function(basedf, alertdf){
  
  if("alert_number" %in% names(basedf)){
    basedf <- dplyr::select(basedf, -alert_number, -alert_type)
    message("dropping alert_number and alert_type columns from base dataset")
  }
  
  if(!("country" %in% names(basedf))){
    clean_alertdf <- dplyr::filter(alertdf, alert_id %in% unique(basedf$alert_id)) %>%
      dplyr::distinct(alert_id, country, alert_number, alert_type)
    rc <- dplyr::left_join(basedf, clean_alertdf, by = c("alert_id"))
    
    message("adding alert_number, alert_type, and country columns ")
  } else{
    clean_alertdf <- dplyr::filter(alertdf, alert_id %in% unique(basedf$alert_id)) %>%
      dplyr::distinct(alert_id, alert_number, alert_type)
    rc <- dplyr::left_join(basedf, clean_alertdf, by = c("alert_id"))
    
    message("adding alert_number and alert_type columns ")
  }
  
  return(rc)
}