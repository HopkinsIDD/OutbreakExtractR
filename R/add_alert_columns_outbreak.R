#' Title add_alert_columns_outbreak
#' @name add_alert_columns_outbreak
#' @description Attach alert unique identifier columns to the linked / outbreak datasets
#' @param basedf 
#' @param alertdf 
#' @return dataframe
#' @export
add_alert_columns_outbreak <- function(basedf, alertdf){
  
  if("alert_type" %in% names(basedf)){
    basedf <- dplyr::select(basedf, -alert_type)
    message("dropping alert_type column from base dataset")
  } else{
    clean_alertdf <- dplyr::filter(alertdf, alert_id %in% unique(basedf$alert_id)) %>%
      dplyr::distinct(alert_number, alert_type)
    rc <- dplyr::left_join(basedf, clean_alertdf, by = c("alert_number"))
    
    message("adding alert_type columns ")
  }
  
  return(rc)
}