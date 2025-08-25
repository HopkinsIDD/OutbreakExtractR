#' Title add_alert_stringency
#' @name add_alert_stringency
#' @description Attach numeric alert stringency value column (low = 1, high = 7), based on numeric alert_number 
#' @param basedf 
#' @return dataframe
#' @export
add_alert_stringency <- function(basedf){
  
  if(!("alert_number" %in% names(basedf))){
    stop("alert_number must be a column in the base dataset")
  }
  
  basedf %>%
    dplyr::mutate(alert_stringency = factor(dplyr::case_when(alert_number %in% c(4, 11, 18) ~ 1,
                                               alert_number %in% c(1, 5, 12, 19) ~ 2,
                                               alert_number %in% c(6, 13, 20) ~ 3,
                                               alert_number %in% c(2, 7, 14, 21) ~ 4,
                                               alert_number %in% c(8, 15, 22) ~ 5,
                                               alert_number %in% c(3, 9, 16, 23) ~ 6,
                                               alert_number %in% c(10, 17, 24) ~ 7), 
                                            # labels = c("1 (Low)", 2, 3, 4, 5, 6, "7 (High)"), 
                                            ordered = TRUE))
                                       
}