#' Title label_alerttypes
#' @name label_alerttypes
#' @description function that converts alert types to an ordered factor with pretty labels
#' @param basedf a df with a column for alert_type
#' @return dataframe
#' @export
label_alerttypes <- function(basedf){
  
  basedf %>%
    dplyr::mutate(alert_type = factor(alert_type, levels = c("trend", "case", "cumsum", "rate"), labels = c("trend", "case", "cum case", "rate"), ordered = TRUE))
}