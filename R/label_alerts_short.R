#' Title label_alerts_short
#' @name label_alerts_short
#' @description function that adds short descriptive labels to alert numbers
#' @param basedf a df with a column for alert numbers
#' @param is_ordered logical with default value of FALSE
#' @return dataframe
#' @export
label_alerts_short <- function(basedf, is_ordered = FALSE){
  
  basedf %>%
    dplyr::mutate(alert_lab = factor(alert_number, levels = 1:17, labels = c(paste0(c("1", "2", "3"), "-week"), paste(c("\U2265 2", "\U2265 5", "\U2265 10", "\U2265 25", "\U2265 50", "\U2265 100", "\U2265 250"), "weekly"), paste(c("\U2265 5", "\U2265 10", "\U2265 25", "\U2265 50", "\U2265 100", "\U2265 500", "\U2265 1000"), "total")), ordered = is_ordered))
}