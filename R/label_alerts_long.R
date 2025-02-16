#' Title label_alerts_long
#' @name label_alerts_long
#' @description function that adds descriptive labels to alert numbers
#' @param basedf a df with a column for alert numbers
#' @return
#' @export
label_alerts_long <- function(basedf){
  
  basedf %>%
    dplyr::mutate(alert_lab = factor(alert_number, levels = 1:17, labels = c("1 week trend", "2 week trend", "3 week trend", "\U2265 2 weekly cases", "\U2265 5 weekly cases", "\U2265 10 weekly cases", "\U2265 25 weekly cases", "\U2265 50 weekly cases", "\U2265 100 weekly cases", "\U2265 250 weekly cases", "\U2265 5 cum cases", "\U2265 10 cum cases", "\U2265 25 cum cases", "\U2265 50 cum cases", "\U2265 100 cum cases", "\U2265 500 cum cases", "\U2265 1000 cum cases")))
}