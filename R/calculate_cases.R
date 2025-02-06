#' Title calculate_cases
#' @name calculate_cases
#' @description
#' Calculates the number of cases in the evaluation period for each alert group
#' @param alert_group_df df with alert groups identified by their alert_id (one per row), location, date of first alert, date of last alert, alert number, alert type
#' @param preoutbreak_ts preoutbreak time series
#' @param delay the delay after the start of the alert group that will begin the evaluation period
#' @param evaluation_duration the duration of the evaluation period
#' @return the alert group df with the alert_id, location, evaluation period start, evaluation period end, and total number of suspected cases during the evaluation period
#' @export
calculate_cases <- function(alert_group_df, preoutbreak_ts, delay_period, evaluation_duration) {
  
  results <- alert_group_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      evaluation_start_date = TL_first_alert + delay_period, ## the date of the first alert in the alert group plus delay period
      evaluation_end_date = evaluation_start_date + evaluation_duration 
    ) %>%
    dplyr::group_by(alert_id, location, evaluation_start_date, evaluation_end_date) %>%
    dplyr::summarise(
      total_sCh = sum(preoutbreak_ts$sCh[
        preoutbreak_ts$location == location & 
          preoutbreak_ts$TL >= evaluation_start_date & 
          preoutbreak_ts$TL <= evaluation_end_date
      ], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(results)
}