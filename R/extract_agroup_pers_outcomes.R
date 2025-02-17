#' @export
#' @title extract_agroup_pers_outcomes
#' @name extract_agroup_pers_outcomes
#' @description calculate persistence-related outcomes for alert groups, incl nweeks_overX_sCh (encoded in metric & value), total_weeks in the year TS, assuming <delay_days> delay to OCV implementation and <evalperiod_days> eval period. 
#' @param dat dataframe
#' @param alert_col character string of column name that contains unique alert id
#' @param alert_group_col character string of column name that contains alert group id
#' @param delay_days start evaluation period <delay_days> after alert is triggered
#' @param evalperiod_days evaluation period duration in days
#' @param nweek_thresh number of weeks with X sCh cases in the next year above which it's useful to intervene with a campaign
#' @param out_col outcome ID
extract_agroup_pers_outcomes <- function(dat, alert_col, alert_group_col, delay_days, evalperiod_days, nweek_thresh, out_settings){
  
  ## check if alert_col and alert_group_col exist in the dataset
  if (!(alert_col %in% colnames(dat))) {
    warning(paste("Column", alert_col, "does not exist in the dataset. Skipping.")) 
    return(tibble()) ## return an empty tibble if the column is missing
  }
  if (!(alert_group_col %in% colnames(dat))) {
    warning(paste("Column", alert_group_col, "does not exist in the dataset. Skipping."))
    return(tibble()) ## return an empty tibble if the column is missing
  }
  
  renamed <- dplyr::rename(dat, tmp_id = tidyselect::all_of(alert_col), tmp_group_id = tidyselect::all_of(alert_group_col))
  unique_alert_ids <- unique(renamed$tmp_id)[which(!is.na(unique(renamed$tmp_id)))]
  
  tmp_function <- function(df, uq_alert_id, set_delay, set_evalperiod){
    tryCatch({
      alert_row <- dplyr::filter(df, tmp_id == uq_alert_id)
      
      if (nrow(alert_row) != 1 || any(is.na(alert_row$location), is.na(alert_row$TL), is.na(alert_row$epiweek))) {
        warning(paste("Skipping alert ID", uq_alert_id, "due to missing or invalid data."))
        return(tibble())
      }
      
      alert_row_loc <- alert_row$location[1]
      alert_row_TL <- alert_row$TL[1]
      alert_row_epiweek <- alert_row$epiweek[1]
      alert_row_group_id <- alert_row$tmp_group_id[1]  # Capture the group ID
      
      ## filter evaluation period data
      outcome_period_ts <- df %>%
        dplyr::filter(location == alert_row_loc & 
                        TL >= alert_row_TL + set_delay & 
                        TL < alert_row_TL + set_delay + set_evalperiod) 
      
      ## sum outcomes for location during evaluation period
      rc <- outcome_period_ts %>%
        dplyr::arrange(location, TL) %>%
        dplyr::mutate(over0_sCh = dplyr::if_else(sCh > 0, 1, 0),
                      over5_sCh = dplyr::if_else(sCh >= 5, 1, 0),
                      over10_sCh = dplyr::if_else(sCh >= 10, 1, 0),
                      over25_sCh = dplyr::if_else(sCh >= 25, 1, 0),
                      over50_sCh = dplyr::if_else(sCh >= 50, 1, 0),
                      over100_sCh = dplyr::if_else(sCh >= 100, 1, 0)) %>%
        dplyr::group_by(location) %>%
        dplyr::summarise(total_weeks = n(),
                         nweeks_over0_sCh = sum(over0_sCh),
                         nweeks_over5_sCh = sum(over5_sCh),
                         nweeks_over10_sCh = sum(over10_sCh),
                         nweeks_over25_sCh = sum(over25_sCh),
                         nweeks_over50_sCh = sum(over50_sCh),
                         nweeks_over100_sCh = sum(over100_sCh)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(tmp_id = uq_alert_id,
                      alert_date_TL = alert_row_TL,
                      alert_group_id = alert_row_group_id,  # Include group ID in results
                      alert_date_epiweek = alert_row_epiweek)
      return(rc)
    }, error = function(e) {
      warning(paste("Error processing alert ID", uq_alert_id, ":", e$message))
      return(tibble()) ## return an empty tibble in case of error
    })
  }
  
  summary_dat <- purrr::map(
    .x = unique_alert_ids,
    \(x) tmp_function(renamed, x, delay_days, evalperiod_days)
  ) %>%
    purrr::list_rbind() %>%
    dplyr::rename(alert_id := tmp_id) %>%
    dplyr::select(alert_id, alert_date_TL, total_weeks, nweeks_over0_sCh, nweeks_over5_sCh, nweeks_over10_sCh, nweeks_over25_sCh, nweeks_over50_sCh, nweeks_over100_sCh, location, alert_date_epiweek, alert_group_id) %>%
    tidyr::pivot_longer(cols = contains("nweeks_over"), names_to = "metric", values_to = "value") %>%
    dplyr::mutate(nweek_thresh = nweek_thresh,
                  outcome = dplyr::if_else(value >= nweek_thresh, TRUE, FALSE)) %>%
    dplyr::left_join(out_settings, by = c("metric" = "case_metric", "nweek_thresh")) %>%
    dplyr::ungroup()
  
  ## consolidate outcomes by alert group id
  ## CA Jan 28: Modification to keep track of first alert_id that has a TRUE outcome in the alert group
  group_summary <- summary_dat %>%
    dplyr::group_by(alert_group_id, out_col) %>%
    dplyr::summarise(
      group_outcome = any(outcome == TRUE),
      earliest_true_alert_id = if (any(outcome == TRUE)) {
        alert_id[outcome == TRUE][1]
      } else {
        NA ## if the group outcome is FALSE
      },
      .groups = 'drop'
    )
  ## end modification
  
  final_summary <-
    dplyr::left_join(summary_dat, group_summary, by = c("alert_group_id", "out_col"))
  
  return(final_summary)
}
