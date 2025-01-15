#' @export
#' @title extract_agroup_case_outcomes
#' @name extract_agroup_case_outcomes
#' @description calculate case-related outcomes for alert groups, incl sCh, cCh, deaths, and count number of weeks with non-zero sCh for each location, assuming <delay_days> delay to OCV implementation and <evalperiod_days> eval period. 
#' @param dat dataframe
#' @param alert_col character string of column name that contains unique alert id
#' @param alert_group_col character string of column name that contains alert group id
#' @param delay_days start evaluation period <delay_days> after alert is triggered
#' @param evalperiod_days evaluation period duration in days
#' @param intervene_thresh number of sCh above which it's useful to intervene with a campaign
#' @param out_col outcome ID
extract_agroup_case_outcomes <- function(dat, alert_col, alert_group_col, delay_days, evalperiod_days, intervene_thresh, out_col) {
  
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
  
  tmp_function <- function(df, uq_alert_id, set_delay, set_evalperiod) {
    tryCatch({
      alert_row <- dplyr::filter(df, tmp_id == uq_alert_id)
      
      if (nrow(alert_row) != 1 || any(is.na(alert_row$location), is.na(alert_row$TL), is.na(alert_row$epiweek))) {
        warning(paste("Skipping alert ID", uq_alert_id, "due to missing or invalid data."))
        return(tibble())
      }
      
      alert_row_loc <- alert_row$location[1]
      alert_row_TL <- alert_row$TL[1]
      alert_row_epiweek <- alert_row$epiweek[1]
      alert_row_group_id <- alert_row$tmp_group_id[1]
      
      outcome_period_ts <- df %>%
        dplyr::filter(location == alert_row_loc & 
                        TL >= alert_row_TL + set_delay & 
                        TL < alert_row_TL + set_delay + set_evalperiod)
      
      rc <- outcome_period_ts %>%
        dplyr::arrange(location, TL) %>%
        dplyr::mutate(non_zero_sCh = dplyr::if_else(sCh > 0, 1, 0),
                      cumsum_sCh = cumsum(sCh),
                      prop_cumsum_sCh = cumsum_sCh / max(cumsum_sCh), 
                      prop_cumsum_sCh = ifelse(is.nan(prop_cumsum_sCh), NA, prop_cumsum_sCh),
                      cumsum_25 = dplyr::if_else(prop_cumsum_sCh >= 0.25, TL, NA), 
                      cumsum_50 = dplyr::if_else(prop_cumsum_sCh >= 0.5, TL, NA),
                      cumsum_75 = dplyr::if_else(prop_cumsum_sCh >= 0.75, TL, NA)) %>%
        dplyr::group_by(location) %>%
        dplyr::summarise(sCh = sum(sCh, na.rm = TRUE),
                         cCh = sum(cCh, na.rm = TRUE),
                         deaths = sum(deaths, na.rm = TRUE),
                         cumsum25_TL = min(cumsum_25, na.rm = TRUE),
                         cumsum50_TL = min(cumsum_50, na.rm = TRUE),
                         cumsum75_TL = min(cumsum_75, na.rm = TRUE),
                         n_weeks_w_sCh = sum(non_zero_sCh)) %>%
        dplyr::mutate(tmp_id = uq_alert_id,
                      alert_date_TL = alert_row_TL,
                      alert_group_id = alert_row_group_id,
                      cumsum25_weeks_since_alert = as.numeric(difftime(cumsum25_TL, alert_date_TL, units = "weeks")),
                      cumsum50_weeks_since_alert = as.numeric(difftime(cumsum50_TL, alert_date_TL, units = "weeks")),
                      cumsum75_weeks_since_alert = as.numeric(difftime(cumsum75_TL, alert_date_TL, units = "weeks")),
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
    dplyr::select(alert_id, alert_date_TL, sCh, n_weeks_w_sCh, cumsum25_weeks_since_alert, cumsum50_weeks_since_alert, cumsum75_weeks_since_alert, cCh, deaths, location, alert_date_epiweek, alert_group_id) %>%
    dplyr::mutate(
      outcome_type = out_col,
      outcome = dplyr::if_else(sCh >= intervene_thresh, TRUE, FALSE))
  
  group_summary <- summary_dat %>%
    dplyr::group_by(alert_group_id) %>%
    dplyr::summarise(group_outcome = any(outcome == TRUE), .groups = 'drop')
  
  final_summary <- summary_dat %>%
    dplyr::left_join(group_summary, by = "alert_group_id")
  
  return(final_summary)
}
