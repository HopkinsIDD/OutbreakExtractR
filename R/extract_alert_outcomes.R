#' @export
#' @title extract_alert_outcomes
#' @name extract_alert_outcomes
#' @description calculate alert outcomes sum sCh, cCh, deaths, and count number of weeks with non-zero sCh for each location, assuming <delay_days> delay to OCV implementation and <evalperiod_days> eval period
#' @param dat dataframe
#' @param alert_col character string of column name that contains unique alert id
#' @param delay_days start evaluation period <delay_days> after alert is triggered
#' @param evalperiod_days evaluation period duration in days
#' @param intervene_thresh number of sCh above which it's useful to intervene with a campaign
#' @param out_col outcome ID
extract_alert_outcomes <- function(dat, alert_col, delay_days, evalperiod_days, intervene_thresh, out_col){
    
    renamed <- dplyr::rename(dat, tmp_id = tidyselect::all_of(alert_col)) 
    unique_alert_ids <- unique(renamed$tmp_id)[which(!is.na(unique(renamed$tmp_id)))]

    # repeat tmp_function for each unique alert ID to process the outcomes
    tmp_function <- function(df, uq_alert_id, set_delay, set_evalperiod){
        alert_row <- dplyr::filter(df, tmp_id == uq_alert_id)
        
        if (nrow(alert_row) != 1){
          stop(paste(uq_alert_id, "does not have exactly 1 alert in extract_alert_outcomes. Please check."))
        } else{
          alert_row_loc <- alert_row$location[1]
          alert_row_TL <- alert_row$TL[1]
          alert_row_epiweek <- alert_row$epiweek[1]
          message(paste("Processing", uq_alert_id, alert_row_TL))
        }

        ## filter evaluation period data
        outcome_period_ts <- df %>%
            dplyr::filter(location == alert_row_loc & 
                          TL >= alert_row_TL + set_delay & 
                          TL < alert_row_TL + set_delay + set_evalperiod) 
        
        ## sum outcomes for location during evaluation period
        ## number of weeks to achieve 25%, 50%, 75% cumsum of cases
        rc <- outcome_period_ts %>%
            dplyr::arrange(location, TL) %>%
            dplyr::mutate(non_zero_sCh = dplyr::if_else(sCh > 0, 1, 0),
                          cumsum_sCh = cumsum(sCh),
                          prop_cumsum_sCh = cumsum_sCh/max(cumsum_sCh), 
                          prop_cumsum_sCh = ifelse(is.nan(prop_cumsum_sCh), NA, prop_cumsum_sCh),
                          cumsum_25 = dplyr::if_else(prop_cumsum_sCh >= 0.25, TL, NA), 
                          cumsum_50 = dplyr::if_else(prop_cumsum_sCh >= 0.5, TL, NA),
                          cumsum_75 = dplyr::if_else(prop_cumsum_sCh >= 0.75, TL, NA)) %>% ## Need to figure out how to handle false and min in summarize
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
                          cumsum25_weeks_since_alert = as.numeric(difftime(cumsum25_TL, alert_date_TL, units = "weeks")),
                          cumsum50_weeks_since_alert = as.numeric(difftime(cumsum50_TL, alert_date_TL, units = "weeks")),
                          cumsum75_weeks_since_alert = as.numeric(difftime(cumsum75_TL, alert_date_TL, units = "weeks")),
                          alert_date_epiweek = alert_row_epiweek)

        return(rc)
    }

    summary_dat <- purrr::map(
        .x = unique_alert_ids,
        \(x) tmp_function(renamed, x, delay_days, evalperiod_days)
        ) %>%
      purrr::list_rbind() %>%
      dplyr::rename(alert_id := tmp_id) %>%
      dplyr::select(alert_id, alert_date_TL, sCh, n_weeks_w_sCh, cumsum25_weeks_since_alert, cumsum50_weeks_since_alert, cumsum75_weeks_since_alert, cCh, deaths, location, alert_date_epiweek) %>%
      dplyr::mutate(
        out_type = out_col,
        outcome = dplyr::if_else(sCh >= intervene_thresh, TRUE, FALSE))

    return(summary_dat)

}