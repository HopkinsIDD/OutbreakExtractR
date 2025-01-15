#' @export
#' @title trigger_alert_caseratio
#' @name trigger_alert_caseratio
#' @description Triggers alerts according to patterns in excess suspected cases relative to a historical 4-week case threshold. Returns a dataframe with alert indicators aligned with the pre-alert timeseries extractions across multiple locations. Alert is TRUE for the week when the alert would have been identified. Three alerts are currently identified: 1) alert week always exceeds mean of last 4 weeks of cases, thus indicating an increase in cases relative to the past month; 2) exceeds mean of last 4 weeks of cases in alert week and at least one other time within the past month; 3) exceeds mean of last 4 weeks of cases in alert week and at least two other times within the past month
#' @param original_data dataframe of pre-alert time series extractions
#' @return dataframe
trigger_alert_caseratio <- function(original_data){
  
  tmp_function <- function(df_original, loc){
    df_tmp <- dplyr::filter(df_original, location == loc)
    cases_seq <- df_tmp$sCh
    
    ## pad NAs at the end to keep the same length as cases_seq
    mean4weeks_tmp <- zoo::rollapply(cases_seq, width = 4, FUN = mean, align = "right", fill = NA)
    # shift index so that 4 week mean is evaluated against the following week
    mean4weeks <- c(NA, mean4weeks_tmp[1:length(mean4weeks_tmp)-1]) 
    
    ## alert1: alert week always exceeds mean of last 4 weeks of cases, thus indicating an increase in cases relative to the past month
    ## alert1cum_4wk: count cumulative number of alert1s in the past month
    a1 <- dplyr::mutate(df_tmp, 
                  thresh_4wk = dplyr::if_else(mean4weeks<0, 0, mean4weeks),
                  alert1 = dplyr::if_else(sCh>thresh_4wk, TRUE, FALSE))
    ## alert2: exceeds mean of last 4 weeks of cases in alert week and at least one other time within the past month
    ## alert3: exceeds mean of last 4 weeks of cases in alert week and at least two other times within the past month
    a3 <- dplyr::mutate(a1,
                  alert1cum_4wk = zoo::rollapply(a1$alert1, width = 4, FUN = sum, align = "right", fill = NA),
                  alert2 = dplyr::if_else(alert1 & alert1cum_4wk >= 2, TRUE, FALSE),
                  alert3 = dplyr::if_else(alert1 & alert1cum_4wk >= 3, TRUE, FALSE)) 
    ## alert3consec: cumulative number of alert3 weeks within the past 12 weeks
    dplyr::mutate(a3, 
                  alert3sum_12wk = zoo::rollapply(a3$alert3, width = 12, FUN = sum, align = "right", fill = NA, partial = TRUE))%>%
      dplyr::relocate("alert1", "alert2", "alert3", .after = tidyselect::last_col())
    
  }
  
  tmp2 <- purrr::map(
    .x = unique(original_data$location),
    \(x) tmp_function(original_data, x)
  ) %>%
    purrr::list_rbind()
  
  return(tmp2)  
  
}

