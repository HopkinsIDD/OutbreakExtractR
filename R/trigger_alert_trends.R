#' @export
#' @title trigger_alert_trends
#' @name trigger_alert_trends
#' @description Triggers alerts according to patterns in suspected case trends. Returns a dataframe with alert indicators aligned with the pre-alert timeseries extractions across multiple locations. Alert is TRUE for the week when the alert would have been identified. One alert is currently identified: 1) 3 consecutive declining or constant weeks (observe 4 weeks total) followed by 2 consecutive increasing weeks (observe 3 weeks total) in the 2-week rolling mean of cases 
#' @param original_data dataframe of pre-alert time series extractions
#' @return dataframe
trigger_alert_trends <- function(original_data){
  
  tmp_function <- function(df_original, loc){
      df_tmp <- dplyr::filter(df_original, location == loc)
      cases_seq <- df_tmp$sCh

      #### alert1 ####
      ## calculate 2-week rolling mean to smooth over reporting issues and trickling transmission across weeks
      mean2 <- zoo::rollapply(cases_seq, width = 2, FUN = mean, align = "right", fill = NA)
      diff_mean2 <- c(NA, diff(mean2, lag = 1))
  
      ## washout period consists of declining or constant values in the difference of the rolling 2-week mean 
      ## constant weeks are included in the washout to account for endemic regions which may have continual regions
      poss_washout <- ifelse(!is.na(diff_mean2) & diff_mean2 <= 0 , TRUE, FALSE)
      sum3_poss_washout <- zoo::rollapply(poss_washout, width = 3, FUN = sum, align = "right", fill = NA)
      
      ## possible alerts are 2 consecutive increasing weeks in the 2-week rolling mean of cases (observe 3 weeks total)
      poss_alert <- ifelse(!is.na(diff_mean2) & cases_seq > 0 & diff_mean2 > 0, TRUE, FALSE)
      sum3_poss_alert <- zoo::rollapply(poss_alert, width = 3, FUN = sum, align = "left", fill = NA) 

      ## alert1:  3 consecutive declining or constant weeks (observe 4 weeks total) followed by 2 consecutive increasing weeks (observe 3 weeks total) in the 2-week rolling mean of cases 
      alert1_exists <- ifelse(sum3_poss_alert >= 2 & sum3_poss_washout == 3, TRUE, FALSE)
      alert1 <- rep(FALSE, length(alert1_exists))
      alert1[which(alert1_exists)+2] <- TRUE

      if(length(alert1) != length(cases_seq)){
        stop("The length of cases_seq does not match the length of any alert vector. Check trigger_alert_trends function.")
      }

      dplyr::mutate(df_tmp, alert1 = alert1)
  }

  tmp2 <- purrr::map(
    .x = unique(original_data$location),
    \(x) tmp_function(original_data, x)
    ) %>%
    purrr::list_rbind()

  return(tmp2)  
  
}

