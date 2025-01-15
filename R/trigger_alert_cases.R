#' @export
#' @title trigger_alert_cases
#' @name trigger_alert_cases
#' @description Triggers alerts according to patterns in suspected cases. Returns a dataframe with alert indicators aligned with the pre-alert timeseries extractions across multiple locations. Alert is TRUE for the week when the alert would have been identified. Three alerts are currently identified: 1) four 0s followed by 2 consecutive weeks with at least 2 sCh each; 2) four 0s followed by a week with at least 2 sCh and at least 10 sCh in the 5-week period but one of the weeks reported 0 cases; 3) <4 sCh in 4 consecutive weeks followed by 2 consecutive weeks with at least 2 sCh each
#' @param original_data dataframe of pre-alert time series extractions
#' @return dataframe
trigger_alert_cases <- function(original_data){
  
  tmp_function <- function(df_original, loc){
      df_tmp <- dplyr::filter(df_original, location == loc)
      cases_seq <- df_tmp$sCh

      ## pad NAs at the end to keep the same length as cases_seq
      sum4weeks <- zoo::rollapply(cases_seq, width = 4, FUN = sum, align = "left", fill = NA)
      sum5weeks <- zoo::rollapply(cases_seq, width = 5, FUN = sum, align = "left", fill = NA)
      sum6weeks <- zoo::rollapply(cases_seq, width = 6, FUN = sum, align = "left", fill = NA)
      sum7weeks <- zoo::rollapply(cases_seq, width = 7, FUN = sum, align = "left", fill = NA)
      sum8weeks <- zoo::rollapply(cases_seq, width = 8, FUN = sum, align = "left", fill = NA)
      sum9weeks <- zoo::rollapply(cases_seq, width = 9, FUN = sum, align = "left", fill = NA)

      ## alert1: four 0s followed by 2 consecutive weeks with at least 2 sCh each
      alert1_exists <- ifelse(sum4weeks == 0 & sum5weeks >= 2 & sum6weeks >= 4 & sum6weeks > sum5weeks, TRUE, FALSE)
      alert1 <- rep(FALSE, length(alert1_exists))
      ## set alert to the week when the alert would be identified
      alert1[which(alert1_exists)+6] <- TRUE 
      
      ## alert2: four 0s followed by a week with at least 2 sCh and at least 10 sCh in the 5-week period but one of the weeks reported 0 cases 
      alert2_exists <- ifelse(sum4weeks == 0 & sum5weeks >= 2 & sum9weeks >= 10 & 
                              sum9weeks > sum5weeks & sum5weeks == sum6weeks, TRUE, FALSE)
      alert2_shift <- dplyr::case_when(
                                alert2_exists & sum7weeks > sum5weeks ~ 6,
                                alert2_exists & sum8weeks > sum5weeks ~ 7,
                                alert2_exists & sum9weeks > sum5weeks ~ 8,
                                .default = 0)
      
      if(length(which(alert2_exists)) != length(which(alert2_shift>0))){
        stop("alert2_exists and alert2_shift have mismatched lengths. Review trigger_alert_cases")
      }
      alert2 <- rep(FALSE, length(alert2_exists))
      ## set alert to the week when the alert would be identified
      alert2[which(alert2_exists) + alert2_shift[which(alert2_shift>0)]] <- TRUE

      ## alert3: <4 sCh in 4 consecutive weeks followed by 2 consecutive weeks with at least 2 sCh each
      alert3_exists <- ifelse(sum4weeks < 4 & 
                              sum5weeks >= sum4weeks+2 & 
                              sum6weeks >= sum4weeks+4 & 
                              sum6weeks > sum5weeks & 
                              sum5weeks > sum4weeks, TRUE, FALSE)
      alert3 <- rep(FALSE, length(alert3_exists))
      ## set alert to the week when the alert would be identified
      alert3[which(alert3_exists)+5] <- TRUE

      if(length(alert1) != length(cases_seq) | length(alert2) != length(cases_seq) | length(alert3) != length(cases_seq)){
        stop("The length of cases_seq does not match the length of any alert vector. Check trigger_alert_cases function.")
      }

      dplyr::mutate(df_tmp, alert1 = alert1, alert2 = alert2, alert3 = alert3)
  }

  tmp2 <- purrr::map(
    .x = unique(original_data$location),
    \(x) tmp_function(original_data, x)
    ) %>%
    purrr::list_rbind()

  return(tmp2)  
  
}

