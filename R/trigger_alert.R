#' @export
#' @title trigger_alert
#' @name trigger_alert
#' @description Triggers alerts according to patterns in suspected case trends. Returns a dataframe with alert indicators aligned with the pre-alert timeseries extractions across multiple locations. Alert is TRUE for the week when the alert would have been identified. Multiple alerts are currently identified, some of which were transferred from trigger_alert_caseratio and trigger_alert_cases. Alerts are triggered in the week AFTER the following patterns are identified: 1-3 TREND-BASED)  number of cases in 1/2/3 weeks in the past month exceeded the past 4-weeks rolling case mean, thus indicating an increase in cases relative to the past month; 4-10 CASE-BASED) at least 2/5/10/25/50/100/250 weekly suspected cases in 3 consecutive weeks; 11-17 CUM CASE-BASED) at least 5/10/25/50/100/500/1000 cumulative suspected cases in past 3 weeks. 
#' @param original_data dataframe of pre-alert time series extractions
#' @return dataframe

trigger_alert <- function(original_data){
  
  tmp_function <- function(df_original, loc){
    df_tmp <- dplyr::filter(df_original, location == loc)
    cases_seq <- df_tmp$sCh
    
    #### Upward trend-based alerts ####

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
    a_caseratio <- dplyr::mutate(a3, 
                  alert3sum_12wk = zoo::rollapply(a3$alert3, width = 12, FUN = sum, align = "right", fill = NA, partial = TRUE))%>%
      dplyr::relocate("alert1", "alert2", "alert3", .after = tidyselect::last_col())

    ## *ECL add processing code to suppress alerts that do not occur after a week that is below thresh_4wk (excluding the first alert)

    #### end excess case alerts ####
    
    #### alerts based on weekly cases ####

    ## pad NAs at the end to keep the same length as cases_seq

    sum1weeks <- zoo::rollapply(cases_seq, width = 1, FUN = sum, align = "left", fill = NA)
    sum2weeks <- zoo::rollapply(cases_seq, width = 2, FUN = sum, align = "left", fill = NA)
    sum3weeks <- zoo::rollapply(cases_seq, width = 3, FUN = sum, align = "left", fill = NA)
    
    ## alert0: At least one sCh in 1 week – expansive alert that captures all reported suspected cholera activity 
    
    alert0_exists <- ifelse(sum1weeks >= 1, TRUE, FALSE)
    alert0 <- rep(FALSE, length(alert0_exists))
    ## set alert to the week after pattern occurs
    alert0[which(alert0_exists)+1] <- TRUE
    
    ## alert4: 3 consecutive weeks with at least 2 sCh each

    alert4_exists <- ifelse(sum1weeks >= 2 & sum2weeks >= sum1weeks+2 & sum3weeks >= sum2weeks +2, TRUE, FALSE)
    alert4 <- rep(FALSE, length(alert4_exists))
    ## set alert to the week when the alert would be identified
    alert4[which(alert4_exists)+3] <- TRUE

    ## alert5: 3 consecutive weeks with at least 5 sCh each
    
    alert5_exists <- ifelse(sum1weeks >= 5 & sum2weeks >= sum1weeks+5 & sum3weeks >= sum2weeks+5, TRUE, FALSE)
    alert5 <- rep(FALSE, length(alert5_exists))
    ## set alert to the week after pattern occurs
    alert5[which(alert5_exists)+3] <- TRUE
    
    ## alert6: 3 consecutive weeks with at least 10 sCh each
    
    alert6_exists <- ifelse(sum1weeks >= 10 & sum2weeks >= sum1weeks+10 & sum3weeks >= sum2weeks+10, TRUE, FALSE)
    alert6 <- rep(FALSE, length(alert6_exists))
    ## set alert to the week after pattern occurs
    alert6[which(alert6_exists)+3] <- TRUE
    
    ## alert7: 3 consecutive weeks with at least 25 sCh each
    
    alert7_exists <- ifelse(sum1weeks >= 25 & sum2weeks >= sum1weeks+25 & sum3weeks >= sum2weeks+25, TRUE, FALSE)
    alert7 <- rep(FALSE, length(alert7_exists))
    ## set alert to the week after pattern occurs
    alert7[which(alert7_exists)+3] <- TRUE
    
    ## alert8: 3 consecutive weeks with at least 50 sCh each
    
    alert8_exists <- ifelse(sum1weeks >= 50 & sum2weeks >= sum1weeks+50 & sum3weeks >= sum2weeks+50, TRUE, FALSE)
    alert8 <- rep(FALSE, length(alert8_exists))
    ## set alert to the week after pattern occurs
    alert8[which(alert8_exists)+3] <- TRUE
    
    ## alert9: 3 consecutive weeks with at least 100 sCh each
    
    alert9_exists <- ifelse(sum1weeks >= 100 & sum2weeks >= sum1weeks+100 & sum3weeks >= sum2weeks+100, TRUE, FALSE)
    alert9 <- rep(FALSE, length(alert9_exists))
    ## set alert to the week after pattern occurs
    alert9[which(alert9_exists)+3] <- TRUE
    
    ## alert10: 3 consecutive weeks with at least 250 sCh each
    
    alert10_exists <- ifelse(sum1weeks >= 250 & sum2weeks >= sum1weeks+250 & sum3weeks >= sum2weeks+250, TRUE, FALSE)
    alert10 <- rep(FALSE, length(alert10_exists))
    ## set alert to the week after pattern occurs
    alert10[which(alert10_exists)+3] <- TRUE
    
    #### end weekly case alerts ####
    
    #### alerts based on cumulative cases ####
    ## alert11: at least 5 sCh cumulatively in past 3 weeks
    
    alert11_exists <- ifelse(sum3weeks >= 5, TRUE, FALSE)
    alert11 <- rep(FALSE, length(alert11_exists))
    ## set alert to the week after pattern occurs
    alert11[which(alert11_exists)+3] <- TRUE
    
    ## alert12: at least 10 sCh cumulatively in past 3 weeks
    
    alert12_exists <- ifelse(sum3weeks >= 10, TRUE, FALSE)
    alert12 <- rep(FALSE, length(alert12_exists))
    ## set alert to the week after pattern occurs
    alert12[which(alert12_exists)+3] <- TRUE
    
    ## alert13: at least 25 sCh cumulatively in past 3 weeks
    
    alert13_exists <- ifelse(sum3weeks >= 25, TRUE, FALSE)
    alert13 <- rep(FALSE, length(alert13_exists))
    ## set alert to the week after pattern occurs
    alert13[which(alert13_exists)+3] <- TRUE
    
    ## alert14: at least 50 sCh cumulatively in past 3 weeks
    
    alert14_exists <- ifelse(sum3weeks >= 50, TRUE, FALSE)
    alert14 <- rep(FALSE, length(alert14_exists))
    ## set alert to the week after pattern occurs
    alert14[which(alert14_exists)+3] <- TRUE
    
    ## alert15: at least 100 sCh cumulatively in past 3 weeks
    
    alert15_exists <- ifelse(sum3weeks >= 100, TRUE, FALSE)
    alert15 <- rep(FALSE, length(alert15_exists))
    ## set alert to the week after pattern occurs
    alert15[which(alert15_exists)+3] <- TRUE
    
    ## alert16: at least 500 sCh cumulatively in past 3 weeks
    
    alert16_exists <- ifelse(sum3weeks >= 500, TRUE, FALSE)
    alert16 <- rep(FALSE, length(alert16_exists))
    ## set alert to the week after pattern occurs
    alert16[which(alert16_exists)+3] <- TRUE
    
    ## alert17: at least 1000 sCh cumulatively in past 3 weeks
    
    alert17_exists <- ifelse(sum3weeks >= 1000, TRUE, FALSE)
    alert17 <- rep(FALSE, length(alert17_exists))
    ## set alert to the week after pattern occurs
    alert17[which(alert17_exists)+3] <- TRUE

    #### end cumulative case alerts ####
    
    rc <- dplyr::mutate(a_caseratio, 
                        alert0 = alert0, 
                        alert4 = alert4, alert5 = alert5, alert6 = alert6, alert7 = alert7, alert8 = alert8, alert9 = alert9, alert10 = alert10,
                        alert11 = alert11, alert12 = alert12, alert13 = alert13, alert14 = alert14, alert15 = alert15, alert16 = alert16, alert17) 
    
    return(rc)

  }
  
  tmp2 <- purrr::map(
    .x = unique(original_data$location),
    \(x) tmp_function(original_data, x)
  ) %>%
    purrr::list_rbind()
  
  return(tmp2)  
  
}