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
    
    ## weekly incidence per 10000 people
    df_tmp <- dplyr::mutate(df_tmp,
                            weekly_incidence = ifelse(pop > 0,
                                                      (sCh / pop) * 10000,
                                                      NA_real_))
    incidence_seq <- df_tmp$weekly_incidence  ## used for rate-based alerts
    
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
    
    ## CA Jan 21 2025 to ensure alerts 1-3 appear on the week after they would be identified - tested
    a3 <- dplyr::mutate(a3,
                        alert1 = dplyr::lag(alert1, n = 1, default = FALSE),
                        alert2 = dplyr::lag(alert2, n = 1, default = FALSE),
                        alert3 = dplyr::lag(alert3, n = 1, default = FALSE))
    
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
    
    #### rate alerts ####
    
    ## rolling incidence over 1, 2, and 3 weeks
    sumincid1weeks <- zoo::rollapply(incidence_seq, width = 1, FUN = sum, align = "left", fill = NA)
    sumincid2weeks <- zoo::rollapply(incidence_seq, width = 2, FUN = sum, align = "left", fill = NA)
    sumincid3weeks <- zoo::rollapply(incidence_seq, width = 3, FUN = sum, align = "left", fill = NA)
    
    ## alert 18: at least 0.25 per 10,000 population in 3 consecutive weeks
    alert18_exists <- ifelse(sumincid1weeks >= 0.25 & sumincid2weeks >= 0.25 * 2 & sumincid3weeks >= 0.25 * 3,TRUE, FALSE)
    alert18 <- rep(FALSE, length(alert18_exists))
    ## set alert to the week after pattern occurs
    alert18[which(alert18_exists) + 3] <- TRUE
    
    ## alert 19: at least 0.5 per 10,000 population in 3 consecutive weeks
    alert19_exists <- ifelse(sumincid1weeks >= 0.5 & sumincid2weeks >= 0.5 * 2 & sumincid3weeks >= 0.5 * 3,TRUE, FALSE)
    alert19 <- rep(FALSE, length(alert19_exists))
    ## set alert to the week after pattern occurs
    alert19[which(alert19_exists) + 3] <- TRUE
    
    ## alert 20: at least 1 per 10,000 population in 3 consecutive weeks
    alert20_exists <- ifelse(sumincid1weeks >= 1 & sumincid2weeks >= 1 * 2 & sumincid3weeks >= 1 * 3,TRUE, FALSE)
    alert20 <- rep(FALSE, length(alert20_exists))
    ## set alert to the week after pattern occurs
    alert20[which(alert20_exists) + 3] <- TRUE
    
    ## alert 21: at least 1.5 per 10,000 population in 3 consecutive weeks
    alert21_exists <- ifelse(sumincid1weeks >= 1.5 & sumincid2weeks >= 1.5 * 2 & sumincid3weeks >= 1.5 * 3,TRUE, FALSE)
    alert21 <- rep(FALSE, length(alert21_exists))
    ## set alert to the week after pattern occurs
    alert21[which(alert21_exists) + 3] <- TRUE
    
    ## alert 22: at least 2.5 per 10,000 population in 3 consecutive weeks
    alert22_exists <- ifelse(sumincid1weeks >= 2.5 & sumincid2weeks >= 2.5 * 2 & sumincid3weeks >= 2.5 * 3,TRUE, FALSE)
    alert22 <- rep(FALSE, length(alert22_exists))
    ## set alert to the week after pattern occurs
    alert22[which(alert22_exists) + 3] <- TRUE
    
    ## alert 23: at least 5 per 10,000 population in 3 consecutive weeks
    alert23_exists <- ifelse(sumincid1weeks >= 5 & sumincid2weeks >= 5 * 2 & sumincid3weeks >= 5 * 3,TRUE, FALSE)
    alert23 <- rep(FALSE, length(alert23_exists))
    ## set alert to the week after pattern occurs
    alert23[which(alert23_exists) + 3] <- TRUE
    
    ## alert 24: at least 7.5 per 10,000 population in 3 consecutive weeks
    alert24_exists <- ifelse(sumincid1weeks >= 7.5 & sumincid2weeks >= 7.5 * 2 & sumincid3weeks >= 7.5 * 3,TRUE, FALSE)
    alert24 <- rep(FALSE, length(alert24_exists))
    ## set alert to the week after pattern occurs
    alert24[which(alert24_exists) + 3] <- TRUE
    
    #### end rate alerts ####
    
    rc <- dplyr::mutate(a_caseratio, 
                        alert4 = alert4, alert5 = alert5, alert6 = alert6, alert7 = alert7, alert8 = alert8, alert9 = alert9, alert10 = alert10,
                        alert11 = alert11, alert12 = alert12, alert13 = alert13, alert14 = alert14, alert15 = alert15, alert16 = alert16, alert17,
                        alert18 = alert18, alert19 = alert19, alert20 = alert20, alert21 = alert21, alert22 = alert22, alert23 = alert23, alert24 = alert24) 
    
    
    
    return(rc)

  }
  
  tmp2 <- purrr::map(
    .x = unique(original_data$location),
    \(x) tmp_function(original_data, x)
  ) %>%
    purrr::list_rbind()
  
  return(tmp2)  
  
}