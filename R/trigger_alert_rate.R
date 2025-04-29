#' @export
#' @title trigger_alert_rate
#' @name trigger_alert_rate
#' @description Triggers rate-based alerts 18-24 according to patterns in incidence rate trends per 10000 people
#' @param original_data dataframe of pre-alert time series extractions
#' @return dataframe

trigger_alert_rate <- function(original_data){
  
  tmp_function <- function(df_original, loc){
    df_tmp <- dplyr::filter(df_original, location == loc)

    ## weekly incidence per 10000 people
    df_tmp <- dplyr::mutate(df_tmp,
                            weekly_incidence = ifelse(pop > 0,
                                                      (sCh / pop) * 10000,
                                                      NA_real_))
    incidence_seq <- df_tmp$weekly_incidence  ## used for rate-based alerts
    
    #### rate-based alerts ####
    
    ## rolling incidence over 1, 2, and 3 weeks
    sumincid1weeks <- zoo::rollapply(incidence_seq, width = 1, FUN = sum, align = "left", fill = NA)
    sumincid2weeks <- zoo::rollapply(incidence_seq, width = 2, FUN = sum, align = "left", fill = NA)
    sumincid3weeks <- zoo::rollapply(incidence_seq, width = 3, FUN = sum, align = "left", fill = NA)
    
    ## alert 18: at least 0.25 per 10,000 population in 3 consecutive weeks
    alert18_exists <- ifelse(sumincid1weeks >= 0.25 & sumincid2weeks >= sumincid1weeks + 0.25 & sumincid3weeks >= sumincid2weeks + 0.25,TRUE, FALSE)
    alert18 <- rep(FALSE, length(alert18_exists))
    ## set alert to the week after pattern occurs
    alert18[which(alert18_exists) + 3] <- TRUE
    
    ## alert 19: at least 0.5 per 10,000 population in 3 consecutive weeks
    alert19_exists <- ifelse(sumincid1weeks >= 0.5 & sumincid2weeks >= sumincid1weeks + 0.5 & sumincid3weeks >= sumincid2weeks + 0.5,TRUE, FALSE)
    alert19 <- rep(FALSE, length(alert19_exists))
    ## set alert to the week after pattern occurs
    alert19[which(alert19_exists) + 3] <- TRUE
    
    ## alert 20: at least 1 per 10,000 population in 3 consecutive weeks
    alert20_exists <- ifelse(sumincid1weeks >= 1 & sumincid2weeks >= sumincid1weeks + 1 & sumincid3weeks >= sumincid2weeks + 1,TRUE, FALSE)
    alert20 <- rep(FALSE, length(alert20_exists))
    ## set alert to the week after pattern occurs
    alert20[which(alert20_exists) + 3] <- TRUE
    
    ## alert 21: at least 1.5 per 10,000 population in 3 consecutive weeks
    alert21_exists <- ifelse(sumincid1weeks >= 1.5 & sumincid2weeks >= sumincid1weeks + 1.5 & sumincid3weeks >= sumincid2weeks + 1.5,TRUE, FALSE)
    alert21 <- rep(FALSE, length(alert21_exists))
    ## set alert to the week after pattern occurs
    alert21[which(alert21_exists) + 3] <- TRUE
    
    ## alert 22: at least 2.5 per 10,000 population in 3 consecutive weeks
    alert22_exists <- ifelse(sumincid1weeks >= 2.5 & sumincid2weeks >= sumincid1weeks + 2.5 & sumincid3weeks >= sumincid2weeks + 2.5,TRUE, FALSE)
    alert22 <- rep(FALSE, length(alert22_exists))
    ## set alert to the week after pattern occurs
    alert22[which(alert22_exists) + 3] <- TRUE
    
    ## alert 23: at least 5 per 10,000 population in 3 consecutive weeks
    alert23_exists <- ifelse(sumincid1weeks >= 5 & sumincid2weeks >= sumincid1weeks + 5 & sumincid3weeks >= sumincid2weeks + 5,TRUE, FALSE)
    alert23 <- rep(FALSE, length(alert23_exists))
    ## set alert to the week after pattern occurs
    alert23[which(alert23_exists) + 3] <- TRUE
    
    ## alert 24: at least 7.5 per 10,000 population in 3 consecutive weeks
    alert24_exists <- ifelse(sumincid1weeks >= 7.5 & sumincid2weeks >= sumincid1weeks + 7.5 & sumincid3weeks >= sumincid2weeks + 7.5,TRUE, FALSE)
    alert24 <- rep(FALSE, length(alert24_exists))
    ## set alert to the week after pattern occurs
    alert24[which(alert24_exists) + 3] <- TRUE
    
    #### end rate alerts ####
    
    rc <- dplyr::mutate(df_tmp, 
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