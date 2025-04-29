#' @title remove_consecutive_alerts
#' @name remove_consecutive_alerts
#' @description Removes consecutive alerts of the same type for alerts 0-17. An alert can only be TRUE if it is preceded by nweeks of consecutive FALSE, which ensures there are no consecutive weeks of the same alert type.
#' @param alerts_df dataframe with alerts 
#' @param nweeks number of weeks of consecutive FALSE required for an alert to be TRUE
#' @return dataframe with columns with the pre-processed and post-processed alerts
#' @importFrom magrittr %>%
#' @export
#'
remove_consecutive_alerts <- function(alerts_df, nweeks = 4) {
  
  adjust_alerts <- function(df, alert_column, n_weeks = nweeks) {
    ## make sure the alert column exists
    if (!alert_column %in% names(df)) {
      stop(paste("Column", alert_column, "not found in data frame."))
    }
    
    ## create run-length encoding of TRUE/FALSE values from the alert column
    rle_alerts <- rle(df[[alert_column]])
    
    ## initialize vector to store the adjusted TRUE/FALSE values
    adjusted_alerts <- df[[alert_column]]
    
    ## start index to track the positions in the data frame
    start_index <- 1
    
    for(i in seq_along(rle_alerts$lengths)) {
      run_length <- rle_alerts$lengths[i]
      run_value <- rle_alerts$values[i]
      
      ## only consider TRUE runs
      if (isTRUE(run_value)) {
        for (j in 1:run_length) {
          current_index <- start_index + j - 1
          
          ## make sure there's at least one prior index to check for preceding FALSE runs
          if (current_index > 1) {
            ## calculate the start date of the current TRUE interval
            current_TL <- df$TL[current_index]
            
            ## start check for the prior n_weeks of FALSE values
            false_streak <- TRUE  ## assuming there's a FALSE streak unless proven otherwise
            
            ## check preceding n_weeks, from current_TL backwards
            for (k in (current_index - 1):1) {
              previous_TR <- df$TR[k]
              previous_value <- df[[alert_column]][k]
              
              ## if previous value is TRUE, stop the streak and break
              if (isTRUE(previous_value)) {
                false_streak <- FALSE
                break
              }
              
              ## if the previous TR is more than n_weeks ago, stop checking
              if ((current_TL - previous_TR) > n_weeks * 7) {
                break
              }
            }
            
            ## if there wasn't a streak of FALSE before the current TRUE, adjust the value
            if (!false_streak) {
              adjusted_alerts[current_index] <- FALSE
            }
          }
        }
      }
      
      ## move to the next part of the rle vector
      start_index <- start_index + run_length
    }
    
    return(adjusted_alerts)
  }
  
  
  ## function to apply the adjustment for each location
  tmp_function <- function(df_alerts, loc) {
    df_tmp <- dplyr::filter(df_alerts, location == loc)
    
    ## adjust alerts for alerts 0-17
    df_tmp$adjusted_alert0 <- adjust_alerts(df_tmp, "alert0")
    df_tmp$adjusted_alert1 <- adjust_alerts(df_tmp, "alert1")
    df_tmp$adjusted_alert2 <- adjust_alerts(df_tmp, "alert2")
    df_tmp$adjusted_alert3 <- adjust_alerts(df_tmp, "alert3")
    df_tmp$adjusted_alert4 <- adjust_alerts(df_tmp, "alert4")
    df_tmp$adjusted_alert5 <- adjust_alerts(df_tmp, "alert5")
    df_tmp$adjusted_alert6 <- adjust_alerts(df_tmp, "alert6")
    df_tmp$adjusted_alert7 <- adjust_alerts(df_tmp, "alert7")
    df_tmp$adjusted_alert8 <- adjust_alerts(df_tmp, "alert8")
    df_tmp$adjusted_alert9 <- adjust_alerts(df_tmp, "alert9")
    df_tmp$adjusted_alert10 <- adjust_alerts(df_tmp, "alert10")
    df_tmp$adjusted_alert11 <- adjust_alerts(df_tmp, "alert11")
    df_tmp$adjusted_alert12 <- adjust_alerts(df_tmp, "alert12")
    df_tmp$adjusted_alert13 <- adjust_alerts(df_tmp, "alert13")
    df_tmp$adjusted_alert14 <- adjust_alerts(df_tmp, "alert14")
    df_tmp$adjusted_alert15 <- adjust_alerts(df_tmp, "alert15")
    df_tmp$adjusted_alert16 <- adjust_alerts(df_tmp, "alert16")
    df_tmp$adjusted_alert17 <- adjust_alerts(df_tmp, "alert17")
    
    
    ## rename columns so that the adjusted columns replace original
    df_tmp <- df_tmp %>%
      dplyr::rename(
        "alert0_old" = "alert0",
        "alert1_old" = "alert1",
        "alert2_old" = "alert2",
        "alert3_old" = "alert3",
        "alert4_old" = "alert4",
        "alert5_old" = "alert5",
        "alert6_old" = "alert6",
        "alert7_old" = "alert7",
        "alert8_old" = "alert8",
        "alert9_old" = "alert9",
        "alert10_old" = "alert10",
        "alert11_old" = "alert11",
        "alert12_old" = "alert12",
        "alert13_old" = "alert13",
        "alert14_old" = "alert14",
        "alert15_old" = "alert15",
        "alert16_old" = "alert16",
        "alert17_old" = "alert17",
        "alert0" = "adjusted_alert0",
        "alert1" = "adjusted_alert1",
        "alert2" = "adjusted_alert2",
        "alert3" = "adjusted_alert3",
        "alert4" = "adjusted_alert4",
        "alert5" = "adjusted_alert5",
        "alert6" = "adjusted_alert6",
        "alert7" = "adjusted_alert7",
        "alert8" = "adjusted_alert8",
        "alert9" = "adjusted_alert9",
        "alert10" = "adjusted_alert10",
        "alert11" = "adjusted_alert11",
        "alert12" = "adjusted_alert12",
        "alert13" = "adjusted_alert13",
        "alert14" = "adjusted_alert14",
        "alert15" = "adjusted_alert15",
        "alert16" = "adjusted_alert16",
        "alert17" = "adjusted_alert17",
      )
    
    return(df_tmp)
  }
  
  ## apply the function to each location in the alerts_df
  tmp2 <- purrr::map(
    .x = unique(alerts_df$location),
    \(x) tmp_function(alerts_df, x)
  ) %>%
    purrr::list_rbind()
  
  return(tmp2)
}
