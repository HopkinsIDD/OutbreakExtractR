#' @export
#' @title create_alert_groups
#' @name create_alert_groups
#' @param df dataframe with alerts (has columns with pre-processed and post-processed alerts) 
#' @param alert_cols a vector with the names of the alert columns
#' @description
#' Group alerts into alert groups that include only one postprocessed alert of the same type and 0 or more preprocessed alerts 
#' The groups are asigned an alert_group_id
#' @return a dataframe with the existing columns of df plus an alert group id column. The alert group id includes the location and date (epiweek) of the first alert in the alert group
create_alert_groups <- function(df, alert_cols) {  
  
  ## nested function to create alert groups per location and alert type using rle
  create_alert_groups_rle <- function(data, alert_col) {  
    
    ## replace NAs in the alert column with FALSE 
    data[[alert_col]] <- ifelse(is.na(data[[alert_col]]), FALSE, data[[alert_col]])
    
    ## order data by epiweek
    data <- dplyr::arrange(data, epiweek)
    
    ## using rle() to find sequences of TRUE and FALSE in the current alert column
    rle_alerts <- rle(data[[alert_col]])
    
    alert_groups <- list()  # List to store alert groups
    group_id <- 0
    index <- 1  # Initialize row index
    
    ## loop over TRUE rle values
    for (i in seq_along(rle_alerts$values)) {
      if (rle_alerts$values[i] == TRUE) {  ## start a new alert group when the postprocessed alert is TRUE
        group_id <- group_id + 1
        group_start_index <- index  ## starting index of the current group
        
        ## initialize alert group with the TRUE rows
        group_rows <- data[group_start_index:(group_start_index + rle_alerts$lengths[i] - 1), , drop = FALSE]
        
        ## make the alert group ID using the location, epiweek, and alert type
        alert_group_id <- paste0("a", gsub("alert", "", alert_col), "_", data$location[group_start_index], "_", data$epiweek[group_start_index])
        
        ## keep adding rows until another TRUE appears (this will start a new group)
        for (j in seq(from = group_start_index + rle_alerts$lengths[i], to = nrow(data))) {  
          if (data[[alert_col]][j] == TRUE) break  
          group_rows <- rbind(group_rows, data[j, , drop = FALSE])  
        }
        
        ## assign alert group ID to the rows in current group
        group_rows[[paste0(alert_col, "_group_id")]] <- alert_group_id
        alert_groups[[group_id]] <- group_rows  # Add to the list of groups
      }
      
      ## move index forward by the length of the current run
      index <- index + rle_alerts$lengths[i]
    }
    
    ## combine all the alert groups into one df
    result <- do.call(rbind, alert_groups)
    return(result)
  }
  
  ## apply the function to each alert type to generate alert groups
  alert_groups_df <- df %>%
    dplyr::group_by(location) %>%
    dplyr::group_map(~ {
      ## to store results for each alert type
      results <- list()
      
      ## generate alert groups for each alert type
      for (alert_col in alert_cols) {
        result <- create_alert_groups_rle(.x, alert_col)  
        results[[alert_col]] <- result
      }
      
      ## combine all results for each alert type into one df
      combined_results <- Reduce(function(x, y) dplyr::full_join(x, y, by = colnames(df)), results)
      return(combined_results)
    }, .keep = TRUE) %>%
    dplyr::bind_rows()
  
  return(alert_groups_df)
}