#' Title add_negative_alert_groups
#' @name add_negative_alert_groups
#' @param summary_outcomes A dataframe containing alert group information and outcomes
#' @description
#' Takes a dataframe with summary outcomes for positive alerts and alert groups (created with extract_agroup_case_outcomes) and creates negative alert groups for alert groups 1-5.
#' Negative alerts groups occur if an alert group 1-5 was not trigerred while an alert group 0 was at a specific location-epiweek combination.
#' Negative alerts groups inherit the outcome and outcome_type from the corresponding alert 0. If the outcome is TRUE, they are considered false negatives.
#' If the outcome is FALSE, they are considered true negatives.
#' @return dataframe with summary outcomes for both positive and added rows for negative alert groups. Negative alert groups get their outcome and out_type from the corresponding alert group type 0 that occurred in the same location and epiweek 
#' @export
add_negative_alert_groups <- function(summary_outcomes) {
    
    summary_outcomes <- summary_outcomes %>%
      dplyr::group_by(alert_group_id) %>%
      dplyr::select(location, alert_group_id, alert_group_type, outcome_type, group_outcome) %>%
      unique() %>%
      #tidyr::drop_na() %>% ## probably not necessary now that we changed the alert group definition
      dplyr::mutate(location_epiweek = sub("^a[0-9]+_", "", alert_group_id)
      )
    
    ## split the dataframe into type_0 (alert_group_type == "a0") and the rest (1-5)
    type_0 <- subset(summary_outcomes, alert_group_type == "a0")
    type_1_to_5 <- subset(summary_outcomes, alert_group_type %in% c("a1", "a2", "a3", "a4", "a5"))
  
    alert_types <- c("a1", "a2", "a3", "a4", "a5")
    
    ## create new dataframe to store the negative alerts
    negative_alerts <- data.frame()
    
    ## add "alert_group_exists" column to the original dataframe to designate existing alert groups as positive
    summary_outcomes$alert_group_exists <- "positive"
    
    ## loop through each row of type_0 and check if the corresponding alert_group_type 1-5 is missing
    for (i in 1:nrow(type_0)) {
      ## get location_epiweek, out_type, and group_outcome for this alert_group_type 0
      loc_epiweek <- type_0$location_epiweek[i]
      outcome_0 <- type_0$group_outcome[i]
      out_type_0 <- type_0$outcome_type[i]
      
      ## check if any alerts 1-5 exist for this location_epiweek
      alerts_present <- unique(type_1_to_5$alert_group_type[type_1_to_5$location_epiweek == loc_epiweek])
      
      ## For each alert type, if it's missing, create a new row for the negative alert
      for (alert in alert_types) {
        if (!(alert %in% alerts_present)) {
          ## create new row with the negative alert
          new_row <- data.frame(
            location = type_0$location[i],
            alert_group_id = paste0("n_",alert, "_", loc_epiweek),  ## alert_group_id for negative alert groups
            alert_group_type = alert,
            outcome_type = out_type_0,                            ## inherits outcome_type from alert_group_type 0
            group_outcome = outcome_0,                        ## inherits group_outcome from alert_group_type 0
            location_epiweek = loc_epiweek,
            alert_group_exists = "negative"                         ## designated "negative" since the alert group doesn't exist
          )
          ## add new row to negative_alerts
          negative_alerts <- rbind(negative_alerts, new_row)
        }
      }
    }
    
    final_summary_outcomes <- rbind(summary_outcomes, negative_alerts)
    return(final_summary_outcomes)
}