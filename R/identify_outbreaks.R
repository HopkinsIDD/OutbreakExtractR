# Function to identify outbreaks

# Internal helper (not exported): zero out outbreak_number for any outbreak whose
# total cases (summed over the full numbered outbreak window) fall below
# min_total_cases.
filter_small_outbreaks <- function(df, min_total_cases) {
  small <- df %>%
    dplyr::filter(outbreak_number > 0) %>%
    dplyr::group_by(outbreak_number) %>%
    dplyr::summarise(total = sum(sCh, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(total < min_total_cases) %>%
    dplyr::pull(outbreak_number)
  if (length(small) > 0)
    df$outbreak_number[df$outbreak_number %in% small] <- 0
  df
}

#' @title identify_outbreaks

#' @param threahold_type: character: 1. fixed threshold: a fixed value as outbreak threshold. 2. mean weekly incidence: use the mean weekly incidence as the outbreak threshold. 3. outbreak_dependent threshold: use the mean weekly cholera incidence for the first three weeks as the threshold for that outbreak
#' @param fixed_outbreak_threshold: numeric: the assigned outbreak threshold (cases per week)
#' @param original_data dataframe: original cholera surveillance data
#' @param zero_case_assumption: logic: whether to assume weeks without reports have zero case
#' @param customized_TL: customize the lower bound of time for outbreak estimation
#' @param customized_TR: customize the upper bound of time for outbreak estimation
#' @param cumulative_min_cases: numeric: minimum cumulative cases. Used by the dual_window cumulative trigger and, when \code{filter_outbreaks_by_size = TRUE}, as the minimum total-case threshold for the post-detection size filter.
#' @param filter_outbreaks_by_size: logical: when TRUE, drop detected outbreaks whose total cases (summed over the full outbreak window) fall below \code{cumulative_min_cases}. Default FALSE (no filtering, backward compatible).
#' @param keep_nonoutbreak_locations: logical: when TRUE, locations that never trigger an epidemic start are returned as their full time series labelled \code{outbreak_number = 0} and \code{`Time Period` = "non-outbreak period"}, instead of an empty data.frame. Use this to retain every location in the output rather than silently dropping those without a detected outbreak. Default FALSE (backward compatible).
#' @export
#' @return list of dataframes

identify_outbreaks <- function(
    threshold_type,
    original_data,
    zero_case_assumption = T,
    customized_TL = NULL,
    customized_TR = NULL,
    outbreak_start_definition = c("consecutive","dual_window"),
    min_weeks_above = 2,
    require_increasing_trend = FALSE,
    window_weeks = window_weeks,
    cumulative_windows = cumulative_windows,
    cumulative_case_threshold_ratio = cumulative_case_threshold_ratio,
    cumulative_trigger_type=cumulative_trigger_type,
    use_cumulative_trigger=use_cumulative_trigger,
    cumulative_min_cases=cumulative_min_cases,
    nonzero_windows = nonzero_windows,
    tail_period =6,
    filter_outbreaks_by_size = FALSE,
    keep_nonoutbreak_locations = FALSE
    ){

  # Identify cholera outbreak thresholds
  original_data_threshold <- OutbreakExtractR::get_outbreak_threshold(
    threshold_type = threshold_type,
    surveillance_data = original_data,
    zero_case_assumption = zero_case_assumption,
    customized_TL = customized_TL,
    customized_TR = customized_TR)
  
  # Create an empty list to store outbreaks
  outbreak_list <- vector(mode = 'list', length = length(unique(original_data_threshold$location)))
  names(outbreak_list) <- unique(original_data_threshold$location)
  list_idx <- 1
  
  for (loc_idx in unique(original_data_threshold$location)) {
    
    preoutbreak_by_location <- original_data_threshold %>% subset(location == loc_idx) %>% arrange(TL)
    new_outbreak_by_location=data.frame()
    
    # extract outbreak start and end
    preoutbreak_by_location_start<-OutbreakExtractR::identify_epidemic_start(outbreak_data = preoutbreak_by_location,
                                                                             outbreak_start_definition = outbreak_start_definition, 
                                                                             require_increasing_trend = require_increasing_trend,
                                                                             min_weeks_above = min_weeks_above,
                                                                             window_weeks = window_weeks,
                                                                             use_cumulative_trigger=use_cumulative_trigger,
                                                                             cumulative_trigger_type=cumulative_trigger_type,
                                                                             cumulative_windows = cumulative_windows,
                                                                             cumulative_case_threshold_ratio = cumulative_case_threshold_ratio,
                                                                             cumulative_min_cases=cumulative_min_cases,
                                                                             nonzero_windows = nonzero_windows)
    preoutbreak_by_location_start_end_washout<-OutbreakExtractR::identify_epidemic_tail(outbreak_data = preoutbreak_by_location_start, tail_period = tail_period)
    
    # get the row idx for epidemic start
    preoutbreak_by_location_start_end_washout$row_idx = rownames(preoutbreak_by_location_start_end_washout)
    epidemic_start_row_idx = unique(preoutbreak_by_location_start_end_washout[preoutbreak_by_location_start_end_washout$epidemic_start,]$row_idx)
    
    if(length(epidemic_start_row_idx)>0){
      preoutbreak_by_location_start_end_washout$outbreak_number = 0
      outbreak_number_idx =1
      if(length(epidemic_start_row_idx) > 1) {
        for (idx in seq(length(epidemic_start_row_idx)-1)) {
          
          data_between_epidemic_start = preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:epidemic_start_row_idx[idx+1],]
          if(any(data_between_epidemic_start$epidemic_tail) & nrow(data_between_epidemic_start)>=tail_period+2){
            outbreak_end = min(as.numeric(data_between_epidemic_start[data_between_epidemic_start$epidemic_tail,]$row_idx))
            preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:as.numeric(as.numeric(outbreak_end)+tail_period-1),]$outbreak_number =
              min(outbreak_number_idx,preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:as.numeric(as.numeric(outbreak_end)+tail_period-1),]$outbreak_number[preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:as.numeric(as.numeric(outbreak_end)+tail_period-1),]$outbreak_number>0])
            outbreak_number_idx = outbreak_number_idx +1
          } else {
            preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:epidemic_start_row_idx[idx+1],]$outbreak_number = 
              min(outbreak_number_idx,preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:epidemic_start_row_idx[idx+1],]$outbreak_number[preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:epidemic_start_row_idx[idx+1],]$outbreak_number>0])
            outbreak_number_idx = outbreak_number_idx +1
          }
        }
      }
      
      #for the last outbreak
      last_epidemic_start = preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)],]
      if(last_epidemic_start$outbreak_number>0){
        final_outbreak_number = last_epidemic_start$outbreak_number
        after_last_epidemi_start = preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:nrow(preoutbreak_by_location_start_end_washout),]
        # Guard: if no epidemic_tail exists after the last start (outbreak extends
        # to the end of the window), fall back to the last row index.
        if(any(after_last_epidemi_start$epidemic_tail)){
          last_outbreak_end_idx = min(as.numeric(after_last_epidemi_start[after_last_epidemi_start$epidemic_tail,]$row_idx))
        } else {
          last_outbreak_end_idx = nrow(preoutbreak_by_location_start_end_washout)
        }
        end_idx = min(as.numeric(last_outbreak_end_idx) + tail_period - 1,
                      nrow(preoutbreak_by_location_start_end_washout))
        preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:end_idx,]$outbreak_number = last_epidemic_start$outbreak_number
      } else {
        #there's only one outbreak start (one potential outbreak)
        data_between_epidemic_start = preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:nrow(preoutbreak_by_location_start_end_washout),]
        if(any(data_between_epidemic_start$epidemic_tail)){
          outbreak_end = min(as.numeric(data_between_epidemic_start[data_between_epidemic_start$epidemic_tail,]$row_idx))
          preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:as.numeric(as.numeric(outbreak_end)+tail_period-1),]$outbreak_number =
            min(outbreak_number_idx,preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:as.numeric(as.numeric(outbreak_end)+tail_period-1),]$outbreak_number[preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:as.numeric(as.numeric(outbreak_end)+tail_period-1),]$outbreak_number>0])
          outbreak_number_idx = outbreak_number_idx +1
        }
      }
      
      # Post-detection size filter: drop outbreaks whose total cases (summed over
      # the full outbreak window) fall below cumulative_min_cases. Applied before
      # the Time Period labelling so dropped outbreaks become "non-outbreak period".
      if (isTRUE(filter_outbreaks_by_size) && !is.null(cumulative_min_cases)) {
        preoutbreak_by_location_start_end_washout <-
          filter_small_outbreaks(preoutbreak_by_location_start_end_washout,
                                 cumulative_min_cases)
      }

      preoutbreak_by_location_start_end_washout <- preoutbreak_by_location_start_end_washout %>%
        mutate(`Time Period` = ifelse(
          outbreak_number>0,
          "outbreak period",
          "non-outbreak period"
        )) %>%
        mutate(
          `Time Period` = factor(`Time Period`,levels =c("outbreak period",'non-outbreak period'))
        )
    } else if (isTRUE(keep_nonoutbreak_locations) &&
               nrow(preoutbreak_by_location_start_end_washout) > 0) {
      # No epidemic start for this location. Rather than dropping it, keep the
      # full series labelled as a non-outbreak period so downstream consumers
      # retain every location. Columns match the if-branch above.
      preoutbreak_by_location_start_end_washout$outbreak_number <- 0
      preoutbreak_by_location_start_end_washout$`Time Period` <- factor(
        "non-outbreak period",
        levels = c("outbreak period", "non-outbreak period")
      )
    } else{
      preoutbreak_by_location_start_end_washout <-data.frame()
    }
    
    outbreak_list[[list_idx]] <- preoutbreak_by_location_start_end_washout
    list_idx =list_idx+1
    
  }
  return(outbreak_list)
}
