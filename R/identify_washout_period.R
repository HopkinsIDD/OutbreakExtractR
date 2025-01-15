#' @export
#' @title identify_washout_period
#' @name identify_washout_period
#' @description this function will be used in identify_outbreak** functions
identify_washout_period <- function (
    outbreak_data, #per location period and start_weekday
    washout_period = 3, #unit is in week,
    tail_period = 2
    
){
  #identify epidemic tail for the outbreak
  outbreak_data_with_tail <-outbreak_data
  outbreak_data_with_tail$washout_period <- FALSE
  
  #identify washout periods of outbreaks
  ## 1. below outbreak threshold. 2. for certain number of consecutive weeks 3. after outbreak tail
  if(any(outbreak_data_with_tail$epidemic_tail)){
    
    late_tail_dates<-data.frame(outbreak_data_with_tail%>%subset(epidemic_tail))%>%
      dplyr::slice(which(dplyr::row_number() %in% seq(tail_period, n(), tail_period)))
   
    late_tail_dates<-late_tail_dates$TL
    
    if(length(late_tail_dates)==1){
      
        outbreak_data_after_tail <- outbreak_data_with_tail%>%
          subset(TL>lubridate::ymd(late_tail_dates)) 

        outbreak_data_with_consecutive_reports <- OutbreakExtractR::identify_consecutive_outbreak_data(
          outbreak_data = outbreak_data_after_tail,
          minimum_consecutive_reports = washout_period,
          temporal_scale="weekly",
          cons_group_col="consecutive_group_washout",
          cons_obs_col="consecutive_obs_washout"
        )
        
        consecutive_vector <- outbreak_data_with_consecutive_reports$consecutive_obs_washout
        below_threshold_vector <- outbreak_data_with_consecutive_reports$risk == "low"
        
        washout_vector <- consecutive_vector*below_threshold_vector
        
        washout_position <- rle(washout_vector)
        
        if(any(washout_position$lengths[washout_position$values==1] >= washout_period)){
          
          washout_position_table <- data.frame(
            values = washout_position$values,
            lengths = washout_position$lengths,
            end_row_idx = cumsum(washout_position$lengths)
          )
          
          washout_position_table$start_row_idx = c(washout_position_table$end_row_idx-c(washout_position_table$end_row_idx[1],diff(washout_position_table$end_row_idx))+1)
          
          washout_position_table = washout_position_table %>%
            subset(lengths >= washout_period & values ==1) %>%
            subset(start_row_idx ==1) %>% #washout period should be right after the epidemic tail
            mutate(end_row_idx=start_row_idx+washout_period-1)
          
          if(!nrow(washout_position_table)==0){
            for (row_idx in seq(nrow(washout_position_table))) {
              outbreak_data_with_consecutive_reports$washout_period[washout_position_table[row_idx,]$start_row_idx:washout_position_table[row_idx,]$end_row_idx] = TRUE
            }
          }
        }
        new_outbreak_data <- rbind(outbreak_data_with_tail[outbreak_data_with_tail$TL<=lubridate::ymd(late_tail_dates),],
                                   outbreak_data_with_consecutive_reports)
    } else{
      
      for (tail_date_idx in 1:(length(late_tail_dates)-1)){
      
        outbreak_data_after_tail <- outbreak_data_with_tail%>%
          subset(TL>lubridate::ymd(late_tail_dates[tail_date_idx]) &
                   TL < lubridate::ymd(late_tail_dates[tail_date_idx+1])) 
        
        outbreak_data_with_consecutive_reports <- OutbreakExtractR::identify_consecutive_outbreak_data(
          outbreak_data = outbreak_data_after_tail,
          minimum_consecutive_reports = washout_period,
          temporal_scale="weekly",
          cons_group_col="consecutive_group_washout",
          cons_obs_col="consecutive_obs_washout"
        )
        consecutive_vector <- outbreak_data_with_consecutive_reports$consecutive_obs_washout
        below_threshold_vector <- outbreak_data_with_consecutive_reports$risk == "low"
        
        washout_vector <- consecutive_vector*below_threshold_vector
        
        washout_position <- rle(washout_vector)
        
        if(any(washout_position$lengths[washout_position$values==1] >= washout_period)){
          
          washout_position_table <- data.frame(
            values = washout_position$values,
            lengths = washout_position$lengths,
            end_row_idx = cumsum(washout_position$lengths)
          )
          
          washout_position_table$start_row_idx = c(washout_position_table$end_row_idx-c(washout_position_table$end_row_idx[1],diff(washout_position_table$end_row_idx))+1)
          
          washout_position_table = washout_position_table %>%
            subset(lengths >= washout_period & values ==1) %>%
            subset(start_row_idx ==1) %>% #washout period should be right after the epidemic tail
            mutate(end_row_idx=start_row_idx+washout_period-1)
          
          if(!nrow(washout_position_table)==0){
            for (row_idx in seq(nrow(washout_position_table))) {
              outbreak_data_with_consecutive_reports$washout_period[washout_position_table[row_idx,]$start_row_idx:washout_position_table[row_idx,]$end_row_idx] = TRUE
            }
          }
        }
        new_outbreak_data <- rbind(outbreak_data_with_tail[outbreak_data_with_tail$TL<lubridate::ymd(late_tail_dates[tail_date_idx]),],
                                   outbreak_data_with_consecutive_reports)
      } 
    }
  } else { new_outbreak_data <- outbreak_data_with_tail
  }
  return(new_outbreak_data)
}