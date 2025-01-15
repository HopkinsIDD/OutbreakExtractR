# Function to identify outbreaks

#' @title identify_outbreaks

#' @param threahold_type: character: 1. fixed threshold: a fixed value as outbreak threshold. 2. mean weekly incidence: use the mean weekly incidence as the outbreak threshold. 3. outbreak_dependent threshold: use the mean weekly cholera incidence for the first three weeks as the threshold for that outbreak
#' @param fixed_outbreak_threshold: numeric: the assigned outbreak threshold (cases per week)
#' @param original_data dataframe: original cholera surveillance data
#' @param zero_case_assumption: logic: whether to assume weeks without reports have zero case
#' @param customized_TL: customize the lower bound of time for outbreak estimation
#' @param customized_TR: customize the upper bound of time for outbreak estimation
#' @export
#' @return

identify_outbreaks <- function(
    threshold_type,
    original_data,
    zero_case_assumption = T,
    customized_TL = NULL,
    customized_TR = NULL
    ){
  
  # Identify cholera outbreak thresholds
  original_data_threshold <- OutbreakExtractR::get_outbreak_threshold(
    threshold_type = threshold_type,
    surveillance_data = original_data,
    zero_case_assumption = zero_case_assumption,
    customized_TL,
    customized_TR)
  
  # Create an empty list to store outbreaks
  outbreak_list <- vector(mode = 'list', length = length(unique(original_data_threshold$location)))
  names(outbreak_list) <- unique(original_data_threshold$location)
  list_idx <- 1
  
  for (loc_idx in unique(original_data_threshold$location)) {
    
    preoutbreak_by_location <- original_data_threshold %>% subset(location == loc_idx) %>% arrange(TL)
    new_outbreak_by_location=data.frame()
    
    # extract outbreak start and end
    preoutbreak_by_location_start<-OutbreakExtractR::identify_epidemic_start(outbreak_data = preoutbreak_by_location)
    
    preoutbreak_by_location_start_end_washout<-OutbreakExtractR::identify_epidemic_tail(outbreak_data = preoutbreak_by_location_start, tail_period = 6)
    
    # get the row idx for epidemic start
    preoutbreak_by_location_start_end_washout$row_idx = rownames(preoutbreak_by_location_start_end_washout)
    epidemic_start_row_idx = unique(preoutbreak_by_location_start_end_washout[preoutbreak_by_location_start_end_washout$epidemic_start,]$row_idx)
    
    if(length(epidemic_start_row_idx)>0){
      preoutbreak_by_location_start_end_washout$outbreak_number = 0
      outbreak_number_idx =1
      if(length(epidemic_start_row_idx) > 1) {
        for (idx in seq(length(epidemic_start_row_idx)-1)) {
          
          data_between_epidemic_start = preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:epidemic_start_row_idx[idx+1],]
          if(any(data_between_epidemic_start$epidemic_tail)){
            outbreak_end = min(as.numeric(data_between_epidemic_start[data_between_epidemic_start$epidemic_tail,]$row_idx))
            preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:as.numeric(as.numeric(outbreak_end)+2-1),]$outbreak_number = 
              min(outbreak_number_idx,preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:as.numeric(as.numeric(outbreak_end)+2-1),]$outbreak_number[preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[idx]:as.numeric(as.numeric(outbreak_end)+2-1),]$outbreak_number>0])
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
        last_outbreak_end_idx = min(as.numeric(after_last_epidemi_start[after_last_epidemi_start$epidemic_tail,]$row_idx))
        preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:(last_outbreak_end_idx+2-1),]$outbreak_number = last_epidemic_start$outbreak_number
      } else {
        #there's only one outbreak start (one potential outbreak)
        data_between_epidemic_start = preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:nrow(preoutbreak_by_location_start_end_washout),]
        if(any(data_between_epidemic_start$epidemic_tail)){
          outbreak_end = min(as.numeric(data_between_epidemic_start[data_between_epidemic_start$epidemic_tail,]$row_idx))
          preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:as.numeric(as.numeric(outbreak_end)+2-1),]$outbreak_number = 
            min(outbreak_number_idx,preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:as.numeric(as.numeric(outbreak_end)+2-1),]$outbreak_number[preoutbreak_by_location_start_end_washout[epidemic_start_row_idx[length(epidemic_start_row_idx)]:as.numeric(as.numeric(outbreak_end)+2-1),]$outbreak_number>0])
          outbreak_number_idx = outbreak_number_idx +1
        }
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
    } else{
      preoutbreak_by_location_start_end_washout <-data.frame()
    }
    
    outbreak_list[[list_idx]] <- preoutbreak_by_location_start_end_washout
    list_idx =list_idx+1
    
  }
  return(outbreak_list)
}