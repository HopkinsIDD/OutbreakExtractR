#' @export
#' @title identify_epidemic_start
#' @name identify_epidemic_start
#' @description this function will be used in identify_outbreak** functions. This function is mainly to check the consecutively increasing epiweeks.
#' @description per location period per start_day group: weekly cholera incidence
identify_epidemic_start <- function(
    outbreak_data = outbreak_data, #consecutive weekly outbreak data
    outbreak_start_definition = c("consecutive","dual_window"), 
    min_weeks_above = 2,
    require_increasing_trend = FALSE,
    window_weeks = 3,
    cumulative_windows = 3,
    cumulative_case_threshold_ratio = 1.5
) {
  outbreak_data_with_epistart<- outbreak_data
  outbreak_data_with_epistart$epidemic_start <- FALSE
  outbreak_data_with_epidemic_start<-data.frame()
  
  if(outbreak_start_definition == "consecutive"){
    if(nrow(outbreak_data_with_epistart) < min_weeks_above){
      return(outbreak_data_with_epistart)
    }
    
    if(nrow(outbreak_data_with_epistart[which(outbreak_data_with_epistart$risk=='high'),])>=min_weeks_above){
      if(require_increasing_trend){
        outbreak_data$diff_sCh=c(diff(outbreak_data_with_epistart$sCh),0)
        outbreak_data_with_epistart$consecutive_increase=FALSE
        
        for (idx in 1:(nrow(outbreak_data_with_epistart)-min_weeks_above+1)) {
          if(all(outbreak_data_with_epistart$diff_sCh[idx:(idx+min_weeks_above-2)]>0)){
            outbreak_data_with_epistart$consecutive_increase[idx:(idx+min_weeks_above-1)]=TRUE
            #the first week has to be exceeding the outbreak threshold
            if(outbreak_data_with_epistart$risk[idx] == "high"){
              outbreak_data_with_epistart$epidemic_start[idx] = TRUE
            }
          }
        }
      } else {
         for (idx in 1:(nrow(outbreak_data_with_epistart)-min_weeks_above+1)) {
           if(all(outbreak_data_with_epistart$risk[idx:(idx+min_weeks_above-1)] == "high")){
             outbreak_data_with_epistart$epidemic_start[idx] = TRUE
           }
        }
      }
  }
} else if(outbreak_start_definition=="dual_window"){
    outbreak_data_with_epistart <- outbreak_data_with_epistart %>% 
      mutate(risk_num = as.integer(risk=='high'),
             sCh_cum_thresh = threshold*pop*cumulative_case_threshold_ratio,
             sCh_pos = as.integer(sCh > 0)) %>% 
      mutate(
        d1 =slider::slide_dbl(risk_num,sum,.before = window_weeks-1, .complete=TRUE),
        d2= slider::slide_dbl(sCh,sum,.before=cumulative_windows-1,.complete=TRUE),
        d2_2 = slider::slide_dbl(sCh_pos,sum,.before = cumulative_windows - 1,.complete = TRUE)
      ) %>% 
      mutate(
        alert = d1 >= min_weeks_above|(d2>=sCh_cum_thresh & sCh_cum_thresh!=0 & d2_2 >=2),
        d1_start = if_else(alert, row_number() - (window_weeks - 1), NA_integer_),
        d2_start = if_else(alert, row_number() - (cumulative_windows - 1), NA_integer_),
        epidemic_start = row_number() %in% c(d1_start, d2_start)
      )
  }
  outbreak_data_with_epidemic_start <- rbind(outbreak_data_with_epidemic_start,
                                             outbreak_data_with_epistart)
  return(outbreak_data_with_epidemic_start)
}
