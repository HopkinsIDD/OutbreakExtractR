#' @export
#' @title identify_epidemic_start
#' @name identify_epidemic_start
#' @description this function will be used in identify_outbreak** functions. This function is mainly to check the consecutively increasing epiweeks.
#' @description per location period per start_day group: weekly cholera incidence
identify_epidemic_start <- function(
    outbreak_data = outbreak_data, #consecutive weekly outbreak data
    minimum_consecutive_reports=3
) {
  #identify consecutively increasing weeks
  outbreak_data$diff_sCh=c(diff(outbreak_data$sCh),0)
  outbreak_data_with_epidemic_start<-data.frame()

    outbreak_data_with_epistart<- outbreak_data
    outbreak_data_with_epistart$consecutive_increase <- FALSE
    outbreak_data_with_epistart$epidemic_start <- FALSE

    if(nrow(outbreak_data_with_epistart[which(outbreak_data_with_epistart$risk=='high'),])>=minimum_consecutive_reports){
      for (idx in 1:(nrow(outbreak_data_with_epistart)-minimum_consecutive_reports+1)) {
        if(all(outbreak_data_with_epistart$diff_sCh[idx:(idx+minimum_consecutive_reports-2)]>0)){
          outbreak_data_with_epistart$consecutive_increase[idx:(idx+minimum_consecutive_reports-1)]=TRUE
          #the first week has to be exceeding the outbreak threshold
          if(outbreak_data_with_epistart$risk[idx] == "high"){
            outbreak_data_with_epistart$epidemic_start[idx] = TRUE
          }
        }
      }
    }
    outbreak_data_with_epidemic_start <- rbind(outbreak_data_with_epidemic_start, outbreak_data_with_epistart)
  
  return(outbreak_data_with_epidemic_start)
}
