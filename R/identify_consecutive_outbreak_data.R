#' @export
#' @title identify_consecutive_outbreak_data
#' @name identify_consecutive_outbreak_data
#' @description this function will be used in identify_epidemic_start and identify_epidemic_period functions
identify_consecutive_outbreak_data <- function (
    outbreak_data,
    minimum_consecutive_reports=3,
    temporal_scale=c("weekly"),
    cons_group_col="consecutive_group",
    cons_obs_col="consecutive_obs"
){
  cons_group_col <- rlang::sym(cons_group_col)
  cons_obs_col<-rlang::sym(cons_obs_col)
  
  new_outbreak_data<-data.frame()
  
  if(temporal_scale=="weekly"){
    time_unit=7
    outbreak_data$weekday<-lubridate::wday(outbreak_data$TL,label=TRUE)
  }else if(temporal_scale=="daily"){
    time_unit=1
    outbreak_data$weekday<-"daily"
  }
  #subset data with that temporal scale
  outbreak_data<-outbreak_data%>%subset(TR-TL+1==time_unit)
  outbreak_data[[cons_group_col]]=NA
  outbreak_data[[cons_obs_col]]=FALSE
  
  for (weekday_idx in unique(outbreak_data$weekday)) {
    outbreak_data_by_lp_startday<- outbreak_data %>%
      subset(weekday==weekday_idx) %>%
      dplyr::arrange(TL)
    
    if(nrow(outbreak_data_by_lp_startday)>=minimum_consecutive_reports){
      #normalize the time difference by the time unit (weekly is 7 and daily is 1
      outbreak_data_by_lp_startday<-outbreak_data_by_lp_startday%>%arrange(TL)
      time_col_diff <- c(0,as.numeric(diff(outbreak_data_by_lp_startday$TR))/time_unit)
      
      consecutive_idx <- data.frame(end_row_position=cumsum(rle(time_col_diff)$lengths),
                                    values=rle(time_col_diff)$values,
                                    start_row_position=c(1,cumsum(rle(time_col_diff)$length)[-length(cumsum(rle(time_col_diff)$length))])) %>%
        subset(values==1) %>%
        subset(end_row_position-start_row_position+1>=minimum_consecutive_reports)
      
      if(!nrow(consecutive_idx)==0){
        for (row_idx in seq(nrow(consecutive_idx))) {
          outbreak_data_by_lp_startday[consecutive_idx$start_row_position[row_idx]:consecutive_idx$end_row_position[row_idx],][[cons_obs_col]] = TRUE
          outbreak_data_by_lp_startday[consecutive_idx$start_row_position[row_idx]:consecutive_idx$end_row_position[row_idx],][[cons_group_col]]=row_idx
        }
      }
    }
    new_outbreak_data<-rbind(new_outbreak_data,outbreak_data_by_lp_startday)
  }
  return(new_outbreak_data)
}
