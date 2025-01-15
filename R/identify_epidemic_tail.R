#' @export
#' @title identify_epidemic_tail
#' @name identify_epidemic_tail
#' @description this function will be used in identify_outbreak** functions
identify_epidemic_tail <- function (
    outbreak_data, #per location period and start_weekday
    tail_period = 6
){
  outbreak_data$epidemic_tail <- FALSE
  #identify tails of outbreaks
  ## 1. below outbreak threshold. 2. for certain number of consecutive weeks
  
  consecutive_vector <- rep(TRUE,nrow(outbreak_data))
  below_threshold_vector <- outbreak_data$risk == "low"
  tail_vector <- consecutive_vector*below_threshold_vector

  tail_position <- rle(tail_vector)

  if(any(tail_position$lengths[tail_position$values==1] >= tail_period)){
    tail_position_table <- data.frame(
      values = tail_position$values,
      lengths = tail_position$lengths,
      end_row_idx = cumsum(tail_position$lengths)
    )

    tail_position_table$start_row_idx = c(tail_position_table$end_row_idx-c(tail_position_table$end_row_idx[1],diff(tail_position_table$end_row_idx))+1)
    tail_position_table = tail_position_table %>%
      subset(lengths >= tail_period & values ==1)

    if(!nrow(tail_position_table)==0){
      
      tail_position_table$end_row_idx <- tail_position_table$end_row_idx - tail_period +1
      
      for (row_idx in seq(nrow(tail_position_table))) {
        outbreak_data$epidemic_tail[tail_position_table[row_idx,]$start_row_idx:tail_position_table[row_idx,]$end_row_idx] = TRUE
      }
    }
  }
  
  return(outbreak_data)
}



