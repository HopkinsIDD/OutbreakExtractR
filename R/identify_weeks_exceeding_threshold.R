#' @export
#' @title identify_weeks_exceeding_threshold
#' @name identify_weeks_exceeding_threshold
identify_weeks_exceeding_threshold <- function(
    outbreak_data,
    threshold_col = c("case_threshold"),
    threshold_type=c('case','rate'),
    pop_col=NULL
){
  if(threshold_type%in%c("case","cases")){
    outbreak_data <- outbreak_data %>%
      mutate(
        risk = ifelse(
          as.numeric(sCh) >= !!rlang::sym(threshold_col),
          "high",
          "low"
        )
      )    
  } else if(threshold_type%in%c("rate","rates")){
    if(exists("pop_col")){
      if(any(colnames(outbreak_data)==pop_col)){
        outbreak_data <- outbreak_data %>%
          mutate(
            risk = ifelse(
              as.numeric(sCh)/!!rlang::sym(pop_col) >= !!rlang::sym(threshold_col),
              "high",
              "low"
            )
          )         
      } else{
        stop("The population column is invalid!!")
      }
    } else{
      stop("Since the threshold type is incidence, a population column must be provided.The population column is missing here.")
    }
  }
  return(outbreak_data)
}