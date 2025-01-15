#' @export
#' @title get_outbreak_threshold
#' @name get_outbreak_threshold
#' @description this function will be used in the identify_outbreak** functions. 1. average weekly/daily incidence across the whole study time period. 2. a fixed number. 3. changing outbreak threshold (for instance: the average weekly/daily cases of the first three weeks).
#' @param threahold_type: character: 1. fixed threshold: a fixed value as outbreak threshold. 2. mean weekly incidence: use the mean weekly incidence as the outbreak threshold. 3. outbreak_dependent threshold: use the mean weekly cholera incidence for the first three weeks as the threshold for that outbreak
#' @param fixed_outbreak_threshold: numeric: the assigned outbreak threshold (cases per week)
#' @param surveillance_data dataframe: original cholera surveillance data
#' @param zero_case_assumption: logic: whether to assume weeks without reports have zero case
#' @param customized_TL: customize the lower bound of time for outbreak estimation
#' @param customized_TR: customize the upper bound of time for outbreak estimation
#'
#' @return 
#' 
get_outbreak_threshold <- function (
    threshold_type = c("fixed threshold","mean weekly incidence rate","outbreak_dependent threshold","time_restricted threshold"),
    fixed_outbreak_threshold = NULL,
    surveillance_data = NULL, #daily or weekly cholera case data by location period
    zero_case_assumption = FALSE, # assume the weeks/days without reporting have 0 case
    customized_TL = NULL,
    customized_TR = NULL
) {
  if(!exists("threshold_type")){
    stop("You need to specify a type of ways to extract outbreak threshold.")
  } else {
    if(threshold_type == "fixed threshold"){
      if(!exists("fixed_outbreak_threshold")){
        stop("You need to assign a fixed number for the outbreak threshold.")
      } else if(fixed_outbreak_threshold<0){
        stop("The fixed outbreak threshold is invalid.")
      } else {
        print(paste0("The fixed outbreak threshold is set as ",fixed_outbreak_threshold," suspected cases per week"))
        return(fixed_outbreak_threshold)
      }
    } else if(threshold_type == "mean weekly incidence rate"){
      if(is.null(surveillance_data)){
        stop("To estimate the mean weekly incidence as the outbreak threshold, weekly or daily cholera surveillance data needs to be assigned")
      }

      
      if(is.null(customized_TL)){
        if(!zero_case_assumption){
          
          surveillance_data_threshold <- surveillance_data %>%
            group_by(location) %>% 
            mutate(threshold = mean(surveillance_data$sCh/surveillance_data$pop))
          
        } else {
          surveillance_data_threshold <- surveillance_data %>%
            group_by(location) %>% 
            mutate(threshold = sum(sCh/pop)/(as.numeric(max(TR)-min(TL+1))%/%7))
        }
      } else {
        surveillance_data <- surveillance_data %>% subset(TL >= customized_TL & TR <= customized_TR)
        if(!zero_case_assumption){
          surveillance_data_threshold <- surveillance_data %>%
            group_by(location) %>% 
            mutate(threshold = mean(surveillance_data$sCh/surveillance_data$pop))
          } else {
            surveillance_data_threshold <- surveillance_data %>%
              group_by(location) %>% 
              mutate(threshold = sum(sCh/pop)/(as.numeric(max(TR)-min(TL+1))%/%7))
            }

      }
      
      surveillance_data_threshold <- surveillance_data_threshold %>% 
      ungroup() %>% 
        mutate(
          risk = ifelse(sCh/pop >= threshold & sCh>0, "high","low")
        )
      
      return(surveillance_data_threshold)

    } else if(threshold_type == "outbreak-dependent threshold"){

    } else {
      stop("Unfortunately, we haven't developed the funciton for creating this type of outbreak threshold.")
    }
  }
}