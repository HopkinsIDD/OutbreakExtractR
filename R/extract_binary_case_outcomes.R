#' Title extract_binary_case_outcomes
#' @name extract_binary_case_outcomes
#' @description returns a df with a binary outcome per alert group that is TRUE if the number of cases in the evaluation period has exceeded the threshold
#' @param eval_case_df a df with the number of cases in the evaluation period per alert group
#' @param case_thresh case threshold that determines an outcome as TRUE/FALSE
#' @return dataframe
#' @export
extract_binary_case_outcomes <- function(eval_case_df, case_thresh = 200){
  
  eval_case_df %>%
    dplyr::mutate(oCase_thresh = as.numeric(case_thresh),
                  oCase_binary = dplyr::if_else(total_sCh>case_thresh, TRUE, FALSE))
}