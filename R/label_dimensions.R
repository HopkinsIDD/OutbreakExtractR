#' Title label_dimensions
#' @name label_dimensions
#' @description function that adds figure-worthy utility dimension labels
#' @param basedf a df with a column for dimension
#' @param is_ordered logical with default value of TRUE
#' @return dataframe
#' @export
label_dimensions <- function(basedf, is_ordered = TRUE){
  
  basedf %>%
    dplyr::mutate(dimension = factor(dimension, levels = c("std_impact", "std_eff", "std_ppv", "std_missed", "std_delay"), labels = c("Impact", "Efficiency", "PPV", "Missed", "Delay"), ordered = is_ordered))
}