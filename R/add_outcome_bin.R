#' Title add_outcome_bin
#' @name add_outcome_bin
#' @description bin case outcomes
#' @param basedf 
#' @param bins 
#' @return
#' @export
add_outcome_bin <- function(basedf, bins){
  
  if(!("sCh_bin" %in% names(basedf))){
    rc <- basedf %>%
      dplyr::mutate(sCh_bin = cut(total_sCh, breaks = c(-1, seq(0, 1000, by = 100), seq(2000, round(max(total_sCh)+1000, digits = -3), by = 1000)), right = TRUE, ordered_result = TRUE, labels = c(seq(0, 1000, by = 100), seq(2000, round(max(total_sCh)+1000, digits = -3), by = 1000))))
    message("adding sCh_bin column")
  } else{
    rc <- basedf
    message("sCh_bin column already exists in base dataset")
  }
  
  return(rc)
}