#' Title add_country_column
#' @name add_country_column
#' @description Attach column with ISO3 country code (country) which is derived from the basedf location
#' @param basedf 
#' @return dataframe
#' @export
add_country_column <- function(basedf){
  
  if(!("country" %in% names(basedf))){
    rc <- dplyr::mutate(basedf, country = stringr::str_sub(location, 6, 8)) 
    message("adding country column to base dataset")
  } else{
    rc <- basedf
    message("country column already exists in base dataset")
  }
  return(rc)
}
