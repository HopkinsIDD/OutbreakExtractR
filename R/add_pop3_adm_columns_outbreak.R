#' Title add_pop3_adm_columns_outbreak
#' @name add_pop3_adm_columns_outbreak
#' @param basedf a df with a location and outbreak_start column and existing population column
#' @description Attach population and spatial scale columns from pre-outbreak extraction to a dataset with an alert_id
#' @return the df with an alert_id column with added columns for population size and spatial scale
#' @export
add_pop3_adm_columns_outbreak <- function(basedf, popdf){
  
  if("population" %in% names(basedf)){
    basedf <- dplyr::rename(basedf, pop = population)
    message("renaming population columns from base dataset")
  }
  
  basedf_ids <- dplyr::mutate(basedf, uq_ids = paste0(location, "_", outbreak_start)) %>%
    dplyr::distinct(uq_ids)
  
  clean_basedf <- dplyr::mutate(basedf, uq_ids = paste0(location, "_", outbreak_start)) %>%
    dplyr::mutate(outbreak_UID = paste0(location, "_", outbreak_start, "_", outbreak_end)) %>%
    dplyr::relocate(outbreak_UID)
  
  if(!("spatial_scale" %in% names(basedf))){
    clean_pop <- dplyr::mutate(popdf, uq_ids = paste0(location, "_", TL)) %>%
      dplyr::filter(uq_ids %in% basedf_ids$uq_ids) %>%
      dplyr::select(uq_ids, spatial_scale)
    message("adding only spatial scale columns")
  } 
  
  rc <- dplyr::left_join(clean_basedf, clean_pop, by = c("uq_ids")) %>%
    dplyr::select(-uq_ids) %>%
    dplyr::mutate(pop_brk = cut(pop, breaks = c(0, 50000, 500000, max(popdf$pop)), labels = c("< 50k", "[50k, 500k)", "\U2265 500k"), include.lowest=T, right=FALSE))
  
  return(rc)
} 
