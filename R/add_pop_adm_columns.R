#' Title add_pop_adm_columns
#' @name add_pop_adm_columns
#' @param basedf a df with alert_id column, can be an alert or an alert group df
#' @param popdf a pre-outbreak df
#' @description Attach population and spatial scale columns from pre-outbreak extraction to a dataset with an alert_id
#' @return the df with an alert_id column with added columns for population size and spatial scale
#' @export
add_pop_adm_columns <- function(basedf, popdf){
  
  if("pop" %in% names(basedf)){
    basedf <- dplyr::select(basedf, -pop, -pop_brk)
    message("dropping pop and pop_brk columns from base dataset")
  }
  
  basedf_ids <- tidyr::separate_wider_delim(basedf, alert_id, delim = "_", names = c(NA, "loc_sep", "TL_sep")) %>%
    dplyr::mutate(uq_ids = paste0(loc_sep, "_", TL_sep)) %>%
    dplyr::distinct(uq_ids)
  
  clean_basedf <- tidyr::separate_wider_delim(basedf, alert_id, delim = "_", names = c(NA, "loc_sep", "TL_sep"), cols_remove = FALSE) %>%
    dplyr::mutate(uq_ids = paste0(loc_sep, "_", TL_sep)) %>%
    dplyr::select(-loc_sep, -TL_sep)
  
  if("spatial_scale" %in% names(basedf)){
    clean_pop <- dplyr::mutate(popdf, uq_ids = paste0(location, "_", TL)) %>%
      dplyr::filter(uq_ids %in% basedf_ids$uq_ids) %>%
      dplyr::select(uq_ids, pop)
    message("adding only population columns")
  } else{
    clean_pop <- dplyr::mutate(popdf, uq_ids = paste0(location, "_", TL)) %>%
      dplyr::filter(uq_ids %in% basedf_ids$uq_ids) %>%
      dplyr::select(uq_ids, spatial_scale, pop)
    message("adding spatial scale and population columns")
  }
  
  rc <- dplyr::left_join(clean_basedf, clean_pop, by = c("uq_ids")) %>%
    dplyr::select(-uq_ids) %>%
    dplyr::mutate(pop_brk = cut(pop, breaks = c(0, 10000, 50000, 150000, 250000, 1E6, max(popdf$pop)), labels = c("< 10k", "[10k, 50k)", "[50k, 150k)", "[150k, 250k)", "[250k, 1M)", "\U2265 1M"), include.lowest=T, right=FALSE))
  
  return(rc)
}