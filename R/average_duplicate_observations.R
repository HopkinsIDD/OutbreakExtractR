#' @export
#' @title average_duplicate_observations
#' @name average_duplicate_observations
#' @description when observations with the same location and overlapping epiweeks exist, average them and remove the duplicates. 
#' @param original_data dataframe with location, epiweek, TL, TR, sCh, cCh, deaths
average_duplicate_observations <- function(
    original_data,...
){
    sorted <- original_data %>% 
        dplyr::arrange(location, TL, epiweek) %>%
        dplyr::group_by(location, epiweek) %>%
        dplyr::mutate(
          original_location_name = custom_paste(original_location_name),
          observation_collection_id = custom_paste(observation_collection_id)
        ) %>% 
        dplyr::add_count() %>%
        dplyr::ungroup()
    
    ## perform aggregation step only for dataframe with duplicates
    dups <- sorted %>%
        dplyr::filter(n>1) %>%
        dplyr::group_by(location, epiweek) %>%
        dplyr::mutate(dplyr::across(c(sCh, cCh, deaths), ~ mean(.x, na.rm = TRUE)),
                      original_location_name = custom_paste(original_location_name),
                      observation_collection_id = custom_paste(observation_collection_id)) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(dplyr::across(c(sCh, cCh, deaths), ~ ifelse(is.nan(.x), NA, .x))) %>% ## rm NaNs created by taking mean of NAs
        dplyr::distinct(location, epiweek, sCh, cCh, deaths, original_location_name, observation_collection_id, .keep_all = TRUE) ## remove the duplicates after averaging
    nodups <- sorted %>%
        dplyr::filter(n==1)
    
    cleaned <- dplyr::bind_rows(dups, nodups) %>%
        dplyr::select(-n) %>%
        dplyr::arrange(location, TL, epiweek)

    return(cleaned)
}