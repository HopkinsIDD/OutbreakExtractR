#' @export
#' @title fill_missing_lps
#' @name fill_missing_lps
#' @description If an observation is missing a location period ID but there is another observation in the dataset with a location period ID for that location, fill in the missing location period ID. If there are multiple lp ID options, select the lp ID from the most recent observation. 
#' @param original_data dataframe with location, epiweek, TL, TR, sCh, cCh, deaths
fill_missing_lps <- function(original_data){

    possible_to_fill_locs <- original_data %>% 
      dplyr::filter(is.na(location_period_id)) %>% 
      arrange(location, TR)

    dictionary <- original_data %>%
      dplyr::select(location, location_period_id, TR) %>%
      dplyr::filter(location %in% possible_to_fill_locs$location & is.na(location_period_id) == F) %>%
      dplyr::arrange(TR) %>% 
      dplyr::group_by(location)

    if(nrow(dictionary)>0){
        message(paste("Some observations in these locations had missing LPs. They will be filled in with LPs from other observations in the same location:", paste(unique(possible_to_fill_locs$location), collapse = ", ")))
        
      rc <- original_data %>% 
          arrange(location, TR) %>% 
          group_by(location) %>%
          mutate(
            # Locate the closest previous and next non-NA location_period_id
            previous_id = zoo::na.locf(location_period_id, na.rm = FALSE),  # Closest previous non-NA value
            next_id = zoo::na.locf(location_period_id, fromLast = TRUE, na.rm = FALSE),  # Closest next non-NA value
            
            # Apply the logic to fill NA location_period_id based on closest previous and next values
            location_period_id = ifelse(
              is.na(location_period_id),
              case_when(
                # If both previous and next location_period_id are the same, use that value
                previous_id == next_id ~ previous_id,
                
                # If previous and next are different, choose the one with the later TR
                !is.na(next_id) & (is.na(previous_id) | TR[match(next_id, location_period_id)] > TR[match(previous_id, location_period_id)]) ~ next_id,
                
                TRUE ~ previous_id
              ),
              location_period_id
            )
          ) %>%
          ungroup() %>% 
          dplyr::select(-previous_id, -next_id)  # Remove helper columns
    } else{
        message("0 empty location periods were successfully filled in.")
        rc <- original_data
    }
    
    return(rc)

}