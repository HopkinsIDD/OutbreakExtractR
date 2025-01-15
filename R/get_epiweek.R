#' @export
#' @title get_epiweek
#' @name get_epiweek
#' @description Return epiweek column for a given date
#' @param epidate date object to 
#' @return epiweek string (YYYY-ww) where "ww" means 2-digit week number
get_epiweek <- function(epidate){

    if(!("Date" %in% class(epidate))){
        error("add_epiweek function only works on Date class objects")
    }

    return(paste(as.character(lubridate::isoyear(epidate)), 
                             dplyr::if_else(lubridate::isoweek(epidate)<10, 
                                            paste0("0", lubridate::isoweek(epidate)), 
                                            as.character(lubridate::isoweek(epidate))), 
                             sep="-"))
}