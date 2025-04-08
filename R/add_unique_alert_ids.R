#' @export
#' @title add_unique_alert_ids
#' @name add_unique_alert_ids
#' @description add unique IDs for each alert -- assuming alerts 1-18 based on trigger_alert function
#' @param dat dataframe
add_unique_alert_ids <- function(dat){

  dat %>%
    
  ## create alert ids using the preprocessed alerts
    dplyr::mutate(alert1_id = dplyr::case_when(alert1 ~ paste("a1", location, TL, sep = "_"),
                                              !alert1 ~ NA),
                  alert2_id = dplyr::case_when(alert2 ~ paste("a2", location, TL, sep = "_"),
                                              !alert2 ~ NA),
                  alert3_id = dplyr::case_when(alert3 ~ paste("a3", location, TL, sep = "_"),
                                              !alert3 ~ NA),
                  alert4_id = dplyr::case_when(alert4 ~ paste("a4", location, TL, sep = "_"),
                                              !alert4 ~ NA),
                  alert5_id = dplyr::case_when(alert5 ~ paste("a5", location, TL, sep = "_"),
                                              !alert5 ~ NA),
                  alert6_id = dplyr::case_when(alert6 ~ paste("a6", location, TL, sep = "_"),
                                               !alert6 ~ NA),
                  alert7_id = dplyr::case_when(alert7 ~ paste("a7", location, TL, sep = "_"),
                                               !alert7 ~ NA),
                  alert8_id = dplyr::case_when(alert8 ~ paste("a8", location, TL, sep = "_"),
                                               !alert8 ~ NA),
                  alert9_id = dplyr::case_when(alert9 ~ paste("a9", location, TL, sep = "_"),
                                               !alert9 ~ NA),
                  alert10_id = dplyr::case_when(alert10 ~ paste("a10", location, TL, sep = "_"),
                                               !alert10 ~ NA),
                  alert11_id = dplyr::case_when(alert11 ~ paste("a11", location, TL, sep = "_"),
                                               !alert11 ~ NA),
                  alert12_id = dplyr::case_when(alert12 ~ paste("a12", location, TL, sep = "_"),
                                               !alert12 ~ NA),
                  alert13_id = dplyr::case_when(alert13 ~ paste("a13", location, TL, sep = "_"),
                                               !alert13 ~ NA),
                  alert14_id = dplyr::case_when(alert14 ~ paste("a14", location, TL, sep = "_"),
                                               !alert14 ~ NA),
                  alert15_id = dplyr::case_when(alert15 ~ paste("a15", location, TL, sep = "_"),
                                               !alert15 ~ NA),
                  alert16_id = dplyr::case_when(alert16 ~ paste("a16", location, TL, sep = "_"),
                                               !alert16 ~ NA),
                  alert17_id = dplyr::case_when(alert17 ~ paste("a17", location, TL, sep = "_"),
                                               !alert17 ~ NA),
                  alert18_id = dplyr::case_when(alert18 ~ paste("a18", location, TL, sep = "_"),
                                                !alert18 ~ NA))
  
  
}