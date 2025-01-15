#' @export
#' @title add_unique_alert_ids
#' @name add_unique_alert_ids
#' @description add unique IDs for each alert -- assuming alerts 0-17 based on trigger_alert function
#' @param dat dataframe
add_unique_alert_ids <- function(dat){

  dat %>%
    
  ## create alert ids using the preprocessed alerts
    dplyr::mutate(alert0_id = dplyr::case_when(alert0_old ~ paste("a0", location, epiweek, sep = "_"),
                                              !alert0_old ~ NA),
                  alert1_id = dplyr::case_when(alert1_old ~ paste("a1", location, epiweek, sep = "_"),
                                              !alert1_old ~ NA),
                  alert2_id = dplyr::case_when(alert2_old ~ paste("a2", location, epiweek, sep = "_"),
                                              !alert2_old ~ NA),
                  alert3_id = dplyr::case_when(alert3_old ~ paste("a3", location, epiweek, sep = "_"),
                                              !alert3_old ~ NA),
                  alert4_id = dplyr::case_when(alert4_old ~ paste("a4", location, epiweek, sep = "_"),
                                              !alert4_old ~ NA),
                  alert5_id = dplyr::case_when(alert5_old ~ paste("a5", location, epiweek, sep = "_"),
                                              !alert5_old ~ NA),
                  alert6_id = dplyr::case_when(alert6_old ~ paste("a6", location, epiweek, sep = "_"),
                                               !alert6_old ~ NA),
                  alert7_id = dplyr::case_when(alert7_old ~ paste("a7", location, epiweek, sep = "_"),
                                               !alert7_old ~ NA),
                  alert8_id = dplyr::case_when(alert8_old ~ paste("a8", location, epiweek, sep = "_"),
                                               !alert8_old ~ NA),
                  alert9_id = dplyr::case_when(alert9_old ~ paste("a9", location, epiweek, sep = "_"),
                                               !alert9_old ~ NA),
                  alert10_id = dplyr::case_when(alert10_old ~ paste("a10", location, epiweek, sep = "_"),
                                               !alert10_old ~ NA),
                  alert11_id = dplyr::case_when(alert11_old ~ paste("a11", location, epiweek, sep = "_"),
                                               !alert11_old ~ NA),
                  alert12_id = dplyr::case_when(alert12_old ~ paste("a12", location, epiweek, sep = "_"),
                                               !alert12_old ~ NA),
                  alert13_id = dplyr::case_when(alert13_old ~ paste("a13", location, epiweek, sep = "_"),
                                               !alert13_old ~ NA),
                  alert14_id = dplyr::case_when(alert14_old ~ paste("a14", location, epiweek, sep = "_"),
                                               !alert14_old ~ NA),
                  alert15_id = dplyr::case_when(alert15_old ~ paste("a15", location, epiweek, sep = "_"),
                                               !alert15_old ~ NA),
                  alert16_id = dplyr::case_when(alert16_old ~ paste("a16", location, epiweek, sep = "_"),
                                               !alert16_old ~ NA),
                  alert17_id = dplyr::case_when(alert17_old ~ paste("a17", location, epiweek, sep = "_"),
                                               !alert17_old ~ NA))
  
  
}