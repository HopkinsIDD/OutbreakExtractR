#' @export
#' @title clean_psql_data
#' @name clean_psql_data
#' @description this function is used to clean the raw data pulled from psql database
#' @param original_data original dataset
clean_psql_data <- function(
    original_data,...
){


  # ---------------------------------------------------------------------------
  # Normalize taxdat API column names to OutbreakExtractR conventions.
  # taxdat::rename_database_fields(source = "api") produces different column
  # names than a direct psql export. This block maps either naming convention
  # to the names expected by the rest of this function, making it compatible
  # with both data sources.
  #
  # API names           -> OutbreakExtractR names
  # is_primary          -> primary
  # locationPeriod_id   -> location_period_id
  # OC_UID              -> observation_collection_id
  # location_name       -> location
  # attributes.fields.suspected_cases  -> sCh
  # attributes.fields.confirmed_cases  -> cCh
  # attributes.fields.deaths           -> deaths
  # ---------------------------------------------------------------------------
  col_map <- c(
    primary                  = "is_primary",
    location_period_id       = "locationPeriod_id",
    observation_collection_id = "OC_UID",
    location                 = "location_name",
    sCh                      = "attributes.fields.suspected_cases",
    cCh                      = "attributes.fields.confirmed_cases",
    deaths                   = "attributes.fields.deaths"
  )
  for (new_name in names(col_map)) {
    old_name <- col_map[[new_name]]
    if (old_name %in% names(original_data) && !new_name %in% names(original_data)) {
      original_data <- dplyr::rename(original_data, !!new_name := !!old_name)
    }
  }

  # clean the location names (keep updated)
  # clean and add descriptive columns
  outbreak_data <- clean_location_names(original_data=original_data) %>%
    dplyr::mutate(
      sCh = as.numeric(sCh),
      cCh = as.numeric(cCh),
      deaths = as.numeric(deaths),
      TL = lubridate::ymd(TL),
      TR = lubridate::ymd(TR),
      primary = dplyr::case_when(
                  is.logical(primary) ~ as.logical(primary),  # API source: already TRUE/FALSE
                  primary == "f" ~ FALSE,                      # psql source: "f"/"t" strings
                  primary == "t" ~ TRUE)
    ) %>% 
    dplyr::filter(primary) %>% ## always only keep primary data
    dplyr::mutate(
      date_range = TR-TL+1,
      temporal_scale = dplyr::case_when(
                  date_range == as.difftime(1, units = "days") ~ "daily",
                  date_range > as.difftime(1, units = "days") & date_range < as.difftime(7, units = "days") ~ "multiday",
                  date_range == as.difftime(7, units = "days") ~ "weekly",
                  date_range > as.difftime(7, units = "days") & date_range < as.difftime(28, units = "days") ~ "multiweek",
                  date_range %in% as.difftime(c(28, 29, 30, 31), units = "days") ~ "monthly",
                  date_range > as.difftime(31, units = "days") & date_range < as.difftime(365, units = "days") ~ "multimonth",
                  date_range %in% as.difftime(c(365, 366), units = "days") ~ "yearly",
                  date_range > as.difftime(366, units = "days") ~ "multiyear"),
      temporal_scale = factor(temporal_scale, levels = c("multiyear", "yearly", "multimonth", "monthly", 'multiweek', 'weekly', 'multiday', 'daily')),
      spatial_scale = dplyr::case_when(
                  stringr::str_count(location, pattern = "::") == 1 ~ "country",
                  stringr::str_count(location, pattern = "::") == 2 ~ "admin1",
                  stringr::str_count(location, pattern = "::") == 3 ~ "admin2",
                  stringr::str_count(location, pattern = "::") == 4 ~ "admin3",
                  stringr::str_count(location, pattern = "::") >= 5 ~ "admin4 or lower"),
      spatial_scale = factor(spatial_scale, levels = c("country", "admin1", "admin2", "admin3", "admin4 or lower")),
      start_weekday = lubridate::wday(TL, label = TRUE, abbr = FALSE),
      epiweek = OutbreakExtractR::get_epiweek(TL),
      composite_loc = dplyr::if_else(stringr::str_detect(location, "\\|"), TRUE, FALSE)
    ) %>%
    dplyr::select(location, TL, TR, sCh, cCh, deaths, spatial_scale, composite_loc, date_range, temporal_scale, start_weekday, epiweek, who_region, country, admin1, admin2, admin3, admin4, admin5, admin6, location_period_id,observation_collection_id,original_location_name)

  return(outbreak_data)
}
