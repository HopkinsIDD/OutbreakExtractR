## fill_phantom_zeroes
#' @export
#' @title fill_phantom_zeroes
#' @name fill_phantom_zeroes
#' @description Fill in weekly zeroes for sCh if no reporting between min(TL)- 8 weeks and max(TL)+ 8 weeks for that location
#' @param original_data dataframe with location, epiweek, TL, TR, sCh, cCh, deaths
fill_phantom_zeroes <- function(original_data){
	
	if(length(unique(original_data$start_weekday))!=1){
		error("All observations should have the same start_weekday. Please run set_uniform_wday_start on this dataset.")
	} else if(nrow(dplyr::distinct(original_data, location, TL) %>%
				dplyr::group_by(location, TL) %>%
				dplyr::add_count() %>%
				dplyr::filter(n>1)) > 1){
		error("There are overlapping weekly observations. Please run_average_duplicate_observations on this dataset.")
	}

	tmp_function <- function(df_original, loc){         

		df_tmp <- df_original %>%
			dplyr::filter(location == loc) %>%
			dplyr::arrange(desc(TR)) %>% ## fill template with most recent obs lp ID for template
			dplyr::group_by(location, spatial_scale, composite_loc, date_range, who_region, country, admin1, admin2, admin3, admin4, admin5) %>%
	        dplyr::summarise(location_period_id = NA)

	    df_data <- df_original %>%
	    	dplyr::filter(location == loc) %>%
	    	dplyr::distinct(location, TL, TR, sCh, cCh, deaths, spatial_scale, composite_loc, date_range, epiweek, start_weekday, who_region, country, admin1, admin2, admin3, admin4, admin5, location_period_id,original_location_name,observation_collection_id) %>%
	    	dplyr::mutate(phantom = FALSE)

	    ## fill zeroes 8 weeks before earliest observation and 8 weeks after latest observation in that location 
		minTL <- min(df_data$TL)-(7*8)
		maxTL <- max(df_data$TL)+(7*8)
		tmp <- tibble::tibble(
				location = loc,
				TL = seq(as.Date(minTL), as.Date(maxTL), by = 7),
				TR = TL + 6,
				sCh = 0,
				cCh = NA,
				deaths = NA,
				epiweek = OutbreakExtractR::get_epiweek(TL),
				start_weekday = lubridate::wday(TL, label = TRUE, abbr = FALSE)) %>%
			dplyr::right_join(df_tmp, by = c("location")) %>%
			dplyr::filter(!(TL %in% df_data$TL)) %>% 
			dplyr::mutate(phantom = TRUE,
			              original_location_name = NA,
			              observation_collection_id = NA)
	  
	  	dplyr::bind_rows(tmp, df_data) %>%
	  		dplyr::arrange(location, TL)
	  
	}

	## Run tmp_function to augment 0s for missing weeks for each location and flatten list of dfs back to single df 
	tmp2 <- purrr::map(
		.x = unique(original_data$location),
		\(x) tmp_function(original_data, x)
		) %>%
		purrr::list_rbind()

	return(tmp2)
	
}