# Helper function
#' Estimate Adjustment Factors for Population Data (for years >=2021, use the adjustment factors at 2020 instead)
#'
#' @param year numeric vector: Years for which population needs to be estimated.
#' @param country character: countries whose population needs to be estimated.
#' @param country_shp sf object: shapefiles of the country
#' @param raster_dir string: Directory containing the WorldPop raster files.
#' @param dest_dir string: Directory to download missing raster files.
#'
#' @return population adjustment factor
#' 
estimate_adj_factors <- function(
    country,
    year, 
    country_shp =NULL,
    raster_dir = "data/raster", 
    dest_dir = "data") {
  
  # Check if required columns are present in WPP2022
  data("WPP2022", package = "OutbreakExtractR")
  required_cols <- c("Time", "ISO3_code", "PopTotal")
  if (!all(required_cols %in% colnames(WPP2022))) {
    stop("WPP2022 must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
    # Adjust year if it exceeds the range
    year <- if (year > 2020) 2020 else year
    
    # Validate year
    if (any(!(year %in% WPP2022$Time))) {
      stop("Invalid year: ", year, ". Must fall within the UN time range: ",
           paste0(range(WPP2022$Time), collapse = "-"))
    }
    
    # Construct file path for the raster
    pop_file_path <- file.path(raster_dir, paste0(tolower(country), "_ppp_", year, ".tif"))
    
    # Download raster if missing
    if (!file.exists(pop_file_path)) {
      message("Downloading raster for ", country, " (", year, ")...")
      pop_file_path <- wpgpDownloadR::wpgpGetCountryDataset(
        ISO3 = country,
        covariate = paste0("ppp_", year),
        destDir = dest_dir,
        method = "curl"
      )
    }
    
    # Load raster and calculate population
    pop_raster <- raster::raster(pop_file_path)
    
    # Get the country-level shapefile
    if(is.null(country_shp)){
      country_shp <- rgeoboundaries::gb_adm0(country=country)
    }
    
    pop <- exactextractr::exact_extract(pop_raster, country_shp$geometry, "sum")
    
    # Get total UN population
    tot_UN <- WPP2022$PopTotal[WPP2022$Time == year & WPP2022$ISO3_code == country] * 1e3
    
    # Calculate adjustment factor
    adj_factors <- tot_UN / pop
    
    # Clean up
    rm(pop_raster)
  
  return(adj_factors)
}

#' @export
#' @title get_pop
#' @name get_pop
#' @description this function is used to get population data for each location period
#' @param shp sf objects: geometries
#' @param year numeric vector: Years for which population needs to be estimated.
#' @param country character: countries whose population needs to be estimated.
#' @param country_shp sf object: country geometry
#' @param pop_raster_path: path to save the pop raster file for a certain year (Estimated total number of people per grid-cell)
#' @param raster_dir path to save the GHS pop raster file for a certain year
#' @return 
get_pop <- function(
    shp= shp,
    year = 2000,
    country="AGO",
    country_shp = NULL,
    pop_raster_path ='worldpop',
    raster_dir = NULL
) {
  
  if(!dir.exists(pop_raster_path)){
    dir.create(pop_raster_path)
  }
  
  pop_year <- if (year > 2020) 2020 else year
  
  raster_file <- paste0(pop_raster_path,"/",country,"_ppp_",pop_year,'.tif')
  if(!file.exists(raster_file)){
    raster_file <- wpgpDownloadR::wpgpGetCountryDataset(ISO3 = country,
                                                        covariate = paste0("ppp_",pop_year),
                                                        destDir = pop_raster_path,
                                                        method = "curl")
  }
  
  if(year > 2020){
    pop_raster_2020 <- raster::raster(raster_file)
    
    # Aligning worldpop estimates to the UN population estiamtes at the country level
    adj_factors <- estimate_adj_factors(
      country = country, 
      year = 2020, 
      raster_dir = pop_raster_path, 
      dest_dir = pop_raster_path,
      country_shp = country_shp
    )
    
    pop_2020 <- exactextractr::exact_extract(pop_raster_2020, shp$geometry,'sum') * adj_factors
    
    pop_country_2020 <- WPP2022[WPP2022$ISO3_code == country & WPP2022$Time == 2020,]$PopTotal  * 1e3
    pop_country_after_2020 <- WPP2022[WPP2022$ISO3_code == country & WPP2022$Time == year,]$PopTotal * 1e3
    
    if(length(pop_country_after_2020)>1){ # for years after 2022, there are multiple UN pop estimates and the average of them is used.
      pop_country_after_2020 = mean(pop_country_after_2020,na.rm=T)
    }
    
    pop_export <- pop_2020/pop_country_2020*pop_country_after_2020
    
  }else{
    pop_raster <- raster::raster(raster_file)
    pop <- exactextractr::exact_extract(pop_raster, shp$geometry,'sum')
    # Aligning worldpop estimates to the UN population estiamtes at the country level
    adj_factors <- estimate_adj_factors(
      country = country, 
      year = year, 
      raster_dir = pop_raster_path, 
      dest_dir = pop_raster_path,
      country_shp = country_shp
    )
  pop_export <- pop * adj_factors
}
  
  # Check if pop is 0, if so, replace the pop with GHS population 
  if(pop_export ==0 ){
    cat("The population estimated based on worldpop is 0. Replace it with the GHS population")
    
    if(is.null(raster_dir) == T) {
      stop(paste("To estimate GHS population, raster directory needs to be specified."))
    } else{
      
      # Estimate the GHS population
      pop_output = get_ghs_pop(shp = shp, years = year, raster_dir = raster_dir, crs = 4326)
      if(pop_output$estimated_population ==0){
        stop(paste("GHS population estiamte is also 0."))
      } else {
        pop_export = pop_output$estimated_population
      }
    }
  }
  
  return(pop_export)
}