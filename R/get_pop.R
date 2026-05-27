# Helper function
#' Download country-specific constrained world pop 100*100m raster (2015-2030) from the worldpop data repo: 
#' @param country country iso code
#' @param year the year of the population raster
#' @param dest_dir folder name where the downloaded population raster will be saved
download_worldpop_constrained <- function(country, year, dest_dir = "worldpop") {
  if (!dir.exists(dest_dir)) dir.create(dest_dir)
  
  out_file <- file.path(
    dest_dir,
    paste0(tolower(country), "_pop_", year, "_CN_100m_R2025A_v1.tif")
  )
  
  if (file.exists(out_file)) return(out_file)
  
  url <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/",
    "R2025A/", year, "/", country,
    "/v1/100m/constrained/",
    tolower(country), "_pop_", year, "_CN_100m_R2025A_v1.tif"
  )
  
  message("Downloading WorldPop constrained raster: ", url)
  curl::curl_download(url, out_file)
  
  return(out_file)
}

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
  
  # Check if required columns are present in WPP2024
  data("WPP2024", package = "OutbreakExtractR") ## possibly not necessary now that WPP2024 is lazily loaded 
  required_cols <- c("Time", "ISO3_code", "PopTotal")
  if (!all(required_cols %in% colnames(WPP2024))) {
    stop("WPP2024 must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Adjust year if it exceeds the range
  year <- if (year < 2015) 2015 else year
  
  # Validate year
  if (any(!(year %in% WPP2024$Time))) {
    stop("Invalid year: ", year, ". Must fall within the UN time range: ",
         paste0(range(WPP2024$Time), collapse = "-"))
  }
  
  # download and load population raster
  pop_file_path <- download_worldpop_constrained(
    country = country,
    year = year,
    dest_dir = raster_dir
  )
  pop_raster <- raster::raster(pop_file_path)
  
  # Get the country-level shapefile
  if(is.null(country_shp)){
    country_shp <- rgeoboundaries::gb_adm0(country=country)
  }
  
  pop <- exactextractr::exact_extract(pop_raster, country_shp$geometry, "sum")
  
  ## CA 1 Apr: Ensure single value
  if (length(pop) > 1) {
    message("multiple population values found when estimating adj values")
  }
  
  # Get total UN population
  tot_UN <- WPP2024$PopTotal[WPP2024$Time == year & WPP2024$ISO3_code == country] * 1e3
  
  # Calculate adjustment factor
  adj_factors <- tot_UN / pop
  
  # Clean up
  rm(pop_raster)
  
  return(adj_factors)
}

#' Estimate population function - updated in May 2026: use the updated population raster between 2015-2030
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
#' @return numeric
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
  
  data("WPP2024", package = "OutbreakExtractR")
  
  country <- toupper(country)
  year <- as.integer(year)
  
  # Get the population raster
  raster_file <- download_worldpop_constrained(
    country = country,
    year = year,
    dest_dir = pop_raster_path
  )
  
  pop_raster <- raster::raster(raster_file)
  
  # Calculate population data
  pop <- exactextractr::exact_extract(
    pop_raster,
    shp$geometry,
    "sum"
  )
  
  # Extract country-year specific adjusting factors to align with UN pop 2024 version
  adj_factors <- estimate_adj_factors(
    country = country,
    year = year,
    raster_dir = pop_raster_path,
    dest_dir = pop_raster_path,
    country_shp = country_shp
  )
  
  pop_export <- pop * adj_factors
  
  # Check if pop is 0, if so, replace the pop with GHS population 
  if(pop_export == 0){
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
  
  return(list(
    estimated_pop = pop_export,
    adj_factors = adj_factors # also return the adjusting factors in the output
  ))
}
