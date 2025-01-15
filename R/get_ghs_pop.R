# Helper function to find the nearest years ending in 0 or 5
find_nearest_years <- function(
    target_year) {
  available_years <- seq(1900, 2100, by = 5) # Modify based on available rasters
  lower_year <- max(available_years[available_years <= target_year])
  upper_year <- min(available_years[available_years >= target_year])
  return(c(lower_year, upper_year))
}

# Function to load rasters for the nearest years
load_raster <- function(
    year, 
    raster_dir) {
  # Raster files need to be manually downloaded from this website: https://human-settlement.emergency.copernicus.eu/download.php
  file_name <- paste0("GHS_POP_E",year,"_GLOBE_R2023A_4326_3ss_V1_0.tif")
  file_path <- file.path(raster_dir, file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("Raster file for year", year, "not found in directory:", raster_dir))
  }
  
  return(raster::raster(file_path))
}

# Function to estimate yearly GHS population
#' Estimate yearly population based on geometry and GHS population rasters.
#' @title get_ghs_pop
#' @export
#' @param shp sf object: geometries.
#' @param years numeric vector: Years for which population needs to be estimated.
#' @param raster_dir string: Directory containing the downloaded GHS raster files.
#' @param crs numeric: Coordinate reference system for the geometry (default: EPSG 4326).
#'
#' @return A dataframe with target year and estimated population.
get_ghs_pop <- function(
    shp, 
    years, 
    raster_dir, 
    crs = 4326) {
  # Process each target year
  output <- lapply(years, function(target_year) {
    # Find the nearest years ending in 0 or 5
    nearest_years <- find_nearest_years(target_year)
    
    # Load rasters for the nearest years
    rasters <- lapply(nearest_years, function(year) {
      load_raster(year, raster_dir)
    })
    
    # Extract population for both years
    populations <- sapply(rasters, function(raster) {
      exactextractr::exact_extract(raster, shp, "sum")
    })
    
    # Ensure populations is a matrix
    if (is.vector(populations)) {
      populations <- matrix(populations, ncol = length(nearest_years), byrow = TRUE)
    }
    
    # Interpolate population for the target year
    interpolated_population <- apply(populations, 1, function(pop) {
      approx(
        x = nearest_years,
        y = pop,
        xout = target_year,
        rule = 2
      )$y
    })
    
    # Create a dataframe for this target year
    data.frame(
      target_year = target_year,
      estimated_population = interpolated_population
    )
  })
  
  # Combine results into a single dataframe
  output <- do.call(rbind, output)
  
  return(output)
}