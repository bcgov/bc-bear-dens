
# Download and process BC DEM data

#### QUERY CDED ####

# Get the DEM data for HG and VI

query_cded <- function(regions, output_dir) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Download it
  out <- bcmaps::cded(aoi = regions,
                      dest_vrt = file.path(output_dir, "CDED_VRT.vrt"))
  return(out)
}


#### PROCESS DEM ####

extract_elevation <- function(dens, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform dens data to match DEM CRS
  dens <- sf::st_transform(dens, cded_epsg)
  # Extract elevation
  out <- terra::extract(cded, dens, ID = FALSE)
  # Return out
  out <- cbind(dens$den_id, out)
  names(out) <- c("den_id", "elevation_m")
  return(out)
}


extract_slope <- function(dens, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform dens data to match DEM CRS
  dens <- sf::st_transform(dens, cded_epsg)
  # Transform DEM to slope
  slope <- terra::terrain(cded, v = "slope", unit = "radians")
  # Extract slope
  out <- terra::extract(slope, dens, ID = FALSE)
  # Convert from degrees to %
  out$slope_pct <- tan(out$slope)
  # Return out
  out <- cbind(dens$den_id, out)
  names(out) <- c("den_id", "slope_rad", "slope_pct")
  out <- out[,c("den_id", "slope_pct")]
  return(out)
}


extract_aspect <- function(dens, cded_path) {
  # Load up CDED VRT
  cded <- terra::rast(cded_path)
  # Extract CRS
  cded_crs <- terra::crs(cded)
  cded_epsg <- stringr::str_extract(cded_crs, "EPSG.*$") |> 
    stringr::str_extract(pattern = "\\d+") |>
    as.numeric()
  # Transform dens data to match DEM CRS
  dens <- sf::st_transform(dens, cded_epsg)
  # Transform DEM to aspect
  aspect <- terra::terrain(cded, v = "aspect", unit = "degrees")
  # Extract aspect
  out <- terra::extract(aspect, dens, ID = FALSE)
  # Return out
  out <- cbind(dens$den_id, out)
  names(out) <- c("den_id", "slope_aspect")
  return(out)
}


# Wrap up all 3
extract_dem <- function(dens, cded_path) {
  message("Extracting elevation...")
  elev <- extract_elevation(dens, cded_path)
  message("Extracting slope...")
  slope <- extract_slope(dens, cded_path)
  message("Extracting aspect...")
  aspect <- extract_aspect(dens, cded_path)
  
  # Merge all 3
  out <- merge(elev, slope, all = TRUE)
  out <- merge(out, aspect, all = TRUE)
  
  # Clean up
  out$slope_pct <- round(out$slope_pct * 100, 1)
  out$slope_aspect <- round(out$slope_aspect, 0)
  
  return(out)
}