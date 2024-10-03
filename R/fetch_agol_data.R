# Functions to connect to ArcGIS Online Bear Dens project

fetch_bears <- function(layer = "field visits", token) {
  stopifnot("`layer` must be one of 'current', 'field visits', or 'potential'." = layer %in% c("current", "field visits", "potential"))
  # there is likely a better way of doing this, but for now - 
  feature_name <- dplyr::case_when(
    layer == "field visits" ~ "Bear_Dens_Updated_23",
    layer == "current" ~ "Bear_Dens_Updated_23",
    layer == "potential" ~ "Bear_Den_Potential"
  )
  lyr_id <- dplyr::case_when(
    layer == "field visits" ~ 6,
    layer == "current" ~ 1,
    layer == "potential" ~ 0
  )
  # Base URl
  url <- httr::parse_url("https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services")
  # Build query url
  url$path <- paste(url$path, feature_name, "FeatureServer", lyr_id, "query", sep = "/")
  url$query <- list(where = "1 = 1",
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson",
                    token = token)
  request <- httr::build_url(url) # e.g. https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/Bear_Dens_Updated_23/FeatureServer/6/query?where1=1?f=pjson&token=<token>
  # Pull data
  bears <- sf::st_read(request)
  return(bears)
}


backup_bears <- function(dens, f, p, path = "temp/Backups") {
  # Store to `temp` folder as a data backup
  systime <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  # Create backup folder
  path <- file.path(path, systime)
  dir.create(path, recursive = TRUE)
  
  # Current
  sf::st_write(dens, paste0(path, "/dens_current_", systime, ".gpkg"))
  
  # Field visits
  write.csv(f, paste0(path, "/dens_visits_", systime, ".csv"),
            row.names = FALSE,
            na = "")
  
  # Potential
  sf::st_write(p, paste0(path, "/dens_potential_", systime, ".gpkg"))
}

pull_den_years <- function(x, date_col = "date_inspected") {
  out <- sort(unique(lubridate::year(x[["date_inspected"]])))
  return(out)
}