# Functions to push local data to live ArcGIS Online bear dens data

to_esri_json <- function(df, pretty = FALSE) {
  geom_tf <- any(grepl("geometry", names(df), ignore.case = TRUE))
  
  # Build ESRI compatible JSON
  # https://developers.arcgis.com/rest/services-reference/enterprise/update-features/
  if (geom_tf) {
    # Build toJSON dataframe with geometry
    out <- data.frame(matrix(NA, nrow = nrow(df), ncol = 2)) |>
      setNames(c("attributes", "geometry")) |>
      data.frame()
    
    out$geometry <- sf::st_coordinates(df) |> 
      as.data.frame() |> 
      setNames(c("x", "y"))
    
    out$attributes <- sf::st_drop_geometry(df)
    
  } else {
    # Build toJSON dataframe without geometry
    out <- data.frame(matrix(NA, nrow = nrow(df), ncol = 2)) |>
      setNames("attributes") |>
      data.frame()
    
    out$attributes <- df
    
  }
  
  # Convert to JSON
  out <- jsonlite::toJSON(out, pretty = pretty)
  
  return(out)

}


push_bears <- function(json, layer, token) {
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
  # Headers
  headers = c('Content-Type' = 'application/x-www-form-urlencoded')
  # Base URL
  url <- paste("https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services", feature_name, "FeatureServer", lyr_id, "updateFeatures", sep = "/")
  # Build body
  body = list('f' = 'json',
              'features' = json,
              'token' = token)
  # Send request
  # e.g. https://services6.arcgis.com/ubm4tcTYICKBpist/arcgis/rest/services/Bear_Dens_Updated_23/FeatureServer/6/query?where1=1?f=pjson&token=<token>
  request <- httr::VERB("POST", 
                        url = url, 
                        body = body, 
                        httr::add_headers(headers), 
                        encode = 'form')
  # Push data & return HTTPS result
  out <- jsonlite::fromJSON(httr::content(request, 'text'))
  return(out)
}




