# Simple cleaning of bear dens data. Fixes excess whitespace, 'Na' strings,
# common typos, inconsistent cases, etc.

clean_bears <- function(df) {
  # Clean up colnames
  df <- janitor::clean_names(df)
  names(df) <- gsub("i_d", "id", names(df)) # fix any cases of janitor splitting 'IDs' into 'i_ds'
  
  # Determine which of the three den layers it is based on
  # columns unique to each table
  layer <- dplyr::case_when("den_type" %in% names(df) ~ "current",
                            "sample_id" %in% names(df) ~ "field visits",
                            "external_den_name" %in% names(df) ~ "potential",
                            TRUE ~ "unknown")
  
  # Drop geometry for field visits
  if (layer == "field visits") df <- sf::st_drop_geometry(df)
  
  # Clean up excess whitespace, abbreviations, NA, etc.
  df <- df |> dplyr::mutate_if(is.character, stringr::str_squish)
  df <- df |> dplyr::mutate_if(is.character, stringr::str_replace_all, 
                               "\\bukn\\b|\\bUkn\\b|\\bUKN\\b|\\bunk\\b|\\bUnk\\b|\\bUNK\\b|\\bunknown\\b", 
                               "Unknown")
  
  # Deal with NAs. Easier to do individual mutates for sf dataframes
  df <- df |> dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"")))
  df <- df |> dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"na")))
  df <- df |> dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"Na")))
  df <- df |> dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"NA")))
  
  # Clean up current layer
  if (layer == "current") {
    # Clean up title case
    df <- df |> dplyr::mutate_at(c("district", "landscape_unit", "x_f_ownership"),
                                     tools::toTitleCase)
    df <- df |> dplyr::mutate_at(c("den_tree_species"), snakecase::to_title_case) # tools vs snakecase handle acronyms differently
    
    # Clean up upper case
    df$x_bec_zone <- ifelse(df$x_bec_zone == "Unknown", 
                              df$x_bec_zone, 
                              toupper(df$x_bec_zone))
    df$x_bec_subzone_variant <- ifelse(df$x_bec_subzone_variant == "Unknown", 
                                         df$x_bec_subzone_variant, 
                                         toupper(df$x_bec_subzone_variant))
    
    # Clean up numerics
    df$entrance_height_functional <- as.numeric(df$entrance_height_functional)
    
    # Replace any instances of words as  needed
    df$landscape_unit <- gsub("Skidigate", "Skidegate", df$landscape_unit)
    df$den_tree_species <- gsub("Red Cedar", "Cw", df$den_tree_species)
  }
  
  # Clean up field visits layer
  if (layer == "field visits") {
    # Clean up date_inspected column for R
    # Note all the other date columns are in genuine UTC
    df$date_inspected <- lubridate::as_datetime(df$date_inspected/1000) |> lubridate::force_tz("America/Vancouver")
    
    # Clean up title case
    df <- df |> dplyr::mutate_at(c("surveyor", "bedding_cup_present", "claw_bite_marks",
                                   "hair_on_entrance", "hair_in_bed", "hair_samples_taken",
                                   "hair_removed_den_entrance", "monitored_by_camera",
                                   "photos_forestry_impact"), 
                                 tools::toTitleCase)
    
    # Clean up upper case
    df$organization <- gsub("bcgov|B.C. Gov|B.C.Gov|BC Gov", "BCGOV", df$organization, ignore.case = TRUE)
    df$organization <- gsub("chn", "CHN", df$organization, ignore.case = TRUE)
    df$organization <- gsub("\\bflnr\\b|\\bflnrord\\b", "FLNRO", df$organization, ignore.case = TRUE)
    df$organization <- gsub("\\bmof\\b", "MoF", df$organization, ignore.case = TRUE)
    df$organization <- gsub("\\sand\\s|/", ", ", df$organization, ignore.case = TRUE)
    
    # Misc cols cleanup
    df$hair_samples_taken <- gsub("\\bn\\b", "No", df$hair_samples_taken, ignore.case = TRUE)
    
  }
  
  if (layer == "potential") {
    # Clean up title case
    df <- df |> dplyr::mutate_at(c("district", "landscape_unit", "potential_for_future_use",
                                   "report_on_file", "licensee_map_on_file", "photos_on_file"),
                                 tools::toTitleCase)
    
    # Clean up date_bc_gov_assessed column for R
    # Note all the other datetime columns are in genuine UTC
    df$date_bc_gov_assessed <- lubridate::as_datetime(df$date_bc_gov_assessed/1000) |> lubridate::force_tz("America/Vancouver")
    
  }
  
  return(df)

}
