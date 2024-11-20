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
    df$den_tree_species <- gsub("Fed", "Fd", df$den_tree_species)
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


# Perform non-forestry verification of the field visits table
# and output a table of flagged dens/sample ids
verify_bears <- function(dens, f) {
  # Trim dens down to verification columns of interest
  dens <- dplyr::select(dens, den_id, den_state)
  
  # Den ID is unique
  dens$flag_den_id_duplicated <- dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)]
  
  # Den status is not NULL
  dens$flag_den_status_null <- is.na(dens$den_state)
  
  # Trim field visits down to verification columns of interest
  f <- dplyr::select(f,
                     # metadata
                     "objectid", "den_id", "sample_id", "date_inspected", "surveyor", "organization", 
                     # bed attributes
                     "bedding_cup_present", "bed_depth", "bed_width", "bed_length", 
                     # den activity attributes
                     "indicator_chg_from_prev_visit", "claw_bite_marks",
                     "claw_bite_marks_location", "hair_on_entrance", "hair_in_bed", 
                     "incidental_bear_sign", 
                     "den_status", "den_status_rationale",
                     "den_flagging_or_signage", "indicator_left_description", 
                     # camera attributes
                     "monitored_by_camera", "camera_details",
                     # samples
                     "hair_samples_taken", "hair_removed_den_entrance", 
                     # forestry attributes
                     "forestry_treatment_desc", "distance_nearest_tree_field",
                     "proportion_forested_field", "proportion_forested",
                     "distance_less40yr_forest_field", "v_distance_less40yr_forest",
                     "distance_grtr40yr_forest_field", "v_distance_grtr40year_forest",
                     "distance_nearest_road", "v_distance_nearest_road",
                     # Misc
                     "nearest_road_status",
                     "x_patch_size_contiguous_den",
                     "proportion_tree_windthrown", 
                     "horizonal_visibility", "horizonal_visibility_reason",
                     "photos_forestry_impact")
  
  # Arrange by den + date
  f <- f[order(f$den_id, f$date_inspected),]
  
  # Merge with dens
  f <- merge(f, dens, by = "den_id", all = TRUE)
  
  # FLAGS
  # Now actually flag the bad records. 
  
  # Note: 999s = confirmed 'NA' or 'inaccessible' (e.g., bear den was too high 
  # off the ground to measure, but still exists), while a blank field indicates 
  # a person likely never actually got around to filling it in. NO, 999s cannot be 
  # bulk replaced. That said, some years 999s were used as placeholders that
  # meant 'no change', i.e. use previous years' data. Here they need to be
  # manually sniffed out and fixed. 
  # There should be NO 999s and NO NULLS in the forestry columns. 
  
  #### Metadata flags ####
  
  # Den ID is NULL
  f$flag_den_id_null <- is.na(f$den_id)
  
  # Den Sample ID is unique
  f$flag_sample_id_duplicated <- f$sample_id %in% f[["sample_id"]][duplicated(f$sample_id)]
  
  # Sample ID is NULL
  f$flag_sample_id_null <- is.na(f$sample_id)
  
  # Date is valid
  f$flag_date_invalid <- is.na(f$date_inspected)
  
  # Surveyors is NULL
  f$flag_surveyor_null <- is.na(f$surveyor)
  
  # Organization is NULL
  f$flag_organization_null <- is.na(f$organization)
  
  #### Bear Bed Metrics ####
  
  # Bedding Cup is NULL
  # First slightly clean up bedding cup values
  f$bedding_cup_present <- ifelse(f$bedding_cup_present %in% c("Yes", "Yes - Unchanged"),
                                  "Yes",
                                  ifelse(f$bedding_cup_present == "No",
                                         "No",
                                         ifelse(f$bedding_cup_present == "",
                                                NA,
                                                f$bedding_cup_present))
  )
  f$flag_bcp_null <- is.na(f$bedding_cup_present)
  
  # Bed Depth, Bed Width, Bed Length is NULL while Bedding Cup Present == "Yes"
  # Q: What to do with 999s/-999s?
  # A: 999 typically indicates that the bed was inaccessible (e.g., high up arboreal
  #    den). So 999s in this column can be left, but need to be confirmed w the 
  #    comments column that they are ok to stay as 999.
  f$flag_bd_null <- (is.na(f$bed_depth) & ((f$bedding_cup_present == "Yes") | f$flag_bcp_null == TRUE))
  f$flag_bw_null <- (is.na(f$bed_width) & ((f$bedding_cup_present == "Yes") | f$flag_bcp_null == TRUE))
  f$flag_bl_null <- (is.na(f$bed_length) & ((f$bedding_cup_present == "Yes") | f$flag_bcp_null == TRUE))
  
  # All bedding attributes NULL
  # This assumes that 'bedding cup present' == "Yes" OR is NULL,
  # based on logic of above three flags
  f$flag_all_bedding_null <- rowSums(f[, grep("flag_bd|flag_bw|flag_bl", names(f))]) == 3
  
  #### Bear Den Activity Metrics ####
  
  # Den Status is NULL
  f$flag_den_status_null <- is.na(f$den_status)
  
  # Den Status Rationale is NULL
  f$flag_den_status_r_null <- is.na(f$den_status_rationale)
  
  # Indicator Change From Previous Visit is NULL
  f$flag_icfpv_null <- is.na(f$indicator_chg_from_prev_visit)
  
  # Claw or Bite Marks is NULL
  f$flag_claw_bite_null <- is.na(f$claw_bite_marks)
  
  # Claw or Bite Marks Location is NULL
  f$flag_claw_bite_loc_null <- is.na(f$claw_bite_marks_location)
  
  # Hair on Entrance is NULL
  f$flag_hair_entrance_null <- is.na(f$hair_on_entrance)
  
  # Hair in Bed is NULL
  f$flag_hair_bed_null <- is.na(f$hair_in_bed)
  
  # Incidental Bear Sightings is NULL
  f$flag_incidental_sign_null <- is.na(f$incidental_bear_sign)
  
  # Den Flagging or Signage is NULL
  f$flag_flagging_signage_null <- is.na(f$den_flagging_or_signage)
  
  # Den Indicator Description is NULL (e.g. what does flagging or signage look like)
  f$flag_indicator_null <- is.na(f$indicator_left_description)
  
  
  #### Camera Attributes ####
  
  # Monitored by Camera is NULL
  f$flag_monitored_camera_null <- is.na(f$monitored_by_camera)
  
  # If Monitored by Camera == YES, then camera description should be filled out
  f$flag_camera_details_null <- (grepl("yes", f$monitored_by_camera, ignore.case = TRUE) & (is.na(f$camera_details) | f$camera_details == 999)) # WHY is there 999 in the camera details col........
  
  
  #### Samples ####
  
  # Hair Samples is NULL
  f$flag_hair_samples_null <- is.na(f$hair_samples_taken)
  
  # Hair Removed From Den Entrance is NULL
  f$flag_hrfde_null <- is.na(f$hair_removed_den_entrance)
  
  
  #### Raw forestry ####
  
  # Forestry Treatment Description
  f$flag_forestry_treatment_null <- is.na(f$forestry_treatment_desc)
  
  # All raw forestry data is NULL
  f$flag_all_raw_forest_null <- 4 == (is.na(f$proportion_forested_field) + is.na(f$distance_less40yr_forest_field) + is.na(f$distance_grtr40yr_forest_field) + is.na(f$distance_nearest_road))
  
  
  #### Foresty GIS verifications ####
  
  # Verification Proportion Forested Within 60m of Den is NULL
  f$flag_vpfw60m_null <- (is.na(f$proportion_forested) | f$proportion_forested == 999 | f$proportion_forested == -999)
  
  # Verification Distance to <40 yr forest is NULL
  f$flag_vdl40yrf_null <- (is.na(f$v_distance_less40yr_forest) | f$v_distance_less40yr_forest == 999 | f$v_distance_less40yr_forest == -999)
  
  # Verification Distance to >40 yr forest is NULL
  f$flag_vdg40yrf_null <- (is.na(f$v_distance_grtr40year_forest) | f$v_distance_grtr40year_forest == 999 | f$v_distance_grtr40year_forest == -999)
  
  # Verification Distance to Nearest Built Road is NULL
  f$flag_vdnbr_null <- (is.na(f$v_distance_nearest_road) | f$v_distance_nearest_road == 999 | f$v_distance_nearest_road == -999)
  
  # Any verifications are NULL
  f$flag_any_gis_verif_null <- rowSums(f[, grep("flag_v", names(f))]) >= 1
  
  # All verifications are NULL
  f$flag_all_gis_verif_null <- rowSums(f[, grep("flag_v", names(f))]) == 4
  
  
  #### Previous year data verifications ####
  
  # Can use a combination of checking if all 
  # raw cols are NULL + all verification cols are NULL + 
  # previous year DOES have data filled out
  # to pull out rows to fill down data with 
  
  # Calculate how many cumulative visits have been made to a den
  f <- f |>
    dplyr::group_by(den_id) |>    
    dplyr::mutate(cumulative_visit = cumsum(!is.na(den_id)))
  
  # And total visits
  f <- f |>
    dplyr::group_by(den_id) |>
    dplyr::mutate(total_visits = dplyr::n())
  
  # Check if previous year has data filled out, grouped by Den ID
  # Note the nature of this lag function is such that it would need
  # to be looped until no more NULLs are found for each den grouping,
  # as it only scans one row previous to it
  f <- f|>
    dplyr::group_by(den_id) |>
    dplyr::mutate(# Raw forestry data
      previous_raw_pfw60m_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                        NA,
                                        (is.na(dplyr::lag(proportion_forested_field, n = 1)) | proportion_forested_field == 999 | proportion_forested_field == -999)),
      previous_raw_dl40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                         NA,
                                         (is.na(dplyr::lag(distance_less40yr_forest_field, n = 1)) | distance_less40yr_forest_field == 999 | distance_less40yr_forest_field == -999)),
      previous_raw_dg40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                         NA,
                                         (is.na(dplyr::lag(distance_grtr40yr_forest_field, n = 1)) | distance_grtr40yr_forest_field == 999 | distance_grtr40yr_forest_field == -999)),
      previous_raw_dnbr_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                      NA,
                                      (is.na(dplyr::lag(distance_nearest_road, n = 1)) | distance_nearest_road == 999 | distance_nearest_road == -999)),
      # Verification forestry data
      previous_v_pfw60m_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                      NA,
                                      (is.na(dplyr::lag(proportion_forested, n = 1)) | proportion_forested == 999 | proportion_forested == -999)),
      previous_v_dl40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                       NA,
                                       (is.na(dplyr::lag(v_distance_less40yr_forest, n = 1)) | v_distance_less40yr_forest == 999 | v_distance_less40yr_forest == -999)),
      previous_v_dg40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                       NA,
                                       (is.na(dplyr::lag(v_distance_grtr40year_forest, n = 1)) | v_distance_grtr40year_forest == 999 | v_distance_grtr40year_forest == -999)),
      previous_v_dnbr_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                    NA,
                                    (is.na(dplyr::lag(v_distance_nearest_road, n = 1)) | v_distance_nearest_road == 999 | v_distance_nearest_road == -999))
    )
  
  
  #### Misc other cols ####
  
  # Distance to Nearest Tree 25cm DBH 
  f$flag_dnt25cm_null <- (is.na(f$distance_nearest_tree_field) | f$distance_nearest_tree_field == 999 | f$distance_nearest_tree_field == -999)
  
  # X Patch Size Contiguous With Den
  f$flag_x_patch_size_null <- is.na(f$x_patch_size_contiguous_den | f$x_patch_size_contiguous_den == 999 | f$x_patch_size_contiguous_den == -999)
  
  # Proportion of Trees Windthrown Within 60m of Den
  f$flag_windthrow_null <- (is.na(f$proportion_tree_windthrown) | f$proportion_tree_windthrown == 999 | f$proportion_tree_windthrown == -999)
  
  # Nearest road status is NULL
  f$flag_road_status_null <- is.na(f$nearest_road_status)
  
  # Horizontal Visibility
  f$flag_hv_null <- (is.na(f$horizonal_visibility) | f$horizonal_visibility == 999 | f$horizonal_visibility == -999)
  
  # Horizonal Visibility Reason is NULL
  f$flag_hvr_null <- is.na(f$horizonal_visibility_reason)
  
  # Photos
  f$flag_photos_null <- is.na(f$photos_forestry_impact)
  
  # Misc other columns previous year checks
  f <- f |>
    dplyr::group_by(den_id) |>
    dplyr::mutate(# Misc cols
      previous_dnt25cm_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                     NA,
                                     (is.na(dplyr::lag(distance_nearest_tree_field, n = 1)) | distance_nearest_tree_field == 999 | distance_nearest_tree_field == -999)),
      previous_windthrow_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                     NA,
                                     (is.na(dplyr::lag(proportion_tree_windthrown, n = 1)) | proportion_tree_windthrown == 999 | proportion_tree_windthrown == -999)),
      previous_hv_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                NA,
                                (is.na(dplyr::lag(horizonal_visibility, n = 1)) | horizonal_visibility == 999 | horizonal_visibility == -999)),
      # Bear den cols | It's ok for bear bed metrics to be 999
      previous_bd_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                NA,
                                is.na(dplyr::lag(bed_depth, n = 1))),
      previous_bw_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                NA,
                                is.na(dplyr::lag(bed_width, n = 1))), 
      previous_bl_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                NA,
                                is.na(dplyr::lag(bed_length, n = 1)))
    )
  
  #### OUTPUT ####
  
  # Don't need to return all the data iself - just the flags table.
  f <- dplyr::select(f, den_id, sample_id, dplyr::starts_with("flag"))
  return(f)
  
}
