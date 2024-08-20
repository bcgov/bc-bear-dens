# Script to pull out NULL or mismatched GIS verification columns

# 'f' for 'field visits'
f <- read.csv("bear_dens_visits_20240801.csv")

f <- janitor::clean_names(f)
names(f)[grep("surveyor", names(f))] <- "surveyors"

# Clean up dates
f$date_inspected <- lubridate::mdy_hms(f$date_inspected,
                                       tz = "America/Vancouver")

# TODO: delete this chunk when these columns are actually deleted in prod
# Double check the last three columns match the verification columns
# NOTE have to fill in NULL values with 99999 otherwise it will spit out NA instead of TRUE/FALSE
# verification_* columns 99999
f$verification_proportion_forested_within_60m_of_den[is.na(f$verification_proportion_forested_within_60m_of_den)] <- 99999
f$verification_distance_to_less_than_40_yr_forest[is.na(f$verification_distance_to_less_than_40_yr_forest)] <- 99999
f$verification_distance_to_greater_than_40_yr_forest[is.na(f$verification_distance_to_greater_than_40_yr_forest)] <- 99999
f$verification_distance_to_nearest_built_road[is.na(f$verification_distance_to_nearest_built_road)] <- 99999

# V* columns 99999
f$v_proportion_forested_str[is.na(f$v_proportion_forested_str)] <- 99999
f$v_distance_less40yr_forest_str[is.na(f$v_distance_less40yr_forest_str)] <- 99999
f$v_distance_grtr40year_forest_st[is.na(f$v_distance_grtr40year_forest_st)] <- 99999
f$v_distance_nearest_road_str[is.na(f$v_distance_nearest_road_str)] <- 99999

# Now actually compare
all(f$v_proportion_forested_str == f$verification_proportion_forested_within_60m_of_den, na.rm = TRUE)
all(f$v_distance_less40yr_forest_str == f$verification_distance_to_less_than_40_yr_forest)
all(f$v_distance_grtr40year_forest_st == f$verification_distance_to_greater_than_40_yr_forest)
all(f$v_distance_nearest_road_str == f$verification_distance_to_nearest_built_road)

# Now remove 99999
f[f == 99999] <- NA 

# Trim down to verification columns of interest
f <- dplyr::select(f,
                   # metadata
                   "objectid", "den_id", "sample_id", "date_inspected", "surveyors", "organization", 
                   # bed attributes
                   "bedding_cup_present", "bed_depth", "bed_width", "bed_length", 
                   # den activity attributes
                   "indicator_change_from_previous_visit", "claw_or_bite_marks",
                   "claw_or_bite_marks_location", "hair_on_entrance", "hair_in_bed", 
                   "incidental_bear_sightings_signs_scat_plug", 
                   "den_status", "den_status_rationale", 
                   "den_flagging_or_signage", "indicator_left_description", 
                   # camera attributes
                   "monitored_by_camera", "camera_details_date_s_model_notes",
                   # samples
                   "hair_samples_taken", "hair_removed_from_den_entrance", 
                   # forestry attributes
                   "forestry_treatment_description", "distance_to_nearest_tree_25cm_dbh",
                   "proportion_forested_within_60m_of_den", "verification_proportion_forested_within_60m_of_den",
                   "distance_to_less_than_40_yr_forest", "verification_distance_to_less_than_40_yr_forest",
                   "distance_to_greater_than_40_yr_forest", "verification_distance_to_greater_than_40_yr_forest",
                   "distance_to_nearest_built_road", "verification_distance_to_nearest_built_road",
                   # Misc
                   #"forestry_treatment_information_collected", # honestly can/should just use the actual forestry columns for flags
                   "nearest_road_status_or_details",
                   "x_patch_size_continguous_with_den",
                   "proportion_of_trees_windthrown_within_60m_den",
                   "horizontal_visibility", "horizontal_visibility_reason",
                   "photos_to_document_forestry_impact"
                   )

# Arrange by den + date
f <- f[order(f$den_id, f$date_inspected),]

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
f$flag_surveyors_null <- is.na(f$surveyors)

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
f$flag_icfpv_null <- is.na(f$indicator_change_from_previous_visit)

# Claw or Bite Marks is NULL
f$flag_claw_bite_null <- is.na(f$claw_or_bite_marks)

# Claw or Bite Marks Location is NULL
f$flag_claw_bite_loc_null <- is.na(f$claw_or_bite_marks_location)

# Hair on Entrance is NULL
f$flag_hair_entrance_null <- is.na(f$hair_on_entrance)

# Hair in Bed is NULL
f$flag_hair_bed_null <- is.na(f$hair_in_bed)

# Incidental Bear Sightings is NULL
f$flag_incidental_scat_null <- is.na(f$incidental_bear_sightings_signs_scat_plug)

# Den Flagging or Signage is NULL
f$flag_flagging_signage_null <- is.na(f$den_flagging_or_signage)

# Den Indicator Description is NULL (e.g. what does flagging or signage look like)
f$flag_indicator_null <- is.na(f$indicator_left_description)


#### Camera Attributes ####

# Monitored by Camera is NULL
f$flag_monitored_camera_null <- is.na(f$monitored_by_camera)

# If Monitored by Camera == YES, then camera description should be filled out
f$flag_camera_details_null <- (grepl("yes", f$monitored_by_camera, ignore.case = TRUE) & (is.na(f$camera_details_date_s_model_notes) | f$camera_details_date_s_model_notes == 999)) # WHY is there 999 in the camera details col........


#### Samples ####

# Hair Samples is NULL
f$flag_hair_samples_null <- is.na(f$hair_samples_taken)

# Hair Removed From Den Entrance is NULL
f$flag_hrfde_null <- is.na(f$hair_removed_from_den_entrance)


#### Foresty GIS verifications ####

# Forestry Treatment Description
f$flag_forestry_treatment_null <- is.na(f$forestry_treatment_description)

# Verification Proportion Forested Within 60m of Den is NULL
f$flag_vpfw60m_null <- (is.na(f$verification_proportion_forested_within_60m_of_den) | f$verification_proportion_forested_within_60m_of_den == 999 | f$verification_proportion_forested_within_60m_of_den == -999)

# Verification Distance to <40 yr forest is NULL
f$flag_vdl40yrf_null <- (is.na(f$verification_distance_to_less_than_40_yr_forest) | f$verification_distance_to_less_than_40_yr_forest == 999 | f$verification_distance_to_less_than_40_yr_forest == -999)

# Verification Distance to >40 yr forest is NULL
f$flag_vdg40yrf_null <- (is.na(f$verification_distance_to_greater_than_40_yr_forest) | f$verification_distance_to_greater_than_40_yr_forest == 999 | f$verification_distance_to_greater_than_40_yr_forest == -999)

# Verification Distance to Nearest Built Road is NULL
f$flag_vdnbr_null <- (is.na(f$verification_distance_to_nearest_built_road) | f$verification_distance_to_nearest_built_road == 999 | f$verification_distance_to_nearest_built_road == -999)

# Any verifications are NULL
f$flag_any_gis_verif_null <- rowSums(f[, grep("flag_v", names(f))]) >= 1

# All verifications are NULL
f$flag_all_gis_verif_null <- rowSums(f[, grep("flag_v", names(f))]) == 4

#### Previous year data verifications ####

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
                                              (is.na(dplyr::lag(proportion_forested_within_60m_of_den, n = 1)) | proportion_forested_within_60m_of_den == 999 | proportion_forested_within_60m_of_den == -999)),
                previous_raw_dl40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                              NA,
                                              (is.na(dplyr::lag(distance_to_less_than_40_yr_forest, n = 1)) | distance_to_less_than_40_yr_forest == 999 | distance_to_less_than_40_yr_forest == -999)),
                previous_raw_dg40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                                    NA,
                                                    (is.na(dplyr::lag(distance_to_greater_than_40_yr_forest, n = 1)) | distance_to_greater_than_40_yr_forest == 999 | distance_to_greater_than_40_yr_forest == -999)),
                previous_raw_dnbr_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                                    NA,
                                                    (is.na(dplyr::lag(distance_to_nearest_built_road, n = 1)) | distance_to_nearest_built_road == 999 | distance_to_nearest_built_road == -999)),
                # Verification forestry data
                previous_v_pfw60m_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                                  NA,
                                                  (is.na(dplyr::lag(verification_proportion_forested_within_60m_of_den, n = 1)) | verification_proportion_forested_within_60m_of_den == 999 | verification_proportion_forested_within_60m_of_den == -999)),
                previous_v_dl40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                                   NA,
                                                   (is.na(dplyr::lag(verification_distance_to_less_than_40_yr_forest, n = 1)) | verification_distance_to_less_than_40_yr_forest == 999 | verification_distance_to_less_than_40_yr_forest == -999)),
                previous_v_dg40yrf_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                                   NA,
                                                   (is.na(dplyr::lag(verification_distance_to_greater_than_40_yr_forest, n = 1)) | verification_distance_to_greater_than_40_yr_forest == 999 | verification_distance_to_greater_than_40_yr_forest == -999)),
                previous_v_dnbr_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                                NA,
                                                (is.na(dplyr::lag(verification_distance_to_nearest_built_road, n = 1)) | verification_distance_to_nearest_built_road == 999 | verification_distance_to_nearest_built_road == -999))
                ) 


# Later: can use a combination of checking if all 
# raw cols are NULL + all verification cols are NULL + 
# previous year DOES have data filled out. Could be used
# to pull out rows to fill down data with 

#### Misc other cols ####

# Distance to Nearest Tree 25cm DBH 
f$flag_dnt25cm_null <- (is.na(f$distance_to_nearest_tree_25cm_dbh) | f$distance_to_nearest_tree_25cm_dbh == 999 | f$distance_to_nearest_tree_25cm_dbh == -999)

# X Patch Size Contiguous With Den
f$flag_x_patch_size_null <- is.na(f$x_patch_size_continguous_with_den | f$x_patch_size_continguous_with_den == 999 | f$x_patch_size_continguous_with_den == -999)

# Proportion of Trees Windthrown Within 60m of Den
f$flag_ptww60m_null <- (is.na(f$proportion_of_trees_windthrown_within_60m_den) | f$proportion_of_trees_windthrown_within_60m_den == 999 | f$proportion_of_trees_windthrown_within_60m_den == -999)

# Horizontal Visibility
f$flag_hv_null <- (is.na(f$horizontal_visibility) | f$horizontal_visibility == 999 | f$horizontal_visibility == -999)

# Misc other columns previous year checks
f|>
  dplyr::group_by(den_id) |>
  dplyr::mutate(# Misc cols
    previous_dnt25cm_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                   NA,
                                   (is.na(dplyr::lag(distance_to_nearest_tree_25cm_dbh, n = 1)) | distance_to_nearest_tree_25cm_dbh == 999 | distance_to_nearest_tree_25cm_dbh == -999)),
    previous_ptww60m_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                   NA,
                                   (is.na(dplyr::lag(proportion_of_trees_windthrown_within_60m_den, n = 1)) | proportion_of_trees_windthrown_within_60m_den == 999 | proportion_of_trees_windthrown_within_60m_den == -999)),
    previous_hv_null = ifelse(cumulative_visit == 1, # if it's the first visit to the den, set this column to == NA
                                   NA,
                                   (is.na(dplyr::lag(horizontal_visibility, n = 1)) | horizontal_visibility == 999 | horizontal_visibility == -999)),
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


# Nearest road status is NULL
f$flag_nearest_road_null <- is.na(f$nearest_road_status_or_details)

# Horizonal Visibility Reason is NULL
f$flag_hvr_null <- is.na(f$horizontal_visibility_reason)

# Photos to Document Forestry Impact is NULL
f$flag_forestry_photos_null <- is.na(f$photos_to_document_forestry_impact)


#### Forestry Raw-Verification Values Mismatch ####

# First calc absolute value difference between the raw and verified
f$diff_pfw60m <- abs(f$verification_proportion_forested_within_60m_of_den - f$proportion_forested_within_60m_of_den)

f$diff_dl40yrf <- abs(f$verification_distance_to_less_than_40_yr_forest - f$distance_to_less_than_40_yr_forest)

f$diff_dg40yrf <- abs(f$verification_distance_to_greater_than_40_yr_forest - f$distance_to_greater_than_40_yr_forest)

f$diff_dnbr <- abs(f$verification_distance_to_nearest_built_road - f$distance_to_nearest_built_road)

# Now flag records where the verification value is off by 10m+ OR by 30%
# of the original value
cutoff_distance <- 10 # in case we want to change these values later
cutoff_percent <- 0.3 

f$flag_diff_pfw60m <- (f$diff_pfw60m >= cutoff_distance | (f$diff_pfw60m / f$proportion_forested_within_60m_of_den) >= cutoff_percent)

f$flag_diff_dl40yrf <- (f$diff_dl40yrf >= cutoff_distance | (f$diff_dl40yrf / f$distance_to_less_than_40_yr_forest) >= cutoff_percent)

f$flag_diff_dg40yrf <- (f$diff_dg40yrf >= cutoff_distance | (f$diff_dg40yrf / f$distance_to_greater_than_40_yr_forest) >= cutoff_percent)

f$flag_diff_dnbr <- (f$diff_dnbr >= cutoff_distance | (f$diff_dnbr / f$distance_to_nearest_built_road) >= cutoff_percent)

