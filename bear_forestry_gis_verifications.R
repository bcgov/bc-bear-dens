#### SET UP ####

# Recommended to first allocate more memory to R, then restart R.
# R_MAX_VSIZE=32Gb
usethis::edit_r_environ()

# Set token
# Token seems to expire anytime your ArcGIS Online session expires -
# seems to be roughly ~30 mins.
# Even though it expires super quickly, better practice to isolate token
# value in some file that's not tracked by git
source("temp/token.R")

# Load functions
source("R/fetch_agol_data.R")
source("R/clean_bear_data.R")
source("R/forestry_gis.R")

# Fetch bear data

# `dens` for 'current' data
dens <- fetch_bears(token = token,
                    layer = "current")

# `f` for 'field visits' data
f <- fetch_bears(token = token,
                 layer = "field visits")

# Data backups
# Store to `temp` folder as a data backup
systime <- format(Sys.time(), "%Y%m%d-%H%M%S")

# Create backup dir
dir.create(paste0("temp/Backups/", systime), recursive = TRUE)

# Current
sf::st_write(dens, paste0("temp/Backups/", systime, "/dens_current_", systime, ".gpkg"))

# Field visits
write.csv(f, paste0("temp/Backups/", systime, "/dens_visits_", systime, ".csv"),
          row.names = FALSE,
          na = "")

rm(systime)

# Keep original column names stored 
# Later will use these original column names to push data changes to 
# online layer
dens_cols <- names(dens)
f_cols <- names(f)

#### GENERAL CLEANUP ####
dens <- clean_bears(dens)
f <- clean_bears(f)

dens <- sf::st_transform(dens, 3005)

#### MERGE ####

# 2024-09-12 For now, drop SAN_FishermanRiver_1 because it's duplicated
# in the dens table.
dens <- dens[dens$den_id != "SAN_FishermanRiver_1",]
f <- f[f$den_id != "SAN_FishermanRiver_1",]

# Merge and throw warning if the number of records changes (this indicates
# duplicated den IDs in the dens table).
f_full <- merge(dens, f, by = "den_id")

if (nrow(f) != nrow(f_full)) stop("WARNING: You have duplicated den IDs in the dens table!!")

#### LOAD GIS LAYERS ####

# Define function to read in large WKT csv files
read_lrg_wkt <- function(filepath, wkt_col = "WKT_GEOM", crs = 3005) {
  x <- arrow::read_csv_arrow(filepath)
  x <- as.data.frame(x)
  x <- sf::st_as_sf(x, wkt = wkt_col)
  sf::st_crs(x) <- crs
  # Maybe drop the giant WKT column - don't need it anymore?
  return(x)
}

# > VRI ----

vri_hg <- read_lrg_wkt("GIS/HG_VEG_COMP_LYR_L1_POLY_202409060839.csv")
vri_vi <- read_lrg_wkt("GIS/arrow_VI_VEG_COMP_LYR_R1_POLY_202409061234.csv")

vri <- dplyr::bind_rows(vri_hg, vri_vi)
rm(vri_hg, vri_vi)

gc()

vri <- janitor::clean_names(vri)


# > Depletions ----

regions <- sf::st_read("GIS/regions_wkt.csv")
sf::st_crs(regions) <- 3005

deps <- sf::st_read("../../GIS/BC/Depletions/2023_Depletions.gpkg")
deps <- sf::st_intersection(deps, regions)
deps <- janitor::clean_names(deps)

# Drop giant wkt column
deps <- deps[,!(names(deps) %in% 'wkt')]

rm(regions)


#### CREATE FORESTRY VERIFICATION LAYERS ####

den_years <- unique(lubridate::year(f_full$date_inspected)) |> sort()

# Determine how many points there are per year
# For years w <10 points per year, it might just be faster to do it manually
plyr::count(lubridate::year(f_full$date_inspected)) |> dplyr::arrange(freq)

# To save on memory space, do NOT create forestry verification layers (FVL)
# for each year and store them in a list. Instead, create the layer for
# a given year, then pull out the GIS data (distance to nearest tree, etc.)
# and store THAT in memory, then wipe that year and do the next year.
dir.create(paste0("GIS/FVL"), recursive = TRUE)

# Loop through bear den years, create FVL, and save it
# Start time 12:14 pm, end time 1:22
lapply(den_years, function(x) {
  message(x, " start ", Sys.time())
  fvl <- create_fvl(den_year = x,
                    vri = vri,
                    depletions = deps)
  sf::st_write(fvl, paste0("GIS/FVL/FVL_", x, ".gpkg")) # Save each year's FVL 
  message(x, " end ", Sys.time())
})

# Nice.
rm(vri, deps) # Remove big files we no longer need
gc() # Clean up memory


#### RUN VERIFICATION CHECKS! ####

# For each given year, run the three primary forestry GIS verifications!


f_full <- f_full[,c("objectid.y", "den_id", "sample_id", "date_inspected", "geometry")]
# TODO: This is a terrible way of doing this, please fix 
f_full$prop_forested_60m <- NA
f_full$dist_lt40 <- NA
f_full$dist_gt40 <- NA
f_full$dist_road <- NA

# Start 3:07 pm
f_x_list <- lapply(den_years, function(x) {
  # Subset dens to given year
  f_x <- f_full[lubridate::year(f_full$date_inspected) == x,]
  # Load up relevant FVL
  fvl <- sf::st_read(paste0("GIS/FVL/FVL_", x, ".gpkg"))
  fvl <- sf::st_collection_extract(fvl)
  # For each feature at a time, run the verification check
  # TODO: definitely improve/vectorize this in the future....
  for (i in 1:nrow(f_x)) {
    message("Running checks for ", f_x[["sample_id"]][i])
    feature <- f_x[i,]
    # > Proportion forested within 60m ----
    f_x[["prop_forested_60m"]][i] <- st_proportion_forested(feature = feature, fvl = fvl)
    # > Distance to <40 yo forest ----
    f_x[["dist_lt40"]][i] <- suppressMessages(nngeo::st_nn(feature, fvl[fvl$forested == 'Non-Forested',], returnDist = T, progress = F)$dist[[1]])
    # > Distance to >40yo forest ----
    f_x[["dist_gt40"]][i] <- suppressMessages(nngeo::st_nn(feature, fvl[fvl$forested == 'Forested',], returnDist = T, progress = F)$dist[[1]])
  }
  message("Finished ", x, " at ", Sys.time())
  return(f_x)
})

f_x <- dplyr::bind_rows(f_x_list) # Inspect f_x
f_full <- f_x # Overwrite f_full if all looks good
rm(f_x_list, f_x)

# Save just in case
write.csv(f_x, "temp/Backups/20240912-114551/forestry_gis_verifications.csv",
          na = "",
          row.names = FALSE)

# > Distance to nearest road ----

# Load up roads data
trans_roads <- read_lrg_wkt("GIS/HG_VI_roads.csv")
forest_roads <- read_lrg_wkt("GIS/HG_VI_forestry_sections.csv")

trans_roads <- janitor::clean_names(trans_roads)
forest_roads <- janitor::clean_names(forest_roads)

roads <- dplyr::bind_rows(trans_roads, forest_roads)
rm(trans_roads, forest_roads)

# Start 10:03 am end 10:09 am 
f_x_list <- lapply(den_years, function(x) {
  # Subset dens to given year
  f_x <- f_full[lubridate::year(f_full$date_inspected) == x,]
  # Load up relevant road for that year
  message("Loading roads for ", x)
  roads_x <- roads[which((roads$award_date <= x | is.na(roads$award_date)) & (roads$retirement_date >= x | is.na(roads$retirement_date) | roads$life_cycle_status_code == 'ACTIVE')),]
  # For each year, run verification check
  roads_dist <- unlist(st_distance_nearest_road(feature = f_x,
                                                roads = roads_x,
                                                filter_by_date = FALSE))
  f_x$dist_road <- roads_dist
  message("Finished ", x, " at ", Sys.time())
  return(f_x)
})

f_x <- dplyr::bind_rows(f_x_list) # Inspect f_x
f_full <- f_x # Overwrite f_full with f_x if all looks good
rm(f_x_list, f_x)

# Save results
write.csv(f_full, "temp/Backups/20240912-114551/forestry_gis_verifications.csv",
          na = "",
          row.names = FALSE)

rm(roads)
gc()


#### COMPARE TO RAW FIELD DATA ####

f_full <- merge(f, 
                f_full[,c("objectid.y", "prop_forested_60m", "dist_lt40", "dist_gt40", "dist_road")], 
                by.x = "objectid",
                by.y = "objectid.y")

# Pull into smaller/more manageable f_c object
f_c <- f_full[,c("objectid", "den_id", "sample_id", "date_inspected", 
                 "proportion_forested_field", "proportion_forested", "prop_forested_60m",
                 "distance_less40yr_forest_field", "v_distance_less40yr_forest", "dist_lt40",
                 "distance_grtr40yr_forest_field", "v_distance_grtr40year_forest", "dist_gt40",
                 "distance_nearest_road", "v_distance_nearest_road", "dist_road"
                 )]

# > Compare existing v_ fields to calc'd verifications ----

# > > Proportion forested within 60m of den ----

# First round up to nearest whole number
f_c$prop_forested_60m <- round(f_c$prop_forested_60m * 100)
# Take absolute difference
f_c$prop_forested_60m_DIFF <- abs(f_c$proportion_forested - f_c$prop_forested_60m)
# Check how many fail the 30% difference cutoff...
nrow(f_c[which(f_c$prop_forested_60m_DIFF >= 30),]) / nrow(f_c) # not bad  :)

# > > Distance <40yo forest ----

f_c$dist_lt40 <- round(f_c$dist_lt40)
f_c$dist_lt40_DIFF <- abs(f_c$v_distance_less40yr_forest - f_c$dist_lt40)
# Now check how many are >=10m apart in difference or 30% different
nrow(f_c[which(f_c$dist_lt40_DIFF >= 10 | (f_c$dist_lt40_DIFF/f_c$v_distance_less40yr_forest) >= 0.3),]) / nrow(f_c)
# But how many dens is that to check?
unique(f_c[["den_id"]][which(f_c$dist_lt40_DIFF >= 10 | (f_c$dist_lt40_DIFF/f_c$v_distance_less40yr_forest) >= 0.3)]) |>
  length()

# > > Distance >40yo forest ----

f_c$dist_gt40 <- round(f_c$dist_gt40)
f_c$dist_gt40_DIFF <- abs(f_c$v_distance_grtr40year_forest - f_c$dist_gt40)
# Now check how many are >=10m apart in difference or 30% different
nrow(f_c[which(f_c$dist_gt40_DIFF >= 10 | (f_c$dist_gt40_DIFF/f_c$v_distance_grtr40year_forest) >= 0.3),]) / nrow(f_c)
# But how many dens is that to check?
unique(f_c[["den_id"]][which(f_c$dist_gt40_DIFF >= 10 | (f_c$dist_gt40_DIFF/f_c$v_distance_grtr40year_forest) >= 0.3)]) |>
  length()

# > > Distance to nearest road ----

f_c$dist_road <- round(f_c$dist_road)
f_c$dist_road_DIFF <- abs(f_c$v_distance_nearest_road - f_c$dist_road)
nrow(f_c[which(f_c$dist_road_DIFF >= 10 | (f_c$dist_road_DIFF/f_c$v_distance_nearest_road) >= 0.3),]) / nrow(f_c)
# But how many dens is that to check?
unique(f_c[["den_id"]][which(f_c$dist_road_DIFF >= 10 | (f_c$dist_road_DIFF/f_c$v_distance_nearest_road) >= 0.3)]) |>
  length()


# > Compare raw fields to calc'd verifications ----

# These are effectively identical to the flags qc script.

# > > Proportion forested within 60m of den ----

# Take absolute difference
f_c$prop_forested_60m_RAWDIFF <- abs(f_c$proportion_forested_field - f_c$prop_forested_60m)
# Check how many fail the 30% difference cutoff...
nrow(f_c[which(f_c$prop_forested_60m_RAWDIFF >= 30),]) / nrow(f_c)

# > > Distance <40yo forest ----

f_c$dist_lt40_RAWDIFF <- abs(f_c$distance_less40yr_forest_field - f_c$dist_lt40)
# Now check how many are >=10m apart in difference or 30% different
nrow(f_c[which(f_c$dist_lt40_RAWDIFF >= 10 | (f_c$dist_lt40_RAWDIFF/f_c$distance_less40yr_forest_field) >= 0.3),]) / nrow(f_c)
# But how many dens is that to check?
unique(f_c[["den_id"]][which(f_c$dist_lt40_RAWDIFF >= 10 | (f_c$dist_lt40_RAWDIFF/f_c$distance_less40yr_forest_field) >= 0.3)]) |>
  length()

# > > Distance >40yo forest ----

f_c$dist_gt40_RAWDIFF <- abs(f_c$distance_grtr40yr_forest_field - f_c$dist_gt40)
# Now check how many are >=10m apart in difference or 30% different
nrow(f_c[which(f_c$dist_gt40_RAWDIFF >= 10 | (f_c$dist_gt40_RAWDIFF/f_c$distance_grtr40yr_forest_field) >= 0.3),]) / nrow(f_c)
# But how many dens is that to check?
unique(f_c[["den_id"]][which(f_c$dist_gt40_RAWDIFF >= 10 | (f_c$dist_gt40_RAWDIFF/f_c$distance_grtr40yr_forest_field) >= 0.3)]) |>
  length()

# > > Distance to nearest road ----

f_c$dist_road_RAWDIFF <- abs(f_c$distance_nearest_road - f_c$dist_road)
nrow(f_c[which(f_c$dist_road_RAWDIFF >= 10 | (f_c$dist_road_RAWDIFF/f_c$distance_nearest_road) >= 0.3),]) / nrow(f_c)
# But how many dens is that to check?
unique(f_c[["den_id"]][which(f_c$dist_road_RAWDIFF >= 10 | (f_c$dist_road_RAWDIFF/f_c$distance_nearest_road) >= 0.3)]) |>
  length()


# > Clean up f_c ----

f_c <- f_c |> dplyr::select(objectid, den_id, sample_id, date_inspected,
                            proportion_forested_field, proportion_forested, prop_forested_60m, prop_forested_60m_DIFF, prop_forested_60m_RAWDIFF,
                            distance_less40yr_forest_field, v_distance_less40yr_forest, dist_lt40, dist_lt40_DIFF, dist_lt40_RAWDIFF,
                            distance_grtr40yr_forest_field, v_distance_grtr40year_forest, dist_gt40, dist_gt40_DIFF, dist_gt40_RAWDIFF,
                            distance_nearest_road, v_distance_nearest_road, dist_road, dist_road_DIFF, dist_road_RAWDIFF)

names(f_c) <- c("objectid", "den_id", "sample_id", "date_inspected",
                "raw_prop_forest_60m", "v_prop_forest_60m", "new_prop_forest_60m", "prop_forest_60m_VDIFF", "prop_forest_60m_RAWDIFF",
                "raw_dist_lt40", "v_dist_lt40", "new_dist_lt40", "dist_lt40_VDIFF", "dist_lt40_RAWDIFF",
                "raw_dist_gt40", "v_dist_gt40", "new_dist_gt40", "dist_gt40_VDIFF", "dist_gt40_RAWDIFF",
                "raw_dist_road", "v_dist_road", "new_dist_road", "dist_road_VIDFF", "dist_road_RAWDIFF")

# Save to inspect in GIS
sf::write_sf(f_c, "temp/Backups/20240912-114551/forestry_gis_verifications_check.gpkg")

#### PUSH CLEANED DATA ####

# TODO: when pushing to AGOL, make sure that nrow f_full == f.
# Don't want to overwrite one objectid multiple times
