#### SET UP ####

# Set token
# Token seems to expire anytime your ArcGIS Online session expires -
# seems to be roughly ~30 mins.
# Even though it expires super quickly, better practice to isolate token
# value in some file that's not tracked by git
source("temp/token.R")

# Load functions
source("R/fetch_agol_data.R")
source("R/clean_bear_data.R")
source("R/push_agol_data.R")

# Fetch bear data

# `dens` for 'current' data
dens <- fetch_bears(token = token,
                    layer = "current")

# `f` for 'field visits' data
f <- fetch_bears(token = token,
                 layer = "field visits")

# `p` for `potential` data
p <- fetch_bears(token = token,
                 layer = "potential")

# Data backups
# Store to `temp` folder as a data backup
systime <- format(Sys.time(), "%Y%m%d-%H%M%S")

# Current
sf::st_write(dens, paste0("temp/dens_current_", systime, ".gpkg"))

# Field visits
write.csv(f, paste0("temp/dens_visits_", systime, ".csv"),
          row.names = FALSE,
          na = "")

# Potential
sf::st_write(p, paste0("temp/dens_potential_", systime, ".gpkg"))

rm(systime)

# Keep original column names stored 
# Later will use these original column names to push data changes to 
# online layer
dens_cols <- names(dens)
f_cols <- names(f)
p_cols <- names(p)

#### GENERAL CLEANUP ####
dens <- clean_bears(dens)
f <- clean_bears(f)
p <- clean_bears(p)


#### CHECK NULL OR DUPE IDs ####

# Check for any NULL den ids
any(is.na(dens$den_id)) # y u c k 
any(is.na(f$den_id))

# Check for any NULL sample ids
any(is.na(f$sample_id))
f[is.na(f$sample_id),]

# Check dud dens
dens[is.na(dens$den_id),]
dens[grep("test|delete", dens$den_id, ignore.case = T),]

dud_dens <- c(dens[["objectid"]][is.na(dens$den_id)],
              dens[["objectid"]][grep("test|delete", dens$den_id, ignore.case = T)])

# Check dud field visits
f[grep("test|delete", f$den_id, ignore.case = T),]

dud_f <- f[["objectid"]][grep("test|delete", f$den_id, ignore.case = T)]

# Check den ids is unique in dens
dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)] # y u c k
dens[dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)],]
# awesome, as of 2024-09-05, all dupe dens have been fixed, aside from SAN_FishermanRiver_1

# Check sample_id is unique in visits
f$sample_id %in% f[["sample_id"]][duplicated(f$sample_id)]
f[(f$sample_id %in% f[["sample_id"]][duplicated(f$sample_id)]),] |>
  dplyr::arrange(sample_id) |>
  View()

# Check for field visits that have no corresponding bear den ID
unique(f$den_id)[!(unique(f$den_id) %in% unique(dens$den_id))]


#### PUSH CLEANED DATA ####

# Rename cols back to ESRI online colnames
names(dens) <- dens_cols
names(f) <- f_cols[1:62] # drop col 'geometry'
names(p) <- p_cols

# Test JSON size - copy and paste into online checker
# e.g. https://www.javainuse.com/bytesizejson
clipr::write_clip(to_esri_json(dens))
clipr::write_clip(to_esri_json(f))
clipr::write_clip(to_esri_json(p))

# Ok here goes..... going to start with Potential layer, as that's the least
# important to mess up in case it goes wrong...
# Update only the character columns + drop geometry to save on URL length
p_update <- dplyr::select(p, OBJECTID, dplyr::where(is.character)) |>
  sf::st_drop_geometry()
# Somehow latitude and longitude are not numerics???
p_update <- p_update[,!(names(p_update) %in% c("Latitude", "Longitude"))]
clipr::write_clip(to_esri_json(p_update))
# Moment of truth
push_bears(json = to_esri_json(p_update),
           layer = "potential",
           token = token)
rm(p_update)

# Now we do the dens layer....
# Again, drop the geometry - don't need to mass edit that. 
dens_update <- sf::st_drop_geometry(dens)
# Check size
clipr::write_clip(to_esri_json(dens_update)) # 232 KB
# Update
push_bears(json = to_esri_json(dens_update),
           layer = "current",
           token = token)
rm(dens_update)


# Field visits layer
# Drop geometry
f_update <- sf::st_drop_geometry(f)
# Drop date column - leave that as-is
f_update <- f_update[, !(names(f_update) %in% c("Date_Inspected"))]
# Check size
clipr::write_clip(to_esri_json(f_update)) # 905 KB
# Update
push_bears(json = to_esri_json(f_update),
           layer = "field visits",
           token = token)
rm(f_update)
