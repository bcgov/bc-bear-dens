#### SET UP ####

# Set token
# Token seems to expire anytime your ArcGIS Online session expires -
# seems to be roughly ~30 mins.
token <- "3NKHt6i2urmWtqOuugvr9fdAOlJ-AnbloT9i0687pzIv10OGYc_T4PR4piOkKNW7XSPsN_c8z0JcKpx1mneoLDifHIglSoLk48s5jDMCfkEFN_hZDR_dJOZ6g0cOW3K07R1UQ4xFDLWjYEyXUDgWnMsGuzO1OFjA8lFiQF-Dwg31RxL66sHUpEOmunQrMfV_jPEc6WMPR1t_Ns_42tqPW9wcFb5N_Ea053xlsFJ6dOA."

# Load functions
source("R/fetch_agol_data.R")

# Fetch bear data

# `dens` for 'current' data
dens <- fetch_bears(token = token,
                    layer = "current")

# `f` for 'field visits' data
f <- fetch_bears(token = token,
                 layer = "field visits")

# Store to `temp` folder as a data backup
systime <- format(Sys.time(), "%Y%m%d-%H%M%S")
write.csv(dens, paste0("temp/dens_current_", systime, ".csv"),
          row.names = FALSE,
          na = "")
write.csv(f, paste0("temp/dens_visits_", systime, ".csv"),
          row.names = FALSE,
          na = "")
rm(systime)

# Keep original column names stored
# Not sure this is 100% accurate for later API attributes but will cross that
# bridge when we get to it
dens_cols <- names(dens)
f_cols <- names(f)

# Clean up column names
dens <- janitor::clean_names(dens)
names(dens) <- gsub("i_d", "id", names(dens)) # fix janitor splitting 'IDs' into 'i_ds'
f <- janitor::clean_names(f)

# Drop geometry column for visits table
f <- sf::st_drop_geometry(f)


#### CLEAN UP ####

# Check for any NULL den ids
any(is.na(dens$den_id)) # y u c k 
any(is.na(f$den_id))

# Check den ids is unique in dens
dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)]
dens[dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)],]

# Check sample_id is unique in visits
f$sample_id %in% f[["sample_id"]][duplicated(f$sample_id)]

