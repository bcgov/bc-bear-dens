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
sf::st_write(dens, paste0("temp/dens_current_", systime, ".shp"))

# Field visits
write.csv(f, paste0("temp/dens_visits_", systime, ".csv"),
          row.names = FALSE,
          na = "")

# Potential
sf::st_write(p, paste0("temp/dens_potential_", systime, ".shp"))

rm(systime)

# Keep original column names stored
# Not sure this is 100% accurate for later API attributes but will cross that
# bridge when we get to it
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

# Check dud dens
dens[is.na(dens$den_id),]
dens[grep("test|delete", dens$den_id, ignore.case = T),]

f[grep("test|delete", f$den_id, ignore.case = T),]

# Check den ids is unique in dens
dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)] # y u c k
dens[dens$den_id %in% dens[["den_id"]][duplicated(dens$den_id)],]

# Check sample_id is unique in visits
f$sample_id %in% f[["sample_id"]][duplicated(f$sample_id)]
f[(f$sample_id %in% f[["sample_id"]][duplicated(f$sample_id)]),] |>
  dplyr::arrange(sample_id) |>
  View()


