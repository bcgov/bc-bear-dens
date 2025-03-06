# PSEUDO DENS COMPARISON

# Comparing dummy den data ('pseudo dens') to actual
# den data. How does the distribution of forestry values
# compare between our sampled dens to random points
# on the landscape?

#### SETUP ####

library(targets)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggsignif)

# Pull our data
# First our points
f <- tar_read(f_analysis) # den field visits, re-arranged such that last year's forestry data is paired with this year's den status
tar_load(dens) # den spatial/static data
tar_load(pseudo_dens) # pseudo den points

# First, for the pseudo den data, we're going to make
# the same assumption as for the real data - this year's
# den status needs to be paired with last year's forestry.
# Add 1 from the pseudo data `year` so that the 
# data runs from 2019-2023, rather than 2020-2024, 
# because then it will line up with our real data (which
# only runs until 2023.)
pseudo_dens$year <- pseudo_dens$year + 1

# Merge spatial/static den data with field visits
f <- merge(dens, f, by = "den_id")

# Add latitude, longitude cols to data
f <- st_transform(f, 3005) # to BC Albers projection
f <- cbind(f, st_coordinates(f))
names(f)[grep("X", names(f))] <- "longitude" # technically easting/northing with BC Albers but shhh
names(f)[grep("Y", names(f))] <- "latitude"

pseudo_dens <- cbind(pseudo_dens, st_coordinates(pseudo_dens))
names(pseudo_dens)[grep("X", names(pseudo_dens))] <- "longitude"
names(pseudo_dens)[grep("Y", names(pseudo_dens))] <- "latitude"

# Assign either VI or HG to data, to examine by island
f$region <- ifelse(f$latitude > 800000, "HG", "VI")
pseudo_dens$region <- ifelse(pseudo_dens$latitude > 800000, "HG", "VI")

# Make den status a factor
f$den_status <- factor(x = f$den_status,
                       levels = c("Currently active", #x
                                  "Active in last denning season", #x
                                  "Active recently (0-4 seasons)", #x
                                  "Not active in last season, but recent use (1-4 seasons)", #x
                                  "Not active in last season, no recent use (>4 seasons)", #x
                                  "No recent evidence of use (>4 seasons)", #x
                                  "Not active in last season", #x
                                  "Obsolete", #x
                                  "Unknown")) #x

# Bin into binary categories
f <- f |> 
  dplyr::mutate(den_status_binary = dplyr::case_when(grepl("^No", den_status) ~ "Not active",
                                                     den_status == "Obsolete" ~ "Obsolete",
                                                     den_status == "Unknown" ~ "Unknown",
                                                     TRUE ~ "Active"))

plyr::count(f$den_status)
plyr::count(f$den_status_binary)


# Define list of "for sure dens"
for_sure_dens <- c("Active in last denning season", 
                   "Not active in last season",
                   "Not active in last season, but recent use (1-4 seasons)",
                   "Not active in last season, no recent use (>4 seasons)",
                   "No recent evidence of use (>4 seasons)")


##### Map #####

# Pull our parks shapefiles
parks1 <- tar_read(hg_vi_tantalis_parks)
parks2 <- tar_read(hg_vi_tantalis_cons_areas)
parks3 <- tar_read(hg_vi_federal_parks)

names(parks2)[1] <- "PROTECTED_LANDS_NAME"
parks2$PROTECTED_LANDS_DESIGNATION <- "CONSERVANCY AREA"

names(parks3)[2] <- "PROTECTED_LANDS_NAME"
parks3$PROTECTED_LANDS_DESIGNATION <- "FEDERAL PARK"

parks <- dplyr::bind_rows(parks1, parks2, parks3)
parks <- st_as_sf(parks, wkt = "WKT_GEOM")
st_crs(parks) <- 3005
names(parks)[5] <- "geometry" # rename geometry column
st_geometry(parks) <- "geometry"
names(parks) <- tolower(names(parks))

rm(parks1, parks2, parks3)

# Intersect parks with dens/pseudo_dens, so we know how
# many pseudo dens were generated inside park lands
pseudo_dens$in_park <- FALSE
pseudo_dens[["in_park"]][unlist(st_intersects(parks, pseudo_dens))] <- TRUE

f$in_park <- FALSE
f[["in_park"]][unlist(st_intersects(parks, f))] <- TRUE

sum(pseudo_dens$in_park) / nrow(pseudo_dens)
sum(f$in_park) / nrow(f)

# And finally pull a shapefile of our study area
tar_load(study_area)

# Set up our map bounding box
bbox <- st_bbox(study_area)
bbox[1:2] <- bbox[1:2] - 10000 # add a 10km buffer to our bounding box
bbox[3:4] <- bbox[3:4] + 10000 # add a 10km buffer to our bounding box

# Pull BC shapefile from rnaturalearth
bc <- ne_states(country = "canada")
bc <- bc[bc$name == "British Columbia", ]
bc <- st_transform(bc, 3005) # BC Albers projection
bc <- st_crop(bc, bbox) # crop to our bounding box area

# Plot time!
# For the life of me can't figure out why the point size is not
# changing but that's a low priority thing to fix
# COMMENTED OUT TO SPEED UP RUNNING THIS SCRIPT
# ggplot() +
#   geom_sf(data = bc, fill = "grey") +
#   geom_sf(data = study_area, fill = "darkgrey") +
#   geom_sf(data = parks, fill = "lightgreen", alpha = 0.4) +
#   geom_sf(data = pseudo_dens, 
#           aes(shape = in_park),
#           stroke = NA, 
#           alpha = 0.3, 
#           size = 0.2) +
#   geom_sf(data = f, 
#           aes(shape = in_park),
#           color = "purple", 
#           stroke = NA, 
#           size = 0.2) +
#   coord_sf(xlim = c(bbox[1], bbox[3]),
#            ylim = c(bbox[2], bbox[4])) +
#   theme(legend.position = "none") +
#   labs(caption = "Triangle shape indicates the den falls within a provincial/federal park or protected area.")



##### Assign random den status #####

# Randomly assign a den_status to the pseudo_data, following
# similar proportions as the actual data

# Pull the % distribution of den statuses from the actual
# data, then round to the nearest hundreth. We'll use those
# percentages to simulate den status in the pseudo data.
status_freq <- round(prop.table(table(f$den_status)), 2)
status_freq

# Generate random den status for our pseudo dens
pseudo_dens$den_status <- sample(names(status_freq), 
                                 nrow(pseudo_dens), 
                                 replace = TRUE, 
                                 prob = status_freq)

# Check that frequencies line up
prop.table(table(pseudo_dens$den_status))

# Make den status a factor
pseudo_dens$den_status <- factor(x = pseudo_dens$den_status,
                                 levels = c("Currently active", #x
                                            "Active in last denning season", #x
                                            "Active recently (0-4 seasons)", #x
                                            "Not active in last season, but recent use (1-4 seasons)", #x
                                            "Not active in last season, no recent use (>4 seasons)", #x
                                            "No recent evidence of use (>4 seasons)", #x
                                            "Not active in last season", #x
                                            "Obsolete", #x
                                            "Unknown")) #x

# Bin into binary categories
pseudo_dens <- pseudo_dens |> 
  dplyr::mutate(den_status_binary = dplyr::case_when(grepl("^No", den_status) ~ "Not active",
                                                     den_status == "Obsolete" ~ "Obsolete",
                                                     den_status == "Unknown" ~ "Unknown",
                                                     TRUE ~ "Active"))

plyr::count(pseudo_dens$den_status)
plyr::count(pseudo_dens$den_status_binary)


##### Pull verification data #####
# Then pull our verification data
# Even though we've manually verified the den data to 
# be the best it can be, we're still going to compare
# just the autogenerated GIS forestry data for both
# groups, to ensure it's an apples-to-apples 
# comparison.
fv <- tar_read(forestry_verifications_full) # read data verification results
pfv <- tar_read(pseudo_forestry_verifications_full) # pseudo data verification results

# Merge verification data to the spatial data
f <- merge(f, fv, by.x = "sample_id_forest", by.y = "sample_id") # again, merging GIS forestry data for the year PRIOR to the den status year
pseudo_dens <- merge(pseudo_dens, pfv, by = "sample_id")


##### Pull DEM data #####
# For each pseudo den, we have elevation, slope (%),
# and slope aspect, extracted from the BC CDED 30m data.
tar_load(pseudo_dens_dem)
pseudo_dens_dem <- unique(pseudo_dens_dem)

pseudo_dens <- merge(pseudo_dens, pseudo_dens_dem)

rm(pseudo_dens_dem)

# And also clean the DEM data fields in the real data
f[["elevation_m"]][which(f$elevation_m == 999)] <- NA
f[["slope_pct"]][which(f$slope_pct > 200)] <- NA
f[["slope_aspect"]][which(f$slope_aspect > 360)] <- NA

##### Pull age class data #####
# For each pseudo den, we have the 2023 VRI age class.
tar_load(pseudo_dens_age_class)
pseudo_dens_age_class <- sf::st_drop_geometry(pseudo_dens_age_class)

pseudo_dens <- merge(pseudo_dens, pseudo_dens_age_class, all.x = TRUE)

rm(pseudo_dens_age_class)

# And also clean up the age class field in the real data
# This will produce some NAs
pseudo_dens$proj_age_class_cd_1 <- as.numeric(pseudo_dens$proj_age_class_cd_1)
f$proj_age_class_cd_1 <- as.numeric(substring(f$age_class, 1, 1))



##### % age class #####

prct <- tar_read(prct_age_class_1.5km)
pseudo_prct <- tar_read(pseudo_prct_age_class_1.5km)

# Bin the data into groups (sort of ~young, old, medium, very old)
prct$lt_3 <- rowSums(prct[,c("age_class_1", "age_class_2")])
prct$gt_3 <- rowSums(prct[,6:12])
prct$three_to_7 <- rowSums(prct[,6:10])
prct$gt_8 <- rowSums(prct[,c("age_class_8", "age_class_9")])

pseudo_prct$lt_3 <- rowSums(pseudo_prct[,c("age_class_1", "age_class_2")])
pseudo_prct$gt_3 <- rowSums(pseudo_prct[,6:12])
pseudo_prct$three_to_7 <- rowSums(pseudo_prct[,6:10])
pseudo_prct$gt_8 <- rowSums(pseudo_prct[,c("age_class_8", "age_class_9")])

# Merge with den status - so we can look at percent of each
# age class with activity status
f <- merge(f, prct, by.x = "sample_id_forest", by.y = "sample_id") # again, merging GIS forestry data for the year PRIOR to the den status year
f <- dplyr::select(f, -den_id.y, -year.y)
names(f)[grep("den_id.x", names(f))] <- "den_id"
names(f)[grep("year.x", names(f))] <- "year"

pseudo_dens <- merge(pseudo_dens, pseudo_prct, by = "sample_id")
pseudo_dens <- dplyr::select(pseudo_dens, -den_id.y, -year.y)
names(pseudo_dens)[grep("den_id.x", names(pseudo_dens))] <- "den_id"
names(pseudo_dens)[grep("year.x", names(pseudo_dens))] <- "year"


##### Road density #####

tar_load(road_density)
tar_load(pseudo_road_density)

road_density <- road_density[,c("sample_id", "road_density_m2")]
pseudo_road_density <- pseudo_road_density[,c("sample_id", "road_density_m2")]

# Note the all.x = TRUE. If the den isn't in
# the road density df, the road density is 0.
f <- merge(f, road_density, by.x = "sample_id_forest", by.y = "sample_id", all.x = TRUE) # again, merging GIS forestry data for the year PRIOR to the den status year
f$road_density_m2 <- ifelse(is.na(f$road_density_m2), 0, f$road_density_m2)

pseudo_dens <- merge(pseudo_dens, pseudo_road_density, by = "sample_id", all.x = TRUE)
pseudo_dens$road_density_m2 <- ifelse(is.na(pseudo_dens$road_density_m2), 0, pseudo_dens$road_density_m2)

##### Merge datasets #####
pseudo_dens$type <- "Pseudo"
f$type <- "Real"

full <- dplyr::bind_rows(pseudo_dens, f)

# We'll use the den year sample_id (rather than the forestry year sample id)
# as the default sample id
full$sample_id <- ifelse(is.na(full$sample_id), full$sample_id_den, full$sample_id)

# Subset to only columns we care about + full observations
full <- full |> 
  dplyr::select(sample_id, den_id, type, year,
                den_status, den_status_binary,
                region, latitude, longitude, in_park,
                elevation_m, slope_pct, slope_aspect,
                proj_age_class_cd_1,
                prop_forest_60m, dist_lt40, dist_gt40, dist_road,
                age_class_1:road_density_m2)

# Filter to only 2019:2023
full <- full[which(full$year %in% c(2019:2024)), ]

#full <- na.omit(full) # this eliminates a lot of dens with incomplete DEM (slope, elevation, etc.) data

# Rename park variable so it's easier to tell what it is on plots
full$in_park <- ifelse(full$in_park, "in_park", "not_in_park")


##### Distance from edge #####
# Next, make just one 'distance from edge' variable
full$dist_from_edge <- ifelse(full$dist_lt40 > 0, full$dist_lt40, full$dist_gt40)
hist(full$dist_from_edge)
full$log_dist_from_edge <- log(full$dist_from_edge + 1)
hist(full$log_dist_from_edge)

# Next, say if it's in old, new, or edge of forest.
full <- full |> dplyr::mutate(dist_lt40 = round(dist_lt40, 0),
                              dist_gt40 = round(dist_gt40, 0),
                              age = dplyr::case_when((dist_lt40 == dist_gt40) ~ "on_edge",
                                               (dist_lt40 > dist_gt40) ~ "within_old",
                                               (dist_lt40 < dist_gt40) ~ "within_young"))


#### CHECK PSEUDO DATA ####

# Does it actually change through time? Good to double check.

ggplot(full, 
       aes(x = year, 
           y = prop_forest_60m, 
           color = den_id, 
           group = den_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(full, 
       aes(x = year, 
           y = dist_lt40, 
           color = den_id, 
           group = den_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(full, 
       aes(x = year, 
           y = dist_gt40, 
           color = den_id, 
           group = den_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(full, 
       aes(x = year, 
           y = dist_road, 
           color = den_id, 
           group = den_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ type, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none")


#### BOXPLOTS ####

# For simplicity's sake we're only going to do
# for_sure_dens comparisons.

##### % forested #####
# Binary categories

# Den status vs % forested
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = prop_forest_60m,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (GIS verified values)",
       title = "Den Status vs. % Forested (GIS verified)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


full |>
  ggplot(aes(x = prop_forest_60m,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ type) +
  labs(x = "Proportion Forested (GIS verified values)",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()


# Vs. In park or not
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = prop_forest_60m,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Park Status",
       y = "Proportion Forested (GIS verified values)",
       title = "Park Status vs. % Forested (GIS verified)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


full |>
  ggplot(aes(x = prop_forest_60m,
             color = in_park,
             fill = in_park)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Park Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Park Status") +
  facet_wrap(~ type) +
  labs(x = "Proportion Forested (GIS verified values)",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()



##### dist <40 #####

# Den status vs dist lt 40
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = (dist_lt40 + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(side = "l") +
  labs(x = "Den Status",
       y = "log Distance to <40 (GIS verified values)",
       title = "Den Status vs. Distance to <40 yo forest (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'") 


psych::describeBy(full$dist_lt40,
                  full$type,
                  mat = TRUE)

full |>
  ggplot(aes(x = (dist_lt40 + 1),
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(side = "b") +
  facet_wrap(~ type) +
  labs(x = "Distance to <40 yo forest (GIS verified values)",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()


# Vs. In park or not
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = (dist_lt40 + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(side = "l") +
  labs(x = "Park Status",
       y = "Distance to <40 yo forest (GIS verified values)",
       title = "Park Status vs. Distance to <40 yo forest (GIS verified)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


full |>
  ggplot(aes(x = (dist_lt40 + 1),
             color = in_park,
             fill = in_park)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Park Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Park Status") +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(side = "b") +
  facet_wrap(~ type) +
  labs(x = "Distance to <40 yo forest (GIS verified values)",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()



##### dist >40 #####

# Den status vs dist gt 40
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = (dist_gt40 + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to >40 (GIS verified values)",
       title = "Den Status vs. Distance to >40 yo forest (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'") 

full |>
  ggplot(aes(x = (dist_gt40 + 1),
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(side = "b") +
  facet_wrap(~ type) +
  labs(x = "Distance to >40 yo forest (GIS verified values)",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()


# Vs. In park or not
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = (dist_gt40 + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(side = "l") +
  labs(x = "Park Status",
       y = "Distance to >40 yo forest (GIS verified values)",
       title = "Park Status vs. Distance to >40 yo forest (GIS verified)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


full |>
  ggplot(aes(x = (dist_gt40 + 1),
             color = in_park,
             fill = in_park)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Park Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Park Status") +
  scale_x_continuous(trans = "log10") +
  annotation_logticks(side = "b") +
  facet_wrap(~ type) +
  labs(x = "Distance to >40 yo forest (GIS verified values)",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()


##### dist to nearest road #####

# Den status vs dist road
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = (dist_road + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to nearest road (GIS verified values)",
       title = "Den Status vs. Distance to nearest road (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


# Vs. in park or not
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = (dist_road + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Park Status",
       y = "log Distance to nearest road (GIS verified values)",
       title = "Park Status vs. Distance to nearest road (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


##### distance to edge #####

# Den status vs dist to edge
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = (dist_from_edge + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to forest edge (GIS verified values)",
       title = "Den Status vs. Distance to forest edge (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, age), 
             y = (dist_from_edge + 1),
             color = age)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.within_old", "Not active.within_old"),
                                 c("Active.within_young", "Not active.within_young")), 
              map_signif_level = TRUE) +
  facet_wrap(~ type) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to forest edge (GIS verified)",
       title = "Den Status vs. Distance to forest edge (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Vs. In park or not
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = (dist_from_edge + 1),
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Park Status",
       y = "log Distance to forest edge (GIS verified values)",
       title = "Park Status vs. Distance to forest edge (GIS verified values)",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'")


##### latitude #####

full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = latitude,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region + type,
             scales = "free") +
  labs(x = "Den Status",
       y = "Latitude",
       title = "Den Status vs. Latitude",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'") 

# Definitely clustered data compared to the pseudo data.
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = latitude,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ type + region) +
  labs(x = "Latitude",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()


##### longitude #####

# Also definitely super clustered in the real data
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = longitude,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region + type,
             scales = "free") +
  labs(x = "Den Status",
       y = "Longitude",
       title = "Den Status vs. Longitude",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'") 

# definitely some clustering in 'Obsolete' dens related to longitude
# on Vancouver Island
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = longitude,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ type + region) +
  labs(x = "Longitude",
       y = "Density",
       caption = "'For sure' categories binned into either 'Active' or 'Not active'") +
  theme_minimal()


##### % age class #####


# Mean % area by age class across all dens across all years:
prct |> 
  dplyr::select(year:age_class_9) |>
  tidyr::pivot_longer(cols = 2:10) |>
  dplyr::group_by(year, name) |>
  dplyr::summarise(value = mean(value)) |>
  ggplot(aes(x = year, 
             y = value, 
             fill = name)) +
  geom_area() +
  labs(title = "% age class through time",
       subtitle = "Real data") +
  theme_minimal()

pseudo_prct |> 
  dplyr::select(year:age_class_9) |>
  tidyr::pivot_longer(cols = 2:10) |>
  dplyr::group_by(year, name) |>
  dplyr::summarise(value = mean(value)) |>
  ggplot(aes(x = year, 
             y = value, 
             fill = name)) +
  geom_area() +
  labs(title = "% age class through time",
       subtitle = "Pseudo data") +
  theme_minimal()


# Den status vs % old growth
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = gt_8,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region + type,
             scales = "free") +
  labs(x = "Den Status",
       y = "% old growth",
       title = "Den Status vs. % old growth",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'",
       caption = "% age class >= 8 by area within 1.5 km of the den") 

# Den status vs % new growth
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = lt_3,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region + type,
             scales = "free") +
  labs(x = "Den Status",
       y = "% fresh cut",
       title = "Den Status vs. % fresh cut",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'",
       caption = "% age class <= 3 by area within 1.5 km of the den")



# In park vs % old growth
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = gt_8,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region + type,
             scales = "free") +
  labs(x = "Park Status",
       y = "% old growth",
       title = "Park Status vs. % old growth",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'",
       caption = "% age class >= 8 by area within 1.5 km of the den") 

# In park vs % new growth
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type), 
             y = lt_3,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region + type,
             scales = "free") +
  labs(x = "Park Status",
       y = "% fresh cut",
       title = "Park Status vs. % fresh cut",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'",
       caption = "% age class <= 3 by area within 1.5 km of the den")


##### road density #####


# Den status vs road density
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type),
             y = (road_density_m2 + 0.0001),
             color = type)) +
  geom_jitter() +
  geom_boxplot(fill = NA) + 
  scale_y_log10() +
  annotation_logticks(side = "l") +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")),
              map_signif_level = TRUE) +
  labs(x = "Den Status",
       y = "Road density",
       title = "Den Status vs. Road Density",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'",
       caption = "m2 of road surface within 1.5 km of the den") +
  theme_minimal()


# Vs. In park or not
full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(in_park, type),
             y = (road_density_m2 + 0.0001),
             color = type)) +
  geom_jitter() +
  geom_boxplot(fill = NA) + 
  scale_y_log10() +
  annotation_logticks(side = "l") +
  geom_signif(comparisons = list(c("in_park.Pseudo", "not_in_park.Pseudo"),
                                 c("in_park.Real", "not_in_park.Real")),
              map_signif_level = TRUE) +
  labs(x = "Den Status",
       y = "Road density",
       title = "Den Status vs. Road Density",
       subtitle = "'For sure' categories binned into either 'Active' or 'Not active'",
       caption = "m2 of road surface within 1.5 km of the den") +
  theme_minimal()


##### elevation #####

full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = elevation_m,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Elevation (m)",
       title = "Den Status vs. Elevation (m)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = elevation_m,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ type + region) +
  labs(x = "Elevation (m)",
       y = "Density") +
  theme_minimal()


##### slope % #####


full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = slope_pct,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Slope Grade (%)",
       title = "Den Status vs. Slope (%)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = slope_pct,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ type + region) +
  labs(x = "Slope Grade (%)",
       y = "Density") +
  theme_minimal()


##### slope aspect #####

full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, type), 
             y = slope_aspect,
             color = type)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.Pseudo", "Not active.Pseudo"),
                                 c("Active.Real", "Not active.Real")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Slope Aspect",
       title = "Den Status vs. Slope Aspect",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


full[which(full$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = slope_aspect,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ type + region) +
  labs(x = "Slope Aspect",
       y = "Density") +
  theme_minimal()


