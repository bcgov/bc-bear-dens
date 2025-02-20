
# TOP ---------------------------------------------------------------------

# Load up data
library(targets)
library(sf)
tar_load(f)
tar_load(dens) # to get static den characteristics + lat/long
dens <- cbind(dens, sf::st_coordinates(dens)) # add coordinates as X, Y cols
names(dens)[grep("^X$", names(dens))] <- "longitude"
names(dens)[grep("^Y$", names(dens))] <- "latitude"

f <- merge(f, dens, by = "den_id")

# Load up other libraries
library(ggplot2)
library(ggsignif)
library(lme4)

# Assign either VI or HG to data, to examine by island
f$region <- ifelse(f$latitude > 52, "HG", "VI")

# Take a look at windthrow variables
unique(f$x_windthrow_code)

plyr::count(f$x_windthrow_code) |>
  dplyr::mutate(prct = round(freq/nrow(f) * 100, 1))

# Fill NA den status w string
any(is.na(f$den_status)) # Though now after latest rounds of QC, there's no NA den status!
f[["den_status"]][is.na(f$den_status)] <- "NA"

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
f <- f |> dplyr::mutate(den_status_binary = dplyr::case_when(grepl("^No", den_status) ~ "Not active",
                                                             den_status == "Obsolete" ~ "Obsolete",
                                                             den_status == "Unknown" ~ "Unknown",
                                                             TRUE ~ "Active"))

# Double check our work
unique(f[,c("den_status", "den_status_binary")])

plyr::count(f$den_status)
plyr::count(f$den_status_binary)

##### DATA WRANGLING #####
# Merge previous year forestry with current year den status

# First, to make things more manageable, subset to
# just our columns of interest
f <- f[,c("sample_id", "den_id", "date_inspected", 
          "region", "latitude", "longitude",
          "struct_stage", "age_class", "canopy_closure",
          "elevation_m", "slope_pct", "slope_aspect",
          "bed_depth", "bed_width", "bed_length", 
          "hair_in_bed", 
          "den_status", "den_status_binary",
          "forestry_treatment_desc",
          "distance_nearest_tree_field",
          "proportion_forested_field", "proportion_forested",
          "distance_less40yr_forest_field", "v_distance_less40yr_forest",
          "distance_grtr40yr_forest_field", "v_distance_grtr40year_forest",
          "distance_nearest_road", "v_distance_nearest_road",
          "proportion_tree_windthrown", "x_windthrow_code")]

# Next drop any records where there may have been more than
# one den visit within the year
f$year <- lubridate::year(f$date_inspected)
f |> 
  dplyr::group_by(den_id, year) |> 
  dplyr::summarise(N = dplyr::n()) |>
  dplyr::filter(N > 1)

# Inspect each den + year combo, and determine which ones to cull
# Just going off whichever ones have more complete data/fewer NAs
f[f$den_id == "COU_AlberniInlet_1" & f$year == 2020, ] # cut COU_AlberniInlet_1_20200129
f[f$den_id == "ROS_ChefCreek_1" & f$year == 2021, ] # cut ROS_ChefCreek_1_20210531
f[["v_distance_less40yr_forest"]][f$sample_id == "ROS_ChefCreek_1_20210903"] <- 400 # Quick manual fix... TODO: update actual data and delete this from the code
f[f$den_id == "SKI_JakesLanding_1" & f$year == 2024, ] # cut SKI_JakesLanding_1_20240609
f[f$den_id == "TSI_MountRussell_7" & f$year == 2021, ] # cut TSI_MountRussell_7_20210916

# Cut 'em
f <- f[!(f$sample_id %in% c("COU_AlberniInlet_1_20200129","ROS_ChefCreek_1_20210531", "SKI_JakesLanding_1_20240609", "TSI_MountRussell_7_20210916")), ]

# Next we want to wrangle the data such that the den status from
# the current year is matched to the forestry data from the 
# previous year.

# Arrange data by den_id + year, for convenience
f <- f[order(f$den_id, f$year),]

# We're going to do this in a simple way... chop the data in half vertically,
# so we have f_a: den info and f_b: forestry info. 
f_a <- dplyr::select(f, sample_id:den_status_binary, year) # select columns 1:9 (den info) + column 22 (year)
f_b <- dplyr::select(f, den_id, sample_id, date_inspected, year,
                     forestry_treatment_desc:v_distance_nearest_road, 
                     proportion_tree_windthrown, x_windthrow_code) # select columns 1:2 (sample_id, den_id) + 10:22 (forestry info). 

# Next, subtract -1 from the den status year - since it's technically
# whether or not the den was occupied in the *last* year
f_a$year <- f_a$year - 1

# Now merge the two back together on den_id and year
# It's gonna cut a lot of data out
f <- merge(f_a, f_b, by = c("den_id", "year"), suffixes = c("_den", "_forest")) 

# I'm keeping sample_id in here just to keep track of which visit the den 
# status came from vs which sample_id the forestry data came from.
# The 'suffixes' argument tacks on either '_den' or '_forest' to the end
# of the sample_id column name to keep track of whether the value came from
# the f_a dataset or f_b dataset.

# Next let's just rename some of the columns to make them a bit more consistent.
# In theory, now that we've done a ton of manual QC, f_* should equal v_*!!
f <- dplyr::rename(f,
              "nearest_tree_m" = "distance_nearest_tree_field",
              "f_prop_forest_60m" = "proportion_forested_field",
              "v_prop_forest_60m" = "proportion_forested",
              "f_dist_lt40" = "distance_less40yr_forest_field", 
              "v_dist_lt40" = "v_distance_less40yr_forest",
              "f_dist_gt40" = "distance_grtr40yr_forest_field", 
              "v_dist_gt40" = "v_distance_grtr40year_forest", 
              "f_dist_road" = "distance_nearest_road", 
              "v_dist_road" = "v_distance_nearest_road",
              "windthrow_prct" = "proportion_tree_windthrown", 
              "windthrow_code" = "x_windthrow_code")

rm(f_a, f_b)


##### DISTANCE TO EDGE + AGE #####

# Next, make just one 'distance from edge' variable
f$dist_from_edge <- ifelse(f$f_dist_lt40 > 0, f$f_dist_lt40, f$f_dist_gt40)
hist(f$dist_from_edge)
f$log_dist_from_edge <- log(f$dist_from_edge + 1)
hist(f$log_dist_from_edge)

# Next, say if it's in old, new, or edge of forest.
f <- f |> dplyr::mutate(dist_lt40 = round(f_dist_lt40, 0),
                        dist_gt40 = round(f_dist_gt40, 0),
                        age = dplyr::case_when((f_dist_lt40 == f_dist_gt40) ~ "on_edge",
                                               (f_dist_lt40 > f_dist_gt40) ~ "within_old",
                                               (f_dist_lt40 < f_dist_gt40) ~ "within_young"))

# EXPLORE BINARY VARS ---------------------------------------------------------


# Double check our data health. There should NO LONGER
# be any 999s or -999s in the forestry data
sum(f$f_prop_forest_60m == 999, na.rm = TRUE)
sum(f$f_dist_lt40 == 999, na.rm = TRUE)
sum(f$f_dist_gt40 == 999, na.rm = TRUE)
sum(f$f_dist_road == 999, na.rm = TRUE)

sum(f$f_prop_forest_60m < 0, na.rm = TRUE)
sum(f$f_dist_lt40 < 0, na.rm = TRUE)
sum(f$f_dist_gt40 < 0, na.rm = TRUE)
sum(f$f_dist_road < 0, na.rm = TRUE)


##### % forested #####
# All categories

f |>
  ggplot(aes(x = den_status, 
             y = f_prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# This was from when we compared to the GIS verified values
# f[which(f$proportion_forested_field < 999), ] |>
#   ggplot(aes(x = den_status, 
#              y = prop_forest_60m)) +
#   geom_boxplot() +
#   geom_jitter() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# % forested
# Binary categories

f |>
  ggplot(aes(x = den_status_binary, 
             y = f_prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (RAW field values)",
       title = "Den Status vs. % Forested (RAW)",
       subtitle = "All categories binned into either 'Active' or 'Not active'")

# f |>
#   ggplot(aes(x = den_status_binary, 
#              y = prop_forest_60m)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
#   theme_minimal() +
#   labs(x = "Den Status",
#        y = "Proportion Forested (VERIFIED)",
#        title = "Den Status vs. % Forested (GIS VERIFIED)",
#        subtitle = "All categories binned into either 'Active' or 'Not active'") 


##### dist <40 #####

f |>
  ggplot(aes(x = den_status_binary, 
             y = f_dist_lt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to <40 (RAW)",
       title = "Den Status vs. Distance to <40 yo forest (RAW field values)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


psych::describeBy(f$f_dist_lt40,
                  f$den_status_binary,
                  mat = TRUE)

f |>
  ggplot(aes(x = f_dist_lt40,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Distance to <40 yo forest (RAW)",
       y = "Density") +
  theme_minimal()



# f |>
#   ggplot(aes(x = den_status_binary, 
#              y = dist_lt40)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
#   theme_minimal() +
#   scale_y_continuous(trans = 'log10') +
#   annotation_logticks() +
#   labs(x = "Den Status",
#        y = "log Distance to <40 (VERIFIED)",
#        title = "Den Status vs. Distance to <40 yo forest (GIS VERIFIED)",
#        subtitle = "All categories binned into either 'Active' or 'Not active'") 



##### dist >40 #####


f |>
  ggplot(aes(x = den_status_binary, 
             y = f_dist_gt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to >40 (RAW)",
       title = "Den Status vs. Distance to >40 yo forest (RAW field values)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 



# f |>
#   ggplot(aes(x = den_status_binary, 
#              y = dist_gt40)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
#   theme_minimal() +
#   scale_y_continuous(trans = 'log10') +
#   annotation_logticks() +
#   labs(x = "Den Status",
#        y = "log Distance to >40 (VERIFIED)",
#        title = "Den Status vs. Distance to >40 yo forest (GIS VERIFIED)",
#        subtitle = "All categories binned into either 'Active' or 'Not active'")


##### dist to nearest road #####

f |>
  ggplot(aes(x = den_status_binary, 
             y = f_dist_road)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to nearest road (RAW)",
       title = "Den Status vs. Distance to nearest road (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


##### distance to edge #####

f |>
  ggplot(aes(x = den_status_binary, 
             y = dist_from_edge)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to forest edge (RAW)",
       title = "Den Status vs. Distance to forest edge (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


f |>
  ggplot(aes(x = interaction(den_status_binary, age), 
             y = dist_from_edge,
             color = age)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.within_old", "Not active.within_old"),
                                 c("Active.within_young", "Not active.within_young")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to forest edge (RAW)",
       title = "Den Status vs. Distance to forest edge (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##### latitude #####

f |>
  ggplot(aes(x = den_status_binary, 
             y = latitude)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region) +
  labs(x = "Den Status",
       y = "Latitude",
       title = "Den Status vs. Latitude",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 

# maybe soooome clustering by latitude in HG - 
# further north has more 'Not active' frequency
# also definitely HIGH clustering of 'Obsolete' dens on VI
f |>
  ggplot(aes(x = latitude,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ region) +
  labs(x = "Latitude",
       y = "Density") +
  theme_minimal()


##### longitude #####

f |>
  ggplot(aes(x = den_status_binary, 
             y = longitude)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region) +
  labs(x = "Den Status",
       y = "Longitude",
       title = "Den Status vs. Longitude",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 

# definitely some clustering in 'Obsolete' dens related to longitude
# on Vancouver Island
f |>
  ggplot(aes(x = longitude,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ region) +
  labs(x = "Longitude",
       y = "Density") +
  theme_minimal()


##### elevation #####

f[["elevation_m"]][which(f$elevation_m == 999)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = elevation_m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Elevation (m)",
       title = "Den Status vs. Elevation (m)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = elevation_m,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Elevation (m)",
       y = "Density") +
  theme_minimal()


##### slope % #####

f[["slope_pct"]][which(f$slope_pct > 200)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = slope_pct)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Slope Grade (%)",
       title = "Den Status vs. Slope (%)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = slope_pct,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Slope Grade (%)",
       y = "Density") +
  theme_minimal()


##### slope aspect #####

f[["slope_aspect"]][which(f$slope_aspect > 360)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = slope_aspect)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Slope Aspect",
       title = "Den Status vs. Slope Aspect",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = slope_aspect,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Slope Aspect",
       y = "Density") +
  theme_minimal()


##### canopy closure #####

f[["canopy_closure"]][which(f$canopy_closure > 100)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = canopy_closure)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Canopy Closure",
       title = "Den Status vs. Canopy Closure",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = canopy_closure,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Canopy Closure",
       y = "Density") +
  theme_minimal()


##### bed depth #####

f[["bed_depth"]][which(abs(f$bed_depth) == 999)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = bed_depth)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Bed Depth (cm)",
       title = "Den Status vs. Bed Depth (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = bed_depth,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Bed Depth (cm)",
       y = "Density") +
  theme_minimal()


##### bed width #####

f[["bed_width"]][which(abs(f$bed_width) == 999)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = bed_width)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Bed Width (cm)",
       title = "Den Status vs. Bed Width (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = bed_width,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Bed Width (cm)",
       y = "Density") +
  theme_minimal()


##### bed length #####

f[["bed_length"]][which(abs(f$bed_length) == 999)] <- NA

f |>
  ggplot(aes(x = den_status_binary, 
             y = bed_length)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Bed Length (cm)",
       title = "Den Status vs. Bed Length (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f |>
  ggplot(aes(x = bed_length,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Bed Length (cm)",
       y = "Density") +
  theme_minimal()


# ONLY FOR SURE ACTIVE/NON ACTIVE -----------------------------------------


for_sure_dens <- c("Active in last denning season", 
                   "Not active in last season",
                   "Not active in last season, but recent use (1-4 seasons)",
                   "Not active in last season, no recent use (>4 seasons)",
                   "No recent evidence of use (>4 seasons)")

##### % forested #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = f_prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (RAW field values)",
       title = "Den Status vs. % Forested (RAW)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


# f[which(f$den_status %in% for_sure_dens), ] |>
#   ggplot(aes(x = den_status_binary, 
#              y = prop_forest_60m)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
#   theme_minimal() +
#   labs(x = "Den Status",
#        y = "Proportion Forested (VERIFIED)",
#        title = "Den Status vs. % Forested (GIS VERIFIED)",
#        subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


##### dist <40 #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = f_dist_lt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to <40 (RAW)",
       title = "Den Status vs. Distance to <40 yo forest (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


# f[which(f$den_status %in% for_sure_dens), ] |>
#   ggplot(aes(x = den_status_binary, 
#              y = dist_lt40)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
#   theme_minimal() +
#   scale_y_continuous(trans = 'log10') +
#   annotation_logticks() +
#   labs(x = "Den Status",
#        y = "log Distance to <40 (VERIFIED)",
#        title = "Den Status vs. Distance to <40 yo forest (GIS VERIFIED)",
#        subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season") 


##### dist >40 #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = f_dist_gt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to >40 (RAW)",
       title = "Den Status vs. Distance to >40 yo forest (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


# f[which(f$den_status %in% for_sure_dens), ] |>
#   ggplot(aes(x = den_status_binary, 
#              y = dist_gt40)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
#   theme_minimal() +
#   scale_y_continuous(trans = 'log10') +
#   annotation_logticks() +
#   labs(x = "Den Status",
#        y = "log Distance to >40 (VERIFIED)",
#        title = "Den Status vs. Distance to >40 yo forest (GIS VERIFIED)",
#        subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season") 


##### dist to nearest road #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = f_dist_road)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to nearest road (RAW)",
       title = "Den Status vs. Distance to nearest road (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


##### distance to edge #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = dist_from_edge)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to forest edge (RAW)",
       title = "Den Status vs. Distance to forest edge (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = interaction(den_status_binary, age), 
             y = dist_from_edge,
             color = age)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.within_old", "Not active.within_old"),
                                 c("Active.within_young", "Not active.within_young")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to forest edge (RAW)",
       title = "Den Status vs. Distance to forest edge (RAW field values)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##### latitude #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = latitude)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region) +
  labs(x = "Den Status",
       y = "Latitude",
       title = "Den Status vs. Latitude",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = latitude,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ region) +
  labs(x = "Latitude",
       y = "Density") +
  theme_minimal()

##### longitude #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = longitude)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region) +
  labs(x = "Den Status",
       y = "Longitude",
       title = "Den Status vs. Longitude",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 

# some clustering in VI
f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = longitude,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  facet_wrap(~ region) +
  labs(x = "Longitude",
       y = "Density") +
  theme_minimal()



##### elevation #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = elevation_m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Elevation (m)",
       title = "Den Status vs. Elevation (m)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = elevation_m,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Elevation (m)",
       y = "Density") +
  theme_minimal()


##### slope % #####


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = slope_pct)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Slope Grade (%)",
       title = "Den Status vs. Slope (%)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = slope_pct,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Slope Grade (%)",
       y = "Density") +
  theme_minimal()


##### slope aspect #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = slope_aspect)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Slope Aspect",
       title = "Den Status vs. Slope Aspect",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = slope_aspect,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Slope Aspect",
       y = "Density") +
  theme_minimal()


##### canopy closure #####


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = canopy_closure)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Canopy Closure",
       title = "Den Status vs. Canopy Closure",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = canopy_closure,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Canopy Closure",
       y = "Density") +
  theme_minimal()


##### bed depth #####


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = bed_depth)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Bed Depth (cm)",
       title = "Den Status vs. Bed Depth (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = bed_depth,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Bed Depth (cm)",
       y = "Density") +
  theme_minimal()


##### bed width #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = bed_width)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Bed Width (cm)",
       title = "Den Status vs. Bed Width (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = bed_width,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Bed Width (cm)",
       y = "Density") +
  theme_minimal()


##### bed length #####

f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = bed_length)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Bed Length (cm)",
       title = "Den Status vs. Bed Length (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = bed_length,
             color = den_status_binary,
             fill = den_status_binary)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Den Status") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Den Status") +
  labs(x = "Bed Length (cm)",
       y = "Density") +
  theme_minimal()


# HAIR IN BED -------------------------------------------------------------

# Let's try looking at the response of just the 'hair' variables

#f$hair_on_entrance
f$hair_in_bed # this one seems to maybe be it?
#f$hair_removed_den_entrance
#f$hair_samples_taken

f$hair_in_bed <- factor(f$hair_in_bed,
                        levels = c("Yes", "No", "Yes - Unchanged", "Unknown", NA))

##### % forested #####

# BOXPLOTS

f[which(f$hair_in_bed %in% c("Yes", "No")),] |>
  ggplot(aes(x = hair_in_bed, 
             y = f_prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")),
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Proportion Forested (RAW field values)",
       title = "Hair Presence vs. % Forested (RAW)")


# f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
#   ggplot(aes(x = hair_in_bed, 
#              y = prop_forest_60m)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
#   theme_minimal() +
#   labs(x = "Hair in Bed",
#        y = "Proportion Forested (VERIFIED)",
#        title = "Hair Presence vs. % Forested (GIS VERIFIED)")


# DENSITY PLOTS

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = f_prop_forest_60m,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#CC79A7"),
                     name = "Hair in Bed") +
  scale_fill_manual(values = c("#E69F00", "#CC79A7"),
                    name = "Hair in Bed") +
  labs(x = "Proportion Forested (RAW)",
       y = "Density") +
  theme_minimal()


# f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
#   ggplot(aes(x = prop_forest_60m,
#              color = hair_in_bed,
#              fill = hair_in_bed)) +
#   geom_density(alpha = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#CC79A7"),
#                      name = "Hair in Bed") +
#   scale_fill_manual(values = c("#E69F00", "#CC79A7"),
#                     name = "Hair in Bed") +
#   labs(x = "Proportion Forested (VERIFIED)",
#        y = "Density") +
#   theme_minimal()


##### dist <40 #####

# BOXPLOTS

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = f_dist_lt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to <40 (RAW)",
       title = "Hair Presence vs. Distance to <40 yo forest (RAW field values)") 


# f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
#   ggplot(aes(x = hair_in_bed, 
#              y = dist_lt40)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
#   theme_minimal() +
#   scale_y_continuous(trans = 'log10') +
#   annotation_logticks() +
#   labs(x = "Hair in Bed",
#        y = "log Distance to <40 (VERIFIED)",
#        title = "Hair Presence vs. Distance to <40 yo forest (GIS VERIFIED)")


# DENSITY PLOTS

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = f_dist_lt40,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#CC79A7"),
                     name = "Hair in Bed") +
  scale_fill_manual(values = c("#E69F00", "#CC79A7"),
                    name = "Hair in Bed") +
  labs(x = "Distance to <40 (RAW)",
       y = "Density") +
  theme_minimal()


# f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
#   ggplot(aes(x = dist_lt40,
#              color = hair_in_bed,
#              fill = hair_in_bed)) +
#   geom_density(alpha = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#CC79A7"),
#                      name = "Hair in Bed") +
#   scale_fill_manual(values = c("#E69F00", "#CC79A7"),
#                     name = "Hair in Bed") +
#   labs(x = "Distance to <40 (VERIFIED)",
#        y = "Density") +
#   theme_minimal()


##### dist >40 #####

# BOXPLOTS

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = f_dist_gt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to >40 (RAW)",
       title = "Hair Presence vs. Distance to >40 yo forest (RAW field values)") 



# f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
#   ggplot(aes(x = hair_in_bed, 
#              y = dist_gt40)) +
#   geom_boxplot() +
#   geom_jitter() +
#   geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
#   theme_minimal() +
#   scale_y_continuous(trans = 'log10') +
#   annotation_logticks() +
#   labs(x = "Hair in Bed",
#        y = "log Distance to >40 (VERIFIED)",
#        title = "Hair Presence vs. Distance to >40 yo forest (GIS VERIFIED)")


# DENSITY PLOTS

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = f_dist_gt40,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#CC79A7"),
                     name = "Hair in Bed") +
  scale_fill_manual(values = c("#E69F00", "#CC79A7"),
                    name = "Hair in Bed") +
  labs(x = "Distance to >40 (RAW)",
       y = "Density") +
  theme_minimal()


# f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
#   ggplot(aes(x = dist_gt40,
#              color = hair_in_bed,
#              fill = hair_in_bed)) +
#   geom_density(alpha = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#CC79A7"),
#                      name = "Hair in Bed") +
#   scale_fill_manual(values = c("#E69F00", "#CC79A7"),
#                     name = "Hair in Bed") +
#   labs(x = "Distance to >40 (VERIFIED)",
#        y = "Density") +
#   theme_minimal()

# Run summary stats of the dist_gt40 col by hair_in_bed categories ("Yes",
# "No", "Yes - Unchanged", "Unknown")
psych::describeBy(f$f_dist_gt40, 
                  f$hair_in_bed, 
                  mat = TRUE)


##### dist to nearest road #####
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = f_dist_road)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to nearest road (RAW)",
       title = "Hair Presence vs. Distance to nearest road (RAW field values)") 




##### distance to edge #####
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = den_status_binary, 
             y = dist_from_edge)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to forest edge (RAW)",
       title = "Hair Presence vs. Distance to forest edge (RAW field values)")


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = interaction(den_status_binary, age), 
             y = dist_from_edge,
             color = age)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active.within_old", "Not active.within_old"),
                                 c("Active.within_young", "Not active.within_young")), 
              map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to forest edge (RAW)",
       title = "Hair Presence vs. Distance to forest edge (RAW field values)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



##### latitude #####

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = latitude)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region) +
  labs(x = "Hair in Bed",
       y = "Longitude",
       title = "Hair Presence vs. Latitude") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = latitude,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  facet_wrap(~ region) +
  labs(x = "Latitude",
       y = "Density") +
  theme_minimal()


##### longitude #####

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = longitude)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  #annotation_logticks(side = "l") +
  facet_wrap(~ region) +
  labs(x = "Hair in Bed",
       y = "Longitude",
       title = "Hair Presence vs. Longitude") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = longitude,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  facet_wrap(~ region) +
  labs(x = "Longitude",
       y = "Density") +
  theme_minimal()


##### elevation #####

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = elevation_m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Elevation (m)",
       title = "Hair Presence vs. Elevation (m)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = elevation_m,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Elevation (m)",
       y = "Density") +
  theme_minimal()


##### slope % #####


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = slope_pct)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Slope Grade (%)",
       title = "Hair Presence vs. Slope (%)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = slope_pct,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Slope Grade (%)",
       y = "Density") +
  theme_minimal()


##### slope aspect #####

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = slope_aspect)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Slope Aspect",
       title = "Hair Presence vs. Slope Aspect",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = slope_aspect,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Slope Aspect",
       y = "Density") +
  theme_minimal()


##### canopy closure #####


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = canopy_closure)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Canopy Closure",
       title = "Hair Presence vs. Canopy Closure",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = canopy_closure,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Canopy Closure",
       y = "Density") +
  theme_minimal()


##### bed depth #####


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = bed_depth)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Bed Depth (cm)",
       title = "Hair Presence vs. Bed Depth (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = bed_depth,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Bed Depth (cm)",
       y = "Density") +
  theme_minimal()


##### bed width #####

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = bed_width)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Bed Width (cm)",
       title = "Hair Presence vs. Bed Width (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = bed_width,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Bed Width (cm)",
       y = "Density") +
  theme_minimal()


##### bed length #####

f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = bed_length)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Bed Length (cm)",
       title = "Hair Presence vs. Bed Length (cm)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = bed_length,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                     name = "Hair Presence") +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7", "purple"),
                    name = "Hair Presence") +
  labs(x = "Bed Length (cm)",
       y = "Density") +
  theme_minimal()


# CHI SQUARES -------------------------------------------------------------

##### Den status x Hair #####


f |>
  dplyr::filter(hair_in_bed %in% c("Yes", "No")) |>
  dplyr::group_by(hair_in_bed, den_status) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = den_status,
                     values_from = n) |>
  tibble::column_to_rownames(var = "hair_in_bed") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Hair in Bed x Den Status")


chisq <- f |>
  dplyr::filter(hair_in_bed %in% c("Yes", "No")) |>
  dplyr::group_by(hair_in_bed, den_status) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = hair_in_bed,
                     values_from = n) |>
  dplyr::mutate(den_status = dplyr::if_else(!is.na(den_status), den_status, "NA")) |>
  tibble::column_to_rownames(var = "den_status") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

chisq$observed
round(chisq$expected, 1)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)

# For sure dens
f |>
  dplyr::filter(hair_in_bed %in% c("Yes", "No"),
                den_status %in% for_sure_dens) |>
  dplyr::group_by(hair_in_bed, den_status) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = den_status,
                     values_from = n) |>
  tibble::column_to_rownames(var = "hair_in_bed") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>  #chisq.test()
  as.table() |>
  gplots::balloonplot(main = "Hair in Bed x Den Status")


# Lenient binary
f |>
  dplyr::filter(hair_in_bed %in% c("Yes", "No"),
                den_status_binary %in% c("Active", "Not active")) |>
  dplyr::group_by(hair_in_bed, den_status_binary) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = hair_in_bed,
                     values_from = n) |>
  tibble::column_to_rownames(var = "den_status_binary") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |> # chisq.test()
  as.table() |>
  gplots::balloonplot(main = "Hair in Bed x Den Status Binary (lenient)")

chisq <- f |>
  dplyr::filter(hair_in_bed %in% c("Yes", "No"),
                den_status_binary %in% c("Active", "Not active")) |>
  dplyr::group_by(hair_in_bed, den_status_binary) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = hair_in_bed,
                     values_from = n) |>
  tibble::column_to_rownames(var = "den_status_binary") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

chisq$observed
round(chisq$expected, 1)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)


##### Den Status x Region #####

f |>
  #dplyr::filter(den_status %in% for_sure_dens) |>
  dplyr::group_by(region, den_status_binary) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = den_status_binary,
                     values_from = n) |>
  tibble::column_to_rownames(var = "region") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Den Status x Region")


chisq <- f |>
  dplyr::group_by(region, den_status) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = region,
                     values_from = n) |>
  dplyr::mutate(den_status = dplyr::if_else(!is.na(den_status), den_status, "NA")) |>
  tibble::column_to_rownames(var = "den_status") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

chisq$observed
round(chisq$expected, 1)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)


rm(chisq)


##### Den Status x Age Class #####

# Note that this is the age class at the time of the
# first visit (i.e., from the static data). It could
# have changed at some point during the study.

f |>
  #dplyr::filter(den_status %in% for_sure_dens) |>
  dplyr::filter(!is.na(age_class)) |>
  dplyr::group_by(age_class, den_status_binary) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = den_status_binary,
                     values_from = n) |>
  tibble::column_to_rownames(var = "age_class") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Den Status x Age Class")


chisq <- f |>
  dplyr::filter(!is.na(age_class)) |>
  dplyr::group_by(age_class, den_status) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = age_class,
                     values_from = n) |>
  dplyr::mutate(den_status = dplyr::if_else(!is.na(den_status), den_status, "NA")) |>
  tibble::column_to_rownames(var = "den_status") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

chisq$observed
round(chisq$expected, 1)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)


rm(chisq)


# % AGE CLASS -------------------------------------------------------------

prct <- tar_read(prct_age_class_1.5km)

# Bin the data into groups (sort of ~young, old, medium, very old)
prct$lt_3 <- rowSums(prct[,c("age_class_1", "age_class_2")])
prct$gt_3 <- rowSums(prct[,6:12])
prct$three_to_7 <- rowSums(prct[,6:10])
prct$gt_8 <- rowSums(prct[,c("age_class_8", "age_class_9")])

# Merge with den status - so we can look at percent of each
# age class with activity status
f <- merge(f, prct, by.x = "sample_id_forest", by.y = "sample_id")
f <- dplyr::select(f, -den_id.y, -year.y)
names(f)[2:3] <- c("den_id", "year")


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
  theme_minimal()

# Mean % area by young/old across all dens + years:
prct |> 
  dplyr::select(year, lt_3, gt_3) |>
  tidyr::pivot_longer(cols = 2:3) |>
  dplyr::group_by(year, name) |>
  dplyr::summarise(value = mean(value)) |>
  ggplot(aes(x = year, 
             y = value, 
             fill = name)) +
  geom_area() +
  theme_minimal()


# Mean % area by young/medium/old across all dens + years:
prct |> 
  dplyr::select(year, lt_3, three_to_7, gt_8) |>
  tidyr::pivot_longer(cols = 2:4) |>
  dplyr::mutate(name = factor(name, levels = c("gt_8", "three_to_7", "lt_3"))) |>
  dplyr::group_by(year, name) |>
  dplyr::summarise(value = mean(value)) |>
  ggplot(aes(x = year, 
             y = value, 
             fill = name)) +
  geom_area() +
  theme_minimal()



f |>
  dplyr::filter(den_status_binary %in% c("Active", "Not active")) |>
  dplyr::select(1:32) |>
  tidyr::pivot_longer(cols = 24:32, 
                      names_to = "age_class", 
                      values_to = "prct_age") |> 
  dplyr::mutate(age_class = factor(age_class, levels = c("age_class_1",
                                                         "age_class_2",
                                                         "age_class_3",
                                                         "age_class_4",
                                                         "age_class_5",
                                                         "age_class_6",
                                                         "age_class_7",
                                                         "age_class_8",
                                                         "age_class_9"))) |>
  #dplyr::filter(age_class == "age_class_9") |>
  ggplot(aes(x = interaction(den_status_binary, age_class),
             y = prct_age,
             color = den_status_binary)) +
  geom_jitter(stroke = NA,
              alpha = 0.3,) +
  geom_boxplot(fill = NA) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# ROAD DENSITY ------------------------------------------------------------


tar_load(road_density)

road_density <- road_density[,c("sample_id", "road_density_m2")]

# Note the all.x = TRUE. If the den isn't in
# the road density df, the road density is 0.
f <- merge(f, road_density, by.x = "sample_id_forest", by.y = "sample_id", all.x = TRUE)
f$road_density_m2 <- ifelse(is.na(f$road_density_m2), 0, f$road_density_m2)

ggplot(f, aes(x = den_status_binary,
              y = road_density_m2)) +
  geom_jitter() +
  geom_boxplot(fill = NA) + 
  geom_signif(comparisons = list(c("Active", "Not active"))) +
  theme_minimal()


# CORRELATION IN COVARIATES -----------------------------------------------


# We are interested in the following variables:
# - 1. Proportion forested
# - 2a. Distance to cutblock
# - 2b. Distance to mature forest
# - 2c. Distance to forest edge
# - 3. Age of den stand (within intact mature forest vs. within cutblock)
# - 4. Proportion of >250 yo forest within 1.5km
# - 5. Road density within 1.5km
# - 6. Year, to control for yearly variation in non-forestry metrics (e.g., prey availability)
# - 7. Den ID, to control for forestry patterns across dens
# - 8. Latitude (Y) and longitude (X), to control for spatial autocorrelation

# TODO: correlation matrices for these variables. Then re-do models.

# Distance from X variables are all zero inflated and highly skewed.
# Take the log of those variables.
f$log_dist_lt40 <- log(f$f_dist_lt40 + 1)
f$log_dist_gt40 <- log(f$f_dist_gt40 + 1)
f$log_dist_road <- log(f$f_dist_road + 1)

cor_mtx <- cor(f[,c("f_prop_forest_60m", "log_dist_lt40", "log_dist_gt40", "log_dist_road", 
                    "log_dist_from_edge", "year", "latitude", "longitude",
                    "windthrow_prct", "lt_3", "gt_8", "road_density_m2")], 
               use = "pairwise.complete.obs")

corrplot::corrplot(cor_mtx, is.corr = FALSE, insig = "p-value")

# Check for correlations with FULL dataset
GGally::ggpairs(f[,c("den_status_binary", "hair_in_bed", "region",
                     "f_prop_forest_60m", "log_dist_lt40", "log_dist_gt40", "log_dist_road", 
                     "log_dist_from_edge", "age", "year", "latitude", "longitude",
                     "windthrow_prct", "lt_3", "gt_8", "road_density_m2")])

# Check for correlations with ONLY FOR SURE DENS dataset
GGally::ggpairs(f[which(f$den_status %in% for_sure_dens),
                  c("den_status_binary", "hair_in_bed", "region",
                     "f_prop_forest_60m", "log_dist_lt40", "log_dist_gt40", "log_dist_road", 
                     "log_dist_from_edge", "age", "year", "latitude", "longitude",
                     "windthrow_prct", "lt_3", "gt_8", "road_density_m2")])

# BANNED COVARIATES!
# The following variables are highly correlated and SHOULD NOT BE covariates:
# f_prop_forest_60m and log_dist_lt40, log_dist_gt40, log_dist_road, log_dist_from_edge, windthrow_prct, lt_3
# log_dist_lt40 and f_prop_forest_60m, log_dist_gt40, log_dist_road, log_dist_from_edge, windthrow_prct, lt_3
# log_dist_gt40 and f_prop_forest_60m, log_dist_lt40, log_dist_road, lt_3
# log_dist_road and f_prop_forest_60m, log_dist_lt40, log_dist_gt40, lt_3
# log_dist_from_edge and f_prop_forest_60m, log_dist_lt40, log_dist_road, windthrow_prct, lt_3
# year and windthrow_prct, lt_3
# windthrow_prct and f_prop_dist_60m, log_dist_lt40, log_dist_road, log_dist_from_edge, year
# lt_3 and f_prop_forest_60m, log_dist_lt40, log_dist_gt40, log_dist_from_edge, gt_8, road_density_m2
# gt_8 and lt_3, road_density_m2
# road_density_m2 and lt_3, gt_3


# LINEAR MODEL - DEN STATUS -----------------------------------------------


# How about all those variables and den status eh?

# Subset to data of interest/suitability for ONLY FOR SURE DENS
# I.e. exclude any "Active in last 4 seasons" ones
dat <- f[f$den_status_binary %in% c("Active", "Not active") & f$den_status %in% for_sure_dens, ]
dat <- dat[, c("den_status_binary", "den_id", "year", "age", 
               "region", "latitude", "longitude",
               "f_prop_forest_60m", "log_dist_lt40", "log_dist_gt40", "log_dist_road",
               "log_dist_from_edge", "lt_3", "gt_8", "road_density_m2")]
dat <- dat[complete.cases(dat),]

# Convert den_status_binary to 0 or 1
dat$den_status_binary <- ifelse(dat$den_status_binary == "Active", 1, 0)

# Examine vars
hist(dat$log_dist_from_edge)
hist(dat$log_dist_lt40)
hist(dat$log_dist_gt40) # hmm
hist(dat$log_dist_road)
hist(dat$road_density_m2)

plyr::count(dat$age)
# ah, going to have to cut out edge ones
ggplot(dat, aes(y = log_dist_from_edge, x = age, color = age)) + 
  geom_jitter() +
  geom_boxplot(fill = NA) 
# on_edge is perfectly colinear with a distance of 0. Will be rank deficient

# Confirm the variances are equal between our categorical variables
# ehhh it's borderline
car::leveneTest(den_status_binary ~ age, dat) # null hypothesis: variances are not significantly different
car::leveneTest(den_status_binary ~ age, dat[which(dat$age != "on_edge"), ]) # null hypothesis: variances are not significantly different
# So, because the variances are NOT even between the groups...
# https://stats.stackexchange.com/questions/34325/regression-modelling-with-unequal-variance
# These models are not perfect - they violate the assumption of even variances
# between your groups in your covariates, but they are a starting point.

# Chi-square is not sensitive to uneven variances.
chisq.test(x = dat$den_status_binary, y = dat$age, simulate.p.value = TRUE)

dat |>
  dplyr::select(den_status_binary, age) |>
  dplyr::group_by(den_status_binary, age) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = age,
                     values_from = n) |>
  tibble::column_to_rownames(var = "den_status_binary") |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Den Status x Age")

# Let's drop "on_edge" bc it's perfectly == 0 in distance.
dat <- dat[which(dat$age != "on_edge"),]

# Examine region
# Nice - even variances there
car::leveneTest(den_status_binary ~ region, dat) # null hypothesis: variances are not significantly different

# Rescale numeric responses
# REMEMBER TO BACK-TRANSFORM IF NECESSARY.
dat$s_prop_forest_60m <- scale(dat$f_prop_forest_60m)[,1]
dat$s_dist_lt40 <- scale(dat$log_dist_lt40)[,1]
dat$s_dist_gt40 <- scale(dat$log_dist_gt40)[,1]
dat$s_dist_from_edge <- scale(dat$log_dist_from_edge)[,1]
dat$s_dist_road <- scale(dat$log_dist_road)[,1]
dat$s_year <- scale(dat$year)[,1]
hist(dat$s_dist_from_edge)
hist(dat$s_prop_forest_60m)
hist(dat$s_dist_lt40)
hist(dat$s_dist_gt40)
hist(dat$s_dist_road)
hist(dat$s_year)

# Split into test and train
train.samples <- caret::createDataPartition(dat$den_status_binary, p = 0.8, list = FALSE)
train.data <- dat[train.samples,]
test.data <- dat[-train.samples,]

# If you get model convergence issues, you can try to test different
# optimizers. E.g.
#allFit(modelx)

# Fit the models!
# Just the basics - each forest metric - effectively same as examining boxplots for significance
model00 <- glm(den_status_binary ~ s_year, data = train.data, family = binomial)
model0r <- glm(den_status_binary ~ region, data = train.data, family = binomial)
model0a <- glm(den_status_binary ~ s_prop_forest_60m , data = train.data, family = binomial)
model0b <- glm(den_status_binary ~ s_dist_lt40 , data = train.data, family = binomial)
model0c <- glm(den_status_binary ~ s_dist_gt40 , data = train.data, family = binomial)
model0d <- glm(den_status_binary ~ age , data = train.data, family = binomial)
model0e <- glm(den_status_binary ~ s_dist_from_edge , data = train.data, family = binomial)
model0f <- glm(den_status_binary ~ s_dist_from_edge * age , data = train.data, family = binomial)
model0g <- glm(den_status_binary ~ s_dist_road , data = train.data, family = binomial)
model0h <- glm(den_status_binary ~ s_dist_road * age , data = train.data, family = binomial)
model0i <- glm(den_status_binary ~ gt_8 , data = train.data, family = binomial)
model0j <- glm(den_status_binary ~ lt_3 , data = train.data, family = binomial)
model0k <- glm(den_status_binary ~ road_density_m2 , data = train.data, family = binomial)
model0l <- glm(den_status_binary ~ gt_8 + s_dist_lt40 , data = train.data, family = binomial)
model0m <- glm(den_status_binary ~ gt_8 + s_prop_forest_60m , data = train.data, family = binomial)
model0n <- glm(den_status_binary ~ road_density_m2 + s_dist_road , data = train.data, family = binomial)
model0o <- glm(den_status_binary ~ s_dist_lt40 * region, data = train.data, family = binomial)

models_names <- ls()[grep("model0", ls())]
models_list <- lapply(models_names, get)
names(models_list) <- models_names
bbmle::AICtab(models_list, weights = TRUE) # They are all almost identical models in terms of AIC! delta AIC >= 4 == better fit

# A significant intercept tells us that if you set all the 
# variables to 0, the response mean is significantly different 
# from 0. It doesn't tell us much other than "if proportion forest, 
# distance <40 and distance >40 all equal zero, den_status_binary 
# is different than if the variables are all >0." At most, it can 
# tell you with confidence that your variables have some influence
# on the response, but not much more than that. 
# This is complicated further by the fact that dist_lt40 and dist_gt40
# have a perfectly inverse relationship - at least one of them will
# ALWAYS be zero

summary(model00)
summary(model0r)
summary(model0a)
summary(model0b)
summary(model0c)
summary(model0d)
summary(model0e)
summary(model0f)
summary(model0g)
summary(model0h)
summary(model0i)
summary(model0j)
summary(model0k)
summary(model0l)
summary(model0m)
summary(model0n)
summary(model0o)

# Visualize the crossover here between den_id and den_status - 
# don't want perfect 1:1 correlation otherwise you get 
# convergence issues with the random effects
ggplot(dat, aes(x = s_prop_forest_60m, y = den_status_binary, color = den_id)) +
  geom_point() +
  ggalt::geom_encircle(aes(group=den_id)) +
  theme(legend.position="none")

jtools::summ(model00) # This slightly more detailed output shows that den_id explains nearly ALL the status.

# Examine residuals
DHARMa::testResiduals(model00)
DHARMa::testResiduals(model0a)
DHARMa::testResiduals(model0b)
DHARMa::testResiduals(model0c)
DHARMa::testResiduals(model0d)
DHARMa::testResiduals(model0e)
DHARMa::testResiduals(model0f)
DHARMa::testResiduals(model0g)
DHARMa::testResiduals(model0h)
DHARMa::testResiduals(model0i)
DHARMa::testResiduals(model0j)
DHARMa::testResiduals(model0k)
DHARMa::testResiduals(model0l)
DHARMa::testResiduals(model0m)
DHARMa::testResiduals(model0n)

# Since all the models are extremely similar to
# each other and have nice residuals, let's examine
# the model average as well.
model_ave <- MuMIn::model.avg(models_list)
summary(model_ave) # literally just year :(

# Check predictive power
# TODO: improve/expand
# Aside from AIC, another metric to check is the AUC,
# or area under [ROC] curve. This can help assess model
# accuracy and precision: how many times does it correctly
# predict occupancy (1) vs non-occupancy (0)?

# Make predictions
probs <- predict(model00, test.data, type = "response", allow.new.levels = TRUE)
resp <- ifelse(probs > 0.5, 1, 0)

# Model accuracy
mean(resp == test.data$den_status_binary) 



# LINEAR MODEL - HAIR IN BED ---------------------------------------------------

# So that kinda sucked. What about hair_in_bed?

# Hair in Bed

# Subset to data of interest/suitability
dat <- f[f$hair_in_bed %in% c("Yes", "No"), ]
dat <- dat[, c("hair_in_bed", "den_id", "year", "age",
               "f_prop_forest_60m", "log_dist_lt40", "log_dist_gt40", "log_dist_road",
               "log_dist_from_edge", "lt_3", "gt_8", "road_density_m2")]
dat <- dat[complete.cases(dat),]

# Convert hair_in_bed to 0 or 1
dat$hair_in_bed <- ifelse(dat$hair_in_bed == "Yes", 1, 0)

# Rescale numeric responses
# REMEMBER TO BACK-TRANSFORM IF NECESSARY.
dat$s_prop_forest_60m <- scale(dat$f_prop_forest_60m)[,1]
dat$s_dist_lt40 <- scale(dat$log_dist_lt40)[,1]
dat$s_dist_gt40 <- scale(dat$log_dist_gt40)[,1]
dat$s_dist_from_edge <- scale(dat$log_dist_from_edge)[,1]
dat$s_dist_road <- scale(dat$log_dist_road)[,1]
dat$s_year <- scale(dat$year)[,1]
hist(dat$s_dist_from_edge)
hist(dat$s_prop_forest_60m)
hist(dat$s_dist_lt40)
hist(dat$s_dist_gt40)
hist(dat$s_dist_road)
hist(dat$s_year)

# Split into test and train
train.samples <- caret::createDataPartition(dat$hair_in_bed, p = 0.8, list = FALSE)
train.data <- dat[train.samples,]
test.data <- dat[-train.samples,]

# Fit the models!
# Just the basics - each forest metric - effectively same as examining boxplots for significance
model00 <- glmer(hair_in_bed ~ s_year + (1|den_id), data = train.data, family = binomial)
model0a <- glmer(hair_in_bed ~ s_prop_forest_60m + s_year + (1|den_id), data = train.data, family = binomial)
model0b <- glmer(hair_in_bed ~ s_dist_lt40 + s_year + (1|den_id), data = train.data, family = binomial)
model0c <- glmer(hair_in_bed ~ s_dist_gt40 + s_year + (1|den_id), data = train.data, family = binomial)
model0d <- glmer(hair_in_bed ~ age + s_year + (1|den_id), data = train.data, family = binomial)
model0e <- glmer(hair_in_bed ~ s_dist_from_edge + s_year + (1|den_id), data = train.data, family = binomial)
model0f <- glmer(hair_in_bed ~ s_dist_from_edge * age + s_year + (1|den_id), data = train.data, family = binomial)
model0g <- glmer(hair_in_bed ~ s_dist_road + s_year + (1|den_id), data = train.data, family = binomial)
model0h <- glmer(hair_in_bed ~ s_dist_road * age + s_year + (1|den_id), data = train.data, family = binomial)
model0i <- glmer(hair_in_bed ~ gt_8 + s_year + (1|den_id), data = train.data, family = binomial)
model0j <- glmer(hair_in_bed ~ lt_3 + s_year + (1|den_id), data = train.data, family = binomial)
model0k <- glmer(hair_in_bed ~ road_density_m2 + s_year + (1|den_id), data = train.data, family = binomial)
model0l <- glmer(hair_in_bed ~ gt_8 + s_dist_lt40 + s_year + (1|den_id), data = train.data, family = binomial)
model0m <- glmer(hair_in_bed ~ gt_8 + s_prop_forest_60m + s_year + (1|den_id), data = train.data, family = binomial)
model0n <- glmer(hair_in_bed ~ road_density_m2 + s_dist_road + s_year + (1|den_id), data = train.data, family = binomial)

models_names <- ls()[grep("model0", ls())]
models_list <- lapply(models_names, get)
names(models_list) <- models_names
bbmle::AICtab(models_list, weights = TRUE) # They are all almost identical models in terms of AIC! delta AIC >= 4 == better fit

summary(model00)
summary(model0a)
summary(model0b)
summary(model0c)
summary(model0d)
summary(model0e)
summary(model0f)
summary(model0g)
summary(model0h)
summary(model0i)
summary(model0j)
summary(model0k)
summary(model0l)
summary(model0m)
summary(model0n)

# Plot confirms that all these replicates per den site can mask effects
# and cause model convergence issues.
# Ben Bolker suggests aggregating data by group in the SO question above. 
# In our case, aggregating data by before/after treatment seems to be best bet!
ggplot(dat, aes(x = s_prop_forest_60m, y = hair_in_bed, color = den_id)) +
  geom_point() +
  ggalt::geom_encircle(aes(group=den_id)) +
  theme(legend.position="none")

# Examine residuals
DHARMa::testResiduals(model00)
DHARMa::testResiduals(model0a)
DHARMa::testResiduals(model0b)
DHARMa::testResiduals(model0c)
DHARMa::testResiduals(model0d)
DHARMa::testResiduals(model0e)
DHARMa::testResiduals(model0f)
DHARMa::testResiduals(model0g)
DHARMa::testResiduals(model0h)
DHARMa::testResiduals(model0i)
DHARMa::testResiduals(model0j)
DHARMa::testResiduals(model0k)
DHARMa::testResiduals(model0l)
DHARMa::testResiduals(model0m)
DHARMa::testResiduals(model0n)

# Since all the models are extremely similar to
# each other and have nice residuals, let's examine
# the model average as well.
model_ave <- MuMIn::model.avg(models_list)
summary(model_ave) # just proportion forested


# Next: see if you can either 1) merge to get 1-2 records per
# den (before/after forestry data changes) OR bootstrap it.