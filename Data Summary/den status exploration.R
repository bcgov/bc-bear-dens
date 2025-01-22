
# TOP ---------------------------------------------------------------------

# Load up data
library(targets)
tar_load(f)

# Load up other libraries
library(ggplot2)
library(ggsignif)
library(lme4)

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
f_a <- f[,c(1:9, 22)] # select columns 1:9 (den info) + column 22 (year)
f_b <- f[,c(1:2, 10:22)] # select columns 1:2 (sample_id, den_id) + 10:22 (forestry info). 

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
names(f)[13:23] <- c("nearest_tree_m", 
                     "f_prop_forest_60m", "v_prop_forest_60m",
                     "f_dist_lt40", "v_dist_lt40",
                     "f_dist_gt40", "v_dist_gt40", 
                     "f_dist_road", "v_dist_road",
                     "windthrow_prct", "windthrow_code")

rm(f_a, f_b)

# ALL BINARY VARS ---------------------------------------------------------


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

# % forested
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


# dist <40

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



# dist >40


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


# dist to nearest road
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


# ONLY FOR SURE ACTIVE/NON ACTIVE -----------------------------------------


for_sure_dens <- c("Active in last denning season", 
                   "Not active in last season",
                   "Not active in last season, but recent use (1-4 seasons)",
                   "Not active in last season, no recent use (>4 seasons)",
                   "No recent evidence of use (>4 seasons)")

# % forested

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


# dist <40

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


# dist >40

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


# dist to nearest road

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



# HAIR IN BED -------------------------------------------------------------

# Let's try looking at the response of just the 'hair' variables

f$hair_on_entrance
f$hair_in_bed # this one seems to maybe be it?
f$hair_removed_den_entrance
f$hair_samples_taken

f$hair_in_bed <- factor(f$hair_in_bed,
                        levels = c("Yes", "No", "Yes - Unchanged", "Unknown", NA))

# % forested

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


# dist <40

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


# dist >40

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


# dist to nearest road
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



# CHI SQUARE ACTIVE X HAIR ------------------------------------------------


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



# LINEAR MODEL - HAIR IN BED ---------------------------------------------------


# Hair in Bed

# Subset to data of interest/suitability
dat <- f[f$hair_in_bed %in% c("Yes", "No"), ]
dat <- dat[, c("hair_in_bed", "den_id", "year", "f_prop_forest_60m", "f_dist_lt40", "f_dist_gt40")]
dat <- dat[complete.cases(dat),]

# Convert hair_in_bed to 0 or 1
dat$hair_in_bed <- ifelse(dat$hair_in_bed == "Yes", 1, 0)

# Next, make just one 'distance from edge' variable
dat$dist_from_edge <- ifelse(dat$f_dist_lt40 > 0, dat$f_dist_lt40, dat$f_dist_gt40)
hist(dat$dist_from_edge)
dat$log_dist_from_edge <- log(dat$dist_from_edge + 1)
hist(dat$log_dist_from_edge)

# Next, say if it's in old, new, or edge of forest.
dat <- dat |> dplyr::mutate(dist_lt40 = round(f_dist_lt40, 0),
                            dist_gt40 = round(f_dist_gt40, 0),
                            age = dplyr::case_when((f_dist_lt40 == f_dist_gt40) ~ "edge",
                                                   (f_dist_lt40 > f_dist_gt40) ~ "old",
                                                   (f_dist_lt40 < f_dist_gt40) ~ "young"))

# Confirm the variances are equal between our categorical variables
car::leveneTest(hair_in_bed ~ age, dat) # null hypothesis: variances are not significantly different

# Rescale other numeric responses - this means we can 
# 1) directly compare model coefficient sizes - all our variables are on the same scale
# and 2) helps with model convergance issues where some values of X are in the hundreds
# while others are in the single digits or tens of digits
dat$s_dist_from_edge <- scale(dat$dist_from_edge)[,1]
dat$s_prop_forest_60m <- scale(dat$f_prop_forest_60m)[,1]
dat$s_dist_lt40 <- scale(dat$f_dist_lt40)[,1]
dat$s_dist_gt40 <- scale(dat$f_dist_gt40)[,1]
hist(dat$s_dist_from_edge, breaks = 10)
hist(dat$s_prop_forest_60m)
hist(dat$s_dist_lt40)
hist(dat$s_dist_gt40)

# Split into test and train
train.samples <- caret::createDataPartition(dat$hair_in_bed, p = 0.8, list = FALSE)
train.data <- dat[train.samples,]
test.data <- dat[-train.samples,]

# Fit the models!
# Just the basics - each forest metric - effectively same as examining boxplots for significance
model0a <- glm(hair_in_bed ~ s_prop_forest_60m, data = train.data, family = binomial)
model0b <- glm(hair_in_bed ~ s_dist_lt40, data = train.data, family = binomial)
model0c <- glm(hair_in_bed ~ s_dist_gt40, data = train.data, family = binomial)
model0d <- glm(hair_in_bed ~ s_dist_from_edge, data = train.data, family = binomial)
# Full model - each distance category
model1a <- glm(hair_in_bed ~ s_prop_forest_60m + s_dist_lt40 + s_dist_gt40, data = train.data, family = binomial)
model1b <- glmer(hair_in_bed ~ s_prop_forest_60m + s_dist_lt40 + s_dist_gt40 + (1|den_id), data = train.data, family = binomial)
# Full model - distance category collapsed into a single distance from edge * age term
model2a <- glm(hair_in_bed ~ s_prop_forest_60m + s_dist_from_edge * age, data = train.data, family = binomial)
model2b <- glmer(hair_in_bed ~ s_prop_forest_60m + s_dist_from_edge * age + (1|den_id), data = train.data, family = binomial)

summary(model0a)
summary(model0b)
summary(model0c)
summary(model0d)

summary(model1a) 

# This model has beautiful results and beautiful residuals, but it throws a convergence warning
# https://stackoverflow.com/questions/53034261/warning-lme4-model-failed-to-converge-with-maxgrad
summary(model1b) # 'Model failed to converge with max|grad| = 0.0203386 (tol = 0.002, component 1)'
# this model failed to converge - I believe it's because the random effect of den_id 
# effectively explained most of the variance.

DHARMa::testResiduals(model1b) # beaufitul residuals

# Plot confirms this - all these replicates per den site cause model convergance issues
# Ben Bolker suggests aggregating data by group in the SO question above. 
# In our case, aggregating data by before/after treatment seems to be best bet!
install.packages("ggalt")
ggplot(dat, aes(x = s_prop_forest_60m, y = hair_in_bed, color = den_id)) +
  geom_point() +
  ggalt::geom_encircle(aes(group=den_id)) +
  theme(legend.position="none")

# Continue examaning other models below

summary(model2a)
summary(model2b)

# model0a is our top model here
bbmle::AICtab(model0a, model0b, model0c, model0d,
              model1a, model1b,
              model2a, model2b,
              base = TRUE)

# The models are all suuuper close to each other,
# but none do a particularly good job at predicting
# bear hair presence/absence.

jtools::summ(model1b)
jtools::effect_plot(model1b, pred = s_prop_forest_60m, plot.points = TRUE)
jtools::effect_plot(model1b, pred = s_dist_lt40, plot.points = TRUE)
jtools::effect_plot(model1b, pred = s_dist_gt40, plot.points = TRUE)


# Make predictions
probs <- predict(model0a, test.data, type = "response")
resp <- ifelse(probs > 0.5, 1, 0)

# Model accuracy
sum(dat$hair_in_bed) / nrow(dat) # baseline - about 45% of the data has hair in bed == TRUE
mean(resp == test.data$hair_in_bed) # the model predicts with about 70% accuracy



# LINEAR MODEL - DEN STATUS -----------------------------------------------


# What about den status?

# Subset to data of interest/suitability for ONLY FOR SURE DENS
# Exclude any "Active in last 4 seasons" ones
dat <- f[f$den_status_binary %in% c("Active", "Not active") & f$den_status %in% for_sure_dens, ]
dat <- dat[, c("den_status_binary", "den_id", "year", "f_prop_forest_60m", "f_dist_lt40", "f_dist_gt40")]
dat <- dat[complete.cases(dat),]

# Convert den_status_binary to 0 or 1
dat$den_status_binary <- ifelse(dat$den_status_binary == "Active", 1, 0)

# Next, make just one 'distance from edge' variable
dat$dist_from_edge <- ifelse(dat$f_dist_lt40 > 0, dat$f_dist_lt40, dat$f_dist_gt40)
hist(dat$dist_from_edge)
dat$log_dist_from_edge <- log(dat$dist_from_edge + 1)
hist(dat$log_dist_from_edge)

# Next, say if it's in old, new, or edge of forest.
dat <- dat |> dplyr::mutate(dist_lt40 = round(f_dist_lt40, 0),
                            dist_gt40 = round(f_dist_gt40, 0),
                            age = dplyr::case_when((f_dist_lt40 == f_dist_gt40) ~ "on_edge",
                                                   (f_dist_lt40 > f_dist_gt40) ~ "within_old",
                                                   (f_dist_lt40 < f_dist_gt40) ~ "within_young"))
plyr::count(dat$age)
# ah, going to have to cut out edge ones
ggplot(dat, aes(y = log_dist_from_edge, x = age, color = age)) + geom_boxplot() 
# on_edge is perfectly colinear with a distance of 0

# Confirm the variances are equal between our categorical variables
# ehhh it's borderline
car::leveneTest(den_status_binary ~ age, dat) # null hypothesis: variances are not significantly different
car::leveneTest(den_status_binary ~ age, dat[which(dat$age != "on_edge"), ]) # null hypothesis: variances are not significantly different
# So, because the variances are NOT even between the groups...
# https://stats.stackexchange.com/questions/34325/regression-modelling-with-unequal-variance
# These models are not perfect - they violate the assumption of even variances
# between your groups in your covariates, but they are a starting point.

# Rescale numeric responses
dat$s_dist_from_edge <- scale(dat$dist_from_edge)[,1]
dat$s_prop_forest_60m <- scale(dat$f_prop_forest_60m)[,1]
dat$s_dist_lt40 <- scale(dat$f_dist_lt40)[,1]
dat$s_dist_gt40 <- scale(dat$f_dist_gt40)[,1]
hist(dat$s_dist_from_edge)
hist(dat$s_prop_forest_60m)
hist(dat$s_dist_lt40)
hist(dat$s_dist_gt40)

# Split into test and train
train.samples <- caret::createDataPartition(dat$den_status_binary, p = 0.8, list = FALSE)
train.data <- dat[train.samples,]
test.data <- dat[-train.samples,]

# Fit the models!
# Just the basics - each forest metric - effectively same as examining boxplots for significance
model0a <- glm(den_status_binary ~ s_prop_forest_60m, data = train.data, family = binomial)
model0b <- glm(den_status_binary ~ s_dist_lt40, data = train.data, family = binomial)
model0c <- glm(den_status_binary ~ s_dist_gt40, data = train.data, family = binomial)
model0d <- glm(den_status_binary ~ age, data = train.data, family = binomial)
model0e <- glm(den_status_binary ~ s_dist_from_edge, data = train.data, family = binomial)
model0f <- glm(den_status_binary ~ s_dist_from_edge * age, data = train.data, family = binomial)
# Full model - each distance category
model1a <- glm(den_status_binary ~ s_prop_forest_60m + s_dist_lt40 + s_dist_gt40, data = train.data, family = binomial)
model1b <- glmer(den_status_binary ~ s_prop_forest_60m + s_dist_lt40 + s_dist_gt40 + (1|den_id), data = train.data, family = binomial)
# Full model - distance category collapsed into a single distance from edge * age term
model2a <- glm(den_status_binary ~ s_prop_forest_60m + s_dist_from_edge * age, data = train.data, family = binomial)
model2b <- glmer(den_status_binary ~ s_prop_forest_60m + s_dist_from_edge * age + (1|den_id), data = train.data, family = binomial)

summary(model0a)
summary(model0b)
summary(model0c)
summary(model0d)
summary(model0e)
summary(model0f)

summary(model1a) 
summary(model1b)

# Unlike the hair_in_bed model1b, there's a lot more
# crossover here between den_id and den_status, so you
# don't get that model convergence issue
ggplot(dat, aes(x = s_prop_forest_60m, y = den_status_binary, color = den_id)) +
  geom_point() +
  ggalt::geom_encircle(aes(group=den_id)) +
  theme(legend.position="none")

# Continue examining other models below

summary(model2a)
summary(model2b)

# Again fairly similar models here - no large delta AIC values
bbmle::AICtab(model0a, model0b, model0c, model0d,
              model1a, model1b,
              model2a, model2b,
              base = TRUE)

DHARMa::testResiduals(model0a)
DHARMa::testResiduals(model1b)

broom::augment(model1b) |>
  ggplot(aes(x = s_prop_forest_60m,
             y = .resid,
             color = den_id)) +
  geom_point() +
  theme(legend.position = "none")

broom::augment(model1b) |>
  ggplot(aes(x = s_dist_lt40,
             y = .resid,
             color = den_id)) +
  geom_point() +
  theme(legend.position = "none")

broom::augment(model1b) |>
  ggplot(aes(x = s_dist_gt40,
             y = .resid,
             color = den_id)) +
  geom_point() +
  theme(legend.position = "none")


broom::augment(model2b) |>
  ggplot(aes(x = s_dist_from_edge,
             y = .resid,
             color = age)) +
  geom_point()

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
jtools::summ(model1b)
jtools::effect_plot(model1b, pred = s_prop_forest_60m, plot.points = TRUE)
jtools::effect_plot(model1b, pred = s_dist_lt40, plot.points = TRUE)
jtools::effect_plot(model1b, pred = s_dist_gt40, plot.points = TRUE)

summary(model2b)
jtools::effect_plot(model2b, pred = s_prop_forest_60m, plot.points = TRUE)
jtools::effect_plot(model2b, pred = s_dist_from_edge, plot.points = TRUE)
jtools::effect_plot(model2b, pred = age, plot.points = TRUE)

ggplot(dat, aes(y = den_status_binary, x = log(dist_from_edge), color = age)) +
  geom_point() +
  ggalt::geom_encircle(aes(group = age))


# Make predictions
probs <- predict(model, test.data, type = "response")
resp <- ifelse(probs > 0.5, 1, 0)

# Model accuracy
mean(resp == test.data$den_status_binary) # the model predicts with about 72% accuracy! Slightly better



# % AGE CLASS -------------------------------------------------------------

prct <- tar_read(prct_age_class_1.5km)

# Bin the data into groups (sort of ~young, old, medium, very old)
prct$lt_3 <- rowSums(prct[,c("age_class_1", "age_class_2")])
prct$gt_3 <- rowSums(prct[,6:12])
prct$three_to_7 <- rowSums(prct[,6:10])
prct$gt_8 <- rowSums(prct[,c("age_class_8", "age_class_9")])

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
