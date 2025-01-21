
# TOP ---------------------------------------------------------------------

# Load up data
library(targets)
tar_load(f)
tar_load(forestry_verifications_full)

# Merge new forestry verifications into dataset
f <- merge(f, forestry_verifications_full, by = "sample_id")

# Take a look at windthrow variables
unique(f$x_windthrow_code)

plyr::count(f$x_windthrow_code) |>
  dplyr::mutate(prct = round(freq/nrow(f) * 100, 1))

# Fill NA den status w string
f[["den_status"]][is.na(f$den_status)] <- "NA"

# Make den status a factor
f$den_status <- factor(x = f$den_status,
                       levels = c("Currently active",
                                  "Active in last denning season",
                                  "Active recently (0-4 seasons)",
                                  "Not active in last season, but recent use (1-4 seasons)",
                                  "Not active in last season, no recent use (>4 seasons)",
                                  "No recent evidence of use (>4 seasons)",
                                  "Not active in last season",
                                  "Obsolete",
                                  "Unknown"))

# Bin into binary categories
f <- f |> dplyr::mutate(den_status_binary = dplyr::case_when(grepl("No", den_status) ~ "Not active",
                                                             den_status == "Obsolete" ~ "Obsolete",
                                                             den_status == "Unknown" ~ "Unknown",
                                                             TRUE ~ "Active"))

# Double check our work
unique(f[,c("den_status", "den_status_binary")])

plyr::count(f$den_status)
plyr::count(f$den_status_binary)


# Now do some boxplots

library(ggplot2)
library(ggsignif)


# ALL BINARY VARS ---------------------------------------------------------



# % forested
# All categories

f[f$proportion_forested_field < 999, ] |>
  ggplot(aes(x = den_status, 
             y = proportion_forested_field)) +
  geom_boxplot() +
  geom_jitter() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

f[f$proportion_forested_field < 999, ] |>
  ggplot(aes(x = den_status, 
             y = prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# % forested
# Binary categories

f[which(f$proportion_forested_field < 999), ] |>
  ggplot(aes(x = den_status_binary, 
             y = proportion_forested_field)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (RAW field values)",
       title = "Den Status vs. % Forested (RAW)",
       subtitle = "All categories binned into either 'Active' or 'Not active'")

#f[which(f$proportion_forested_field < 999), ] |>
f |>
  ggplot(aes(x = den_status_binary, 
             y = prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (VERIFIED)",
       title = "Den Status vs. % Forested (GIS VERIFIED)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 


# dist <40

f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1), ] |>
  ggplot(aes(x = den_status_binary, 
             y = distance_less40yr_forest_field)) +
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

# This is disgusting but please excuse
# psych::describeBy(f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1), ]$distance_less40yr_forest_field, 
#                   f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1), ]$den_status_binary, 
#                   mat = TRUE)
# 
# f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1), ] |>
#   ggplot(aes(x = distance_less40yr_forest_field,
#              color = den_status_binary,
#              fill = den_status_binary)) +
#   geom_density(alpha = 0.1) +
#   scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                      name = "Den Status") +
#   scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7"),
#                     name = "Den Status") +
#   labs(x = "Distance to <40 yo forest (RAW)",
#        y = "Density") +
#   theme_minimal()


#f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1), ] |>
f |>
  ggplot(aes(x = den_status_binary, 
             y = dist_lt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to <40 (VERIFIED)",
       title = "Den Status vs. Distance to <40 yo forest (GIS VERIFIED)",
       subtitle = "All categories binned into either 'Active' or 'Not active'") 



# dist >40

#f[which(f$distance_less40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1), ] |>
f[which(f$distance_less40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > 0), ] |>
  ggplot(aes(x = den_status_binary, 
             y = distance_grtr40yr_forest_field)) +
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


#f[which(f$distance_less40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1), ] |>
#f[which(f$dist_gt40 > 0),] |>
f |>
  ggplot(aes(x = den_status_binary, 
             y = dist_gt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to >40 (VERIFIED)",
       title = "Den Status vs. Distance to >40 yo forest (GIS VERIFIED)",
       subtitle = "All categories binned into either 'Active' or 'Not active'")



# ONLY FOR SURE ACTIVE/NON ACTIVE -----------------------------------------


for_sure_dens <- c("Active in last denning season", 
                   "Currently active", 
                   "Not active in last season")

# % forested

f[which(f$proportion_forested_field < 999 & f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = proportion_forested_field)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (RAW field values)",
       title = "Den Status vs. % Forested (RAW)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")

#f[which(f$proportion_forested_field < 999 & f$den_status %in% for_sure_dens), ] |>
f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Den Status",
       y = "Proportion Forested (VERIFIED)",
       title = "Den Status vs. % Forested (GIS VERIFIED)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season")


# dist <40

f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1 & f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = distance_less40yr_forest_field)) +
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


# f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1 & f$den_status %in% for_sure_dens), ] |>
f[which(f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = dist_lt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to <40 (VERIFIED)",
       title = "Den Status vs. Distance to <40 yo forest (GIS VERIFIED)",
       subtitle = "ONLY confirmed 'Active' or 'Not active' in the current season") 


# dist >40

f[which(f$distance_less40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1 & f$den_status %in% for_sure_dens), ] |>
#f[which(f$distance_less40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > 0 & f$den_status %in% for_sure_dens), ] |>
  ggplot(aes(x = den_status_binary, 
             y = distance_grtr40yr_forest_field)) +
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


#f[which(f$distance_less40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1 & f$den_status %in% for_sure_dens), ] |>
f[which(f$den_status %in% for_sure_dens), ] |>
#f[which(f$den_status %in% for_sure_dens & f$dist_gt40 > 0), ] |>
  ggplot(aes(x = den_status_binary, 
             y = dist_gt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Active", "Not active")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Den Status",
       y = "log Distance to >40 (VERIFIED)",
       title = "Den Status vs. Distance to >40 yo forest (GIS VERIFIED)",
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

f[which(f$proportion_forested_field < 999 & f$hair_in_bed %in% c("Yes", "No")),] |>
  ggplot(aes(x = hair_in_bed, 
             y = proportion_forested_field)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")),
              map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Proportion Forested (RAW field values)",
       title = "Hair Presence vs. % Forested (RAW)")

#f[which(f$proportion_forested_field < 999 & f$hair_in_bed %in% c("Yes", "No")),] |>
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = prop_forest_60m)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  labs(x = "Hair in Bed",
       y = "Proportion Forested (VERIFIED)",
       title = "Hair Presence vs. % Forested (GIS VERIFIED)")


# DENSITY PLOTS

f[which(f$proportion_forested_field < 999 & f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = proportion_forested_field,
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

#f[which(f$proportion_forested_field < 999 & f$hair_in_bed %in% c("Yes", "No")), ] |>
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = prop_forest_60m,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#CC79A7"),
                     name = "Hair in Bed") +
  scale_fill_manual(values = c("#E69F00", "#CC79A7"),
                    name = "Hair in Bed") +
  labs(x = "Proportion Forested (VERIFIED)",
       y = "Density") +
  theme_minimal()


# dist <40

# BOXPLOTS

f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = distance_less40yr_forest_field)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to <40 (RAW)",
       title = "Hair Presence vs. Distance to <40 yo forest (RAW field values)") 


#f[which(f$distance_less40yr_forest_field < 999 & f$distance_less40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = dist_lt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to <40 (VERIFIED)",
       title = "Hair Presence vs. Distance to <40 yo forest (GIS VERIFIED)")


# DENSITY PLOTS

f[which(f$proportion_forested_field < 999 & f$distance_less40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = distance_less40yr_forest_field,
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

#f[which(f$proportion_forested_field < 999 & f$distance_less40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = dist_lt40,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#CC79A7"),
                     name = "Hair in Bed") +
  scale_fill_manual(values = c("#E69F00", "#CC79A7"),
                    name = "Hair in Bed") +
  labs(x = "Distance to <40 (VERIFIED)",
       y = "Density") +
  theme_minimal()


# dist >40

# BOXPLOTS

f[which(f$distance_grtr40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = distance_grtr40yr_forest_field)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to >40 (RAW)",
       title = "Hair Presence vs. Distance to >40 yo forest (RAW field values)") 


#f[which(f$distance_grtr40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = hair_in_bed, 
             y = dist_gt40)) +
  geom_boxplot() +
  geom_jitter() +
  geom_signif(comparisons = list(c("Yes", "No")), map_signif_level = TRUE) +
  theme_minimal() +
  scale_y_continuous(trans = 'log10') +
  annotation_logticks() +
  labs(x = "Hair in Bed",
       y = "log Distance to >40 (VERIFIED)",
       title = "Hair Presence vs. Distance to >40 yo forest (GIS VERIFIED)")


# DENSITY PLOTS

f[which(f$distance_grtr40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = distance_grtr40yr_forest_field,
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

#f[which(f$distance_grtr40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1 & f$hair_in_bed %in% c("Yes", "No")), ] |>
f[which(f$hair_in_bed %in% c("Yes", "No")), ] |>
  ggplot(aes(x = dist_gt40,
             color = hair_in_bed,
             fill = hair_in_bed)) +
  geom_density(alpha = 0.1) +
  scale_color_manual(values = c("#E69F00", "#CC79A7"),
                     name = "Hair in Bed") +
  scale_fill_manual(values = c("#E69F00", "#CC79A7"),
                    name = "Hair in Bed") +
  labs(x = "Distance to >40 (VERIFIED)",
       y = "Density") +
  theme_minimal()

# Run summary stats of the dist_gt40 col by hair_in_bed categories ("Yes",
# "No", "Yes - Unchanged", "Unknown")
psych::describeBy(f[which(f$distance_grtr40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1), ]$dist_gt40, 
                  f[which(f$distance_grtr40yr_forest_field < 999 & f$distance_grtr40yr_forest_field > -1), ]$hair_in_bed, 
                  mat = TRUE)



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



# LINEAR MODEL ------------------------------------------------------------

# Hair in Bed

# Subset to data of interest/suitability
dat <- f[f$hair_in_bed %in% c("Yes", "No"), ]
dat <- dat[, c("hair_in_bed", "prop_forest_60m", "dist_lt40", "dist_gt40")]
dat <- dat[complete.cases(dat),]

# Convert hair_in_bed to 0 or 1
dat$hair_in_bed <- ifelse(dat$hair_in_bed == "Yes", 1, 0)

# Rescale numeric responses
dat$prop_forest_60m <- scale(dat$prop_forest_60m)[,1]
dat$dist_lt40 <- scale(dat$dist_lt40)[,1]
dat$dist_gt40 <- scale(dat$dist_gt40)[,1]

# Split into test and train
train.samples <- caret::createDataPartition(dat$hair_in_bed, p = 0.8, list = FALSE)
train.data <- dat[train.samples,]
test.data <- dat[-train.samples,]

# Fit the model!
model <- glm(hair_in_bed ~ ., data = train.data, family = binomial)
summary(model) # base R
jtools::summ(model)

jtools::effect_plot(model, pred = prop_forest_60m, plot.points = TRUE)
jtools::effect_plot(model, pred = dist_lt40, plot.points = TRUE)
jtools::effect_plot(model, pred = dist_gt40, plot.points = TRUE)

# Make predictions
probs <- predict(model, test.data, type = "response")
resp <- ifelse(probs > 0.5, 1, 0)

# Model accuracy
mean(resp == test.data$hair_in_bed) # the model predicts with about 50% accuracy! garbage



# What about den status?


# Subset to data of interest/suitability
dat <- f[f$den_status_binary %in% c("Active", "Not active"), ]
dat <- dat[, c("den_status_binary", "prop_forest_60m", "dist_lt40", "dist_gt40")]
dat <- dat[complete.cases(dat),]

# Convert den_status_binary to 0 or 1
dat$den_status_binary <- ifelse(dat$den_status_binary == "Active", 1, 0)

# Rescale numeric responses
dat$prop_forest_60m <- scale(dat$prop_forest_60m)[,1]
dat$dist_lt40 <- scale(dat$dist_lt40)[,1]
dat$dist_gt40 <- scale(dat$dist_gt40)[,1]

# Split into test and train
train.samples <- caret::createDataPartition(dat$den_status_binary, p = 0.8, list = FALSE)
train.data <- dat[train.samples,]
test.data <- dat[-train.samples,]

# Fit the model!
model <- glm(den_status_binary ~ ., data = train.data, family = binomial)
summary(model) # base R
jtools::summ(model) 

# A significant intercept tells us that if you set all the 
# variables to 0, the response mean is significantly different 
# from 0. It doesn't tell us much other than "if proportion forest, 
# distance <40 and distance >40 all equal zero, den_status_binary 
# is different than if the variables are all >0." At most, it can 
# tell you with confidence that your variables have some influence
# on the response, but not much more than that.

jtools::effect_plot(model, pred = prop_forest_60m, plot.points = TRUE)
jtools::effect_plot(model, pred = dist_lt40, plot.points = TRUE)
jtools::effect_plot(model, pred = dist_gt40, plot.points = TRUE)


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
