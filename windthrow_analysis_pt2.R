# Windthrow Analysis Pt 2

# Linking windthrow impacts to den use

# 01 SETUP ----------------------------------------------------------------

library(targets) # load data
library(ggplot2) # plotting
library(ggsignif) # plotting significance (t-test + wilcox test)
library(ggmosaic) # mosaic plot
library(fitdistrplus) # exploratory tool to find data distributions
library(gamlss) # modeling non-linear relationships + distributions
library(glmmTMB) # modeling non-linear relationships + distributions
library(MASS) # modeling ordered logistic regression
library(brant) # brant statistical test
library(effects) # extract model effect sizes
library(khroma) # color themes

# For this analysis, we need to use the `f_analysis` dataframe,
# because this links the previous years' forestry conditions (like windthrow) 
# to the current years' den status. This ensures we're modeling the cause-effect
# relationship correctly. 
tar_load(f_analysis) # field visit data, rearranged such that forestry conditions from the previous year are matched to den status of current year
tar_load(dens) # static den data

# Rename forestry columns to more consistent/easier to use names
f_analysis <- dplyr::rename(f_analysis,
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

# Merge static den data to field visits data
f_analysis <- merge(f_analysis, dens, by = "den_id")

rm(dens)

## 01-1 Set factors ----

f_analysis$windthrow_code <- factor(f_analysis$windthrow_code, 
                           levels = c("None", 
                                      "At risk", 
                                      "Windthrow adjacent (>5%)",
                                      "Windthrow adjacent - catastrophic (>25%)",
                                      "Windthrow block access",
                                      "Den tree top blown",
                                      "Den tree blown over"))

f_analysis$patch <- factor(f_analysis$forestry_treatment_desc,
                  levels = c("Contiguous Forest within >60m radius",
                             "Large retention patch (30-60m radius or 2830-11300 m2)",
                             "Medium retention patch (10-30m radius or 314-2830m2)",
                             "Edge of block (within 5m)",
                             "Other"),
                  labels = c("Contiguous",
                             "Large RP",
                             "Med. RP",
                             "Edge (<5 m)",
                             "Other"))

## 01-2 Create 'harvested' column ----

# Per the explanatory comments in Gavin's 
# "_03_Bear Den Windthrow - Data filtering and analysis prep_Gavin_Newall.xlsx"
# document, the "Harvested" column was calculated to reflect
# any dens where a cutblock was within 60m of the den. To re-create
# this column with our updated data, we can use a combination of the 
# "% forested within 60m" and "distance to <40 yo forest" columns. If 
# (% forested within 60m) < 100% AND ((distance to <40 yo forest <= 60) OR (distance to road <= 60)), 
# we can assume there was harvest activity near the den. 
# The reason we don't exclusively use the "% forested" column on it's own
# would be to account for the few cases where a den might be near a 
# natural forest edge, e.g. near a lake or the ocean, or natural windthrow
# not related to harvest.

# Harvested
f_analysis[which(f_analysis$v_prop_forest_60m < 100 & (f_analysis$v_dist_lt40 <= 60 | f_analysis$v_dist_road <= 60)), ]

# Not harvested
f_analysis[which(f_analysis$v_prop_forest_60m == 100), ]
f_analysis[which(f_analysis$v_prop_forest_60m < 100 & (f_analysis$v_dist_lt40 > 60 & f_analysis$v_dist_road > 60)), ]

# Now turn it into a column of data
f_analysis$harvested <- ifelse((f_analysis$v_prop_forest_60m < 100 & (f_analysis$v_dist_lt40 <= 60 | f_analysis$v_dist_road <= 60)),
                      "Harvested",
                      "Not harvested")
f_analysis$harvested_yn <- ifelse(f_analysis$harvested == "Harvested",
                         TRUE,
                         FALSE)

plyr::count(f_analysis$harvested)

## 01-3 Categorize windthrow severity ----

f_analysis <- f_analysis |>
  dplyr::mutate(windthrow_sev = dplyr::case_when(windthrow_code == "None" ~ "Insignificant",
                                                 windthrow_code == "At risk" ~ "At risk",
                                                 windthrow_code == "Windthrow adjacent (>5%)" ~ "Moderate",
                                                 windthrow_code %in% c("Windthrow adjacent - catastrophic (>25%)",
                                                                       "Windthrow block access",
                                                                       "Den tree top blown",
                                                                       "Den tree blown over") ~ "Severe",
                                                 TRUE ~ NA))

f_analysis$windthrow_sev <- factor(f_analysis$windthrow_sev,
                          levels = c("Insignificant",
                                     "At risk",
                                     "Moderate",
                                     "Severe"))

## 01-4 Distance from edge ----

# The previous analysis lumped together distance from edge
# to <40 yo forest and distance to road. They were both
# considered 'disturbed' forest edges susceptible to windthrow.

# Choose whichever one is the lower value
f_analysis$dist_to_edge <- ifelse(f_analysis$f_dist_lt40 < f_analysis$f_dist_road,
                         f_analysis$f_dist_lt40,
                         f_analysis$f_dist_road)


## 01-5 Den status binary ----

# Add a `den_status_binary` column
f_analysis <- f_analysis |> dplyr::mutate(den_status_binary = dplyr::case_when(grepl("No", den_status) ~ "Not Active",
                                                                               den_status %in% c("Active in last denning season", "Currently active") ~ "Active",
                                                                               den_status == "Obsolete" ~ "Obsolete",
                                                                               den_status == "Unknown" ~ "Unknown",
                                                                               TRUE ~ "Unknown"))

# If "Active recently (0-4 seasons)" is the very first data point, 
# change it to "Active". 
f_analysis <- f_analysis |>
  dplyr::arrange(den_id, date_inspected_den) |>
  dplyr::group_by(den_id) |>
  dplyr::mutate(cumulative_visit = cumsum(!is.na(den_id))) |>
  dplyr::mutate(den_status_binary = ifelse(cumulative_visit == 1 & den_status == "Active recently (0-4 seasons)",
                                           "Active",
                                           den_status_binary))

## 01-6 Create `dat` analysis df ----

# Select cols
dat <- f_analysis[ ,c("den_id", "windthrow_prct", "windthrow_sev", "den_status_binary")]

# Remove dens where den status is Unknown
dat <- dat[which(dat$den_status_binary != "Unknown"), ]

# Remove dens where windthrow severity is "At risk"
dat <- dat[which(dat$windthrow_sev != "At risk"), ]

# Remove NAs
dat <- na.omit(dat)

# Remove leftover dplyr fluff
dat <- as.data.frame(dat)

# 02 WINDTHROW SEVERITY x DEN STATUS --------------------------------------

# We remove the "At risk" category because it's not an
# objective outcome, but rather a prediction
# that the den will potentially experience windthrow 
# impacts in the future.

## 02-1 Chi Square ----

dat |>
  dplyr::select(den_id, windthrow_sev, den_status_binary) |>
  dplyr::select(-den_id) |> 
  na.omit() |>
  dplyr::group_by(windthrow_sev, den_status_binary) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = den_status_binary,
                     values_from = n) |>
  tibble::column_to_rownames(var = "windthrow_sev") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Windthrow Severity x Den Status")

chisq <- dat |>
  dplyr::select(den_id, windthrow_sev, den_status_binary) |>
  dplyr::select(-den_id) |>
  na.omit() |>
  dplyr::group_by(windthrow_sev, den_status_binary) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = windthrow_sev,
                     values_from = n) |>
  tibble::column_to_rownames(var = "den_status_binary") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test(simulate.p.value = TRUE) # to handle smaller groups; ie Fisher exact test

chisq
chisq$observed
round(chisq$expected)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)

## 02-2 Cramer's V ----

# Calculate Cramer's V
# Total sample size
n <- sum(chisq$observed)
# Phi coefficient (sqrt of chi-square statistic divided by sample size)
phi <- sqrt(chisq$statistic / n) 
# Cramer's V
cramers_v <- phi / sqrt(min(dim(chisq$observed)) - 1)  

# Print Cramer's V
cramers_v

## 02-3 Contigency table ----
# the last step is to generate confidence intervals for the true probabilities.
# We will need to do this one variable at a time and use bonferroni correction 
# because we are running 3 tests

# generating the contingency table
cont.table <- 
  dat |>
  dplyr::select(den_id, windthrow_sev, den_status_binary) |>
  dplyr::select(-den_id) |> 
  droplevels() |>
  na.omit() |>
  table() |>
  t()
# <- table(f$harvested, f$windthrow_sev)
ncol <- ncol(cont.table)
nrow <- nrow(cont.table)
num.entries <- nrow*ncol

# setting the bonferroni adjusted confidence level
alpha <- 1 - (0.05 / num.entries)

# looping through each categoty and calculating the confidence interval
for(i in 1:num.entries){
  col.i <- (i-1) %% ncol +1
  row.i <- (i-1) %/% ncol +1
  
  # creating a new contingency table for 1 ourcome at a time. It will have 
  # the number of times that outcome did vs didn't occur for either harvested
  # or not harvested.
  new.cont.table <- matrix(NA, nrow = 1, ncol = 2)
  colnames(new.cont.table) <- 
    c(colnames(cont.table)[col.i], paste("Not", colnames(cont.table)[col.i]))
  rownames(new.cont.table) <- rownames(cont.table)[row.i]
  
  # Putting the new values into the contingency table.
  new.cont.table[1] <- cont.table[row.i,col.i]
  
  # PREVIOUSLY: Gavin was calculating the confidence intervals
  # for each harvest status separately (Harvested vs Not harvested),
  # such that the odds added up too 100% for each harvest status 
  # (and thus added up to 200% total for the whole table).
  #new.cont.table[2] <- sum(cont.table[row.i,-col.i])
  
  # NOW: calculate the odds for each category relative to ANY AND ALL
  # other categories in the table. 
  new.cont.table[2] <- sum(cont.table) - cont.table[row.i, col.i]
  
  # calculating the confidence interval
  result <- prop.test(new.cont.table, conf.level = alpha, correct = FALSE)
  
  # printing the results
  print(colnames(cont.table)[col.i])
  print(rownames(cont.table)[row.i])
  print(result)
}


## 02-4 Mosaic plot ----

# Create a mosaic plot with color that shows the distribution of the windthrow
# categories vs harvest status

# OBSERVED
m1 <-
  dat |> 
  dplyr::select(den_id, windthrow_sev, den_status_binary) |>
  dplyr::select(-den_id) |> 
  droplevels() |>
  na.omit() |>
  ggplot() +
  geom_mosaic(aes(x = product(den_status_binary), 
                  fill = windthrow_sev), 
              alpha = 1) + 
  scale_fill_okabeito(black_position = "last") +
  labs(title = "Observed",
       y = "Windthrow Severity", 
       x = "Den Status") + 
  theme_mosaic() +
  theme(legend.position = "none", 
        axis.title = element_blank()
        #axis.title.x = element_text(size = 14, vjust = -0.2), 
        #axis.title.y = element_text(size = 14)
        )

# EXPECTED
m2 <- 
  round(chisq$expected) |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  tidyr::pivot_longer(cols = c(2:4)) |> 
  tidyr::uncount(value) |>
  ggplot() +
  geom_mosaic(aes(x = product(rowname), 
                  fill = name),
              alpha = 0.8) +
  scale_fill_okabeito(black_position = "last") +
  labs(title = "Expected",
       x = "Harvest Status") +
  theme_mosaic() +
  theme(legend.position = "none",
        axis.title = element_blank()
        #axis.title.x = element_text(size = 14, vjust = -0.2), 
        #axis.title.y = element_text(size = 14)
  )

ggpubr::ggarrange(m1, m2, nrow = 1, labels = c("A", "B"))


# Clean up
rm(chisq, result, alpha, col.i, cont.table, cramers_v, i,
   n, ncol, nrow, num.entries, phi, row.i)
