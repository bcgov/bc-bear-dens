# Authorship:
# Much of this analysis was originally written by Gavin Newall
# in 2023. This code has been cleaned up where appropriate
# and modified to fit in with the targets workflow. 
# The original script can be found at:
# L:\Environmental Stewardship\Black Bear\Data Analysis\Analyses\Windthrow analysis\GN_FinalPackage\Bear_Dens_Analysis_GN_Final.R

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
#library(car) # 
library(splines)

tar_load(f_analysis) # field visit data rearranged such that previous year's forestry data matches current year's den status
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
f <- merge(f_analysis, dens, by = "den_id")

rm(f_analysis, dens)


## 01-1 Set factors ----

f$windthrow_code <- factor(f$windthrow_code, 
                           levels = c("None", 
                                      "At risk", 
                                      "Windthrow adjacent (>5%)",
                                      "Windthrow adjacent - catastrophic (>25%)",
                                      "Windthrow block access",
                                      "Den tree top blown",
                                      "Den tree blown over"))

f$patch <- factor(f$forestry_treatment_desc,
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
f[which(f$f_prop_forest_60m < 100 & (f$f_dist_lt40 <= 60 | f$f_dist_road <= 60)), ]

# Not harvested
f[which(f$f_prop_forest_60m == 100), ]
f[which(f$f_prop_forest_60m < 100 & (f$f_dist_lt40 > 60 & f$f_dist_road > 60)), ]

# Now turn it into a column of data
f$harvested <- ifelse((f$f_prop_forest_60m < 100 & (f$f_dist_lt40 <= 60 | f$f_dist_road <= 60)),
                      "Harvested",
                      "Not harvested")
f$harvested_yn <- ifelse(f$harvested == "Harvested",
                         TRUE,
                         FALSE)


## 01-3 Categorize windthrow severity ----

f <- f |>
  dplyr::mutate(windthrow_sev = dplyr::case_when(windthrow_code == "None" ~ "Insignificant",
                                                 windthrow_code == "At risk" ~ "At risk",
                                                 windthrow_code == "Windthrow adjacent (>5%)" ~ "Moderate",
                                                 windthrow_code %in% c("Windthrow adjacent - catastrophic (>25%)",
                                                                       "Windthrow block access",
                                                                       "Den tree top blown",
                                                                       "Den tree blown over") ~ "Severe",
                                                 TRUE ~ NA))

f$windthrow_sev <- factor(f$windthrow_sev,
                          levels = c("Insignificant",
                                     "At risk",
                                     "Moderate",
                                     "Severe"))

## 01-4 Distance from edge ----

# The previous analysis lumped together distance from edge
# to <40 yo forest and distance to road. They were both
# considered 'disturbed' forest edges susceptible to windthrow.

# Choose whichever one is the lower value
f$dist_to_edge <- ifelse(f$f_dist_lt40 < f$f_dist_road,
                         f$f_dist_lt40,
                         f$f_dist_road)


# 02 EXPLORATORY STATS ----------------------------------------------------

## 02-1 Windthrow code ----
plyr::count(f$windthrow_code)

ggplot(f,
       aes(x = windthrow_code)) +
  geom_bar() +
  theme_minimal()

## 02-2 Windthrow percent ----
summary(f$windthrow_prct)

hist(f$windthrow_prct)

ggplot(f,
       aes(x = windthrow_prct)) +
  geom_density() +
  theme_minimal()

## 02-3 Retention patch size ----
plyr::count(f$forestry_treatment_desc)
plyr::count(f$patch)

ggplot(f,
       aes(x = patch)) +
  geom_bar() + 
  theme_minimal()

f[!is.na(f$patch), ] |>
  ggplot(aes(x = patch,
             y = windthrow_prct)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  theme_minimal()

## 02-4 Harvest status ----
plyr::count(f$harvested)

ggplot(f,
       aes(x = harvested)) +
  geom_bar() +
  theme_minimal()

f[!is.na(f$harvested), ] |>
  ggplot(aes(x = harvested,
           y = windthrow_prct)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  geom_signif(comparisons = list(c("Harvested", "Not harvested")),
              map_signif_level = TRUE,
              test = "wilcox.test") +
  theme_minimal()


## 02-5 Distance from edge ----
# Again, this assumes that distance to <40 yo forest OR
# distance to road counts as the relevant distance to 
# 'disturbed' forest edge. 
hist(f$dist_to_edge)

ggplot(f,
       aes(x = dist_to_edge)) +
  geom_density() +
  theme_minimal()

ggplot(f[which(f$dist_to_edge < 6000), ],
       aes(x = dist_to_edge,
           y = windthrow_prct)) +
  #scale_x_log10() +
  geom_point() + 
  labs(title = "Distance to Edge vs Windthrow %") +
  theme_minimal()

ggplot(f,
       aes(x = dist_to_edge,
           y = windthrow_prct,
           color = patch)) +
  #scale_x_log10() +
  geom_point() + 
  theme_minimal()


## 02-6 Structural stage ----

f[!is.na(f$struct_stage), ] |>
  ggplot(aes(x = struct_stage,
             y = windthrow_prct)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  theme_minimal()

## 02-7 Age class ----

f[!is.na(f$age_class), ] |>
  ggplot(aes(x = age_class,
             y = windthrow_prct)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  theme_minimal()

f |>
  dplyr::filter(!is.na(age_class)) |>
  dplyr::mutate(age_class = ifelse(age_class == "9: >250",
                                   ">9 (Old growth)",
                                   "<9 (all other stages)")) |>
  ggplot(aes(x = age_class,
             y = windthrow_prct)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  geom_signif(comparisons = list(c(">9 (Old growth)", "<9 (all other stages)")),
              map_signif_level = TRUE,
              test = "wilcox.test") +
  theme_minimal()


## 02-8 Canopy closure ----

# Looks like a relationship here!
ggplot(f[which(f$canopy_closure <= 100), ],
       aes(x = canopy_closure,
           y = windthrow_prct)) +
  geom_point() +
  stat_smooth(method = "lm") +
  # scale_x_log10() +
  # scale_y_log10() +
  # annotation_logticks() +
  theme_minimal()

# Linear model
summary(lm(windthrow_prct ~ canopy_closure,
           data = f[which(f$canopy_closure <= 100), ]))

# Log-log model
summary(lm(log(windthrow_prct + 1) ~ log(canopy_closure + 1),
           data = f[which(f$canopy_closure <= 100), ]))


# 03 % WINDTHROW x DIST TO EDGE -----------------------------------------


# As we can see from this plot, there IS a relationship between 
# distance to edge and % windthrow, but it is decidedly non-linear:

plot(f$dist_to_edge, f$windthrow_prct)

# It looks like it could be an inverse relationship, e.g. y ~ 1/x 
# (or similar). Fitting a model to this will be tricky because
# we also know that the response variable, `windthrow_prct`, violates
# assumptions of normally distributed data:

ggplot(f,
       aes(x = windthrow_prct)) +
  geom_density() +
  theme_minimal()

# So, it's clearly not normally distributed data. What distribution IS 
# it then? Let's figure that out with help from the {fitdistrplus} package.

## 03-1 Find the response var distribution ----

# First for ease of use let's isolate windthrow prct (and throw out NA
# windthrow prcts while we are at it).
wp <- f$windthrow_prct[!is.na(f$windthrow_prct)]


# See where our % falls within a set of common data distributions
descdist(wp, boot = 1000) # looks like our w% is beta distributed data!

# Looks like our w% is beta distributed data! However, beta distributed
# data must fall from 0 to 1, rather than 0 to 100. Let's transform it.
wp <- wp / 100
fb <- fitdist(wp, "beta") # looks like it's beta distribution where alpha ~= 0.55, and beta ~=3
summary(fb)

# Just to be sure, let's compare our actual windthrow data versus
# randomly generated data that follows a beta distribution with 
# alpha = 0.55 and beta = 3.
r <- rbeta(n = 1000, shape1 = 0.55, shape2 = 3)

# Hmm..... not quite right
data.frame(value = wp,
           source = "real data") |>
  dplyr::bind_rows(data.frame(value = r,
                              source = "simulated data")) |>
  ggplot(aes(x = value,
             color = source)) +
  geom_density() +
  theme_minimal()

# Let's try gamma and weibull distributions next, just to be sure. 
# For Weibull, we need to help it along first - give it a starting 
# point to start fitting the distribution from. From looking at plots
# of the Weibull distribution on Wikipedia, versus looking at the plot
# of our own data, it looks like our data follows a Weibull form 
# closest to lambda ~= 1 and k ~= 0.5. So let's use that as our starting
# parameters.
fg <- fitdist(wp, "gamma")
fw <- fitdist(wp, "weibull", start = list(scale = 1, shape = 0.5))
fnb <- fitdist(wp, "nbinom")

AIC(fb)
AIC(fg)
AIC(fw) # best AIC by far
AIC(fnb)

plot.legend <- c("beta", "gamma", "Weibull", "NB")
#denscomp(list(fb, fg, fw, fnb), legendtext = plot.legend, ylim = c(0, 0.5)) # ylim error for some reason?
qqcomp(list(fb, fg, fw, fnb), legendtext = plot.legend)
cdfcomp(list(fb, fg, fw, fnb), legendtext = plot.legend)
ppcomp(list(fb, fg, fw, fnb), legendtext = plot.legend)

plot(fw) # Weibull is not perfect, but it's the best we've got.
summary(fw)

# Again, same as we did for the beta distribution, let's compare
# simulated data to the real data to see how well the distribution
# fits. Similar to the above plots (the `denscomp` one that fails)
w <- rweibull(n = 1000, scale = fw$estimate[[1]], shape = fw$estimate[[2]])

# Not perfect, but the best we've got. The real data is
# zero-inflated + likely underestimating cases where windfall
# is in the 1-10% range, as that's hard to eyeball effectively.
data.frame(value = wp,
           source = "real data") |>
  dplyr::bind_rows(data.frame(value = r,
                              source = "simulated beta")) |>
  dplyr::bind_rows(data.frame(value = w,
                              source = "simulated weibull")) |>
  ggplot(aes(x = value,
             color = source)) +
  geom_density() +
  theme_minimal()

# Clean up
rm(fb, fg, fnb, plot.legend, r, w, wp)

## 03-2 Model windthrow ~ dist to edge ----

# Now that we know the distribution our data is likely following (Weibull),
# we'll actually model the relationship between our various variables of
# interest and % Windthrow.

# The GAMLSS package supports modeling data with the Weibull distribution,
# so that's what we'll use.

dat <- f[,c("windthrow_prct", "dist_to_edge")]
dat <- dat[dat$dist_to_edge < 6000, ] # remove this outlier
dat$dist_to_edge <- log(dat$dist_to_edge + 1)
dat <- na.omit(dat)
dat$windthrow_prct <- dat$windthrow_prct / 100

# Remove all the duplicates
dat <- unique(dat)

# Our basic linear model to compare it to
m0 <- lm(formula = windthrow_prct ~ dist_to_edge,
         data = dat)

# Then fit to each of the three Weibull fitting options in the package

# GAMLSS is strict about the Weibull distribution not containing any zeroes...
# add +0.1 to any values that are 0
w1 <- gamlss(formula = (windthrow_prct + 0.1) ~ dist_to_edge, 
             data = dat,
             family = WEI,
             mu.start = fw$estimate[[1]],
             #mu.fix = TRUE,
             sigma.start = fw$estimate[[2]],
             #sigma.fix = TRUE
             )

w2 <- gamlss(formula = (windthrow_prct + 0.1) ~ dist_to_edge, 
             data = dat,
             family = WEI2,
             mu.start = fw$estimate[[1]],
             sigma.start = fw$estimate[[2]])

w3 <- gamlss(formula = (windthrow_prct + 0.1) ~ dist_to_edge, 
             data = dat,
             family = WEI3,
             mu.start = fw$estimate[[1]],
             sigma.start = fw$estimate[[2]])

nb1 <- glmmTMB(formula = (windthrow_prct * 100) ~ dist_to_edge,
               data = dat,
               family = nbinom1,
               ziformula = ~1)

tw <- glmmTMB(formula = (windthrow_prct * 100) ~ dist_to_edge,
               data = dat,
               family = tweedie)

bbmle::AICtab(m0, w1, w2, w3, nb1, tw, base = TRUE)

plot(m0, ask = FALSE) # ask = FALSE to just show all 4 plots in one go instead of having to press Enter to display next plot
plot(w1)
plot(w2) # just totally wonky
plot(w3)

sjPlot::plot_model(nb1, type = "pred", transform = "exp")
sjPlot::plot_model(tw, type = "pred", transform = "exp")

# It's not great, but it's a lot better than the linear model fit.
# Let's compare the predictions of the model to the actual values.
linear_preds <- predict(m0, data = dat, type = "response")
weibull1_preds <- predict(w1, data = dat, type = "response")
weibull2_preds <- predict(w2, data = dat, type = "response") # just totally wonky
weibull3_preds <- predict(w3, data = dat, type = "response")
nb_preds <- predict(nb1, dat, type = "response") / 100 # get it back on 0-1 scale
tw_preds <- predict(tw, dat, type = "response") / 100 # get it back on 0-1 scale

# Now let's plot up real values with all the predicted values from 
# each model:
dat |>
  dplyr::mutate(source = "real data") |>
  dplyr::bind_rows(data.frame(windthrow_prct = linear_preds,
                              dist_to_edge = dat$dist_to_edge,
                              source = "linear model predictions")) |>
  dplyr::bind_rows(data.frame(windthrow_prct = weibull1_preds,
                              dist_to_edge = dat$dist_to_edge,
                              source = "Weibull1 model predictions")) |>
  # dplyr::bind_rows(data.frame(windthrow_prct = weibull2_preds,
  #                             dist_to_edge = dat$dist_to_edge,
  #                             source = "Weibull2 model predictions")) |>
  dplyr::bind_rows(data.frame(windthrow_prct = weibull3_preds,
                              dist_to_edge = dat$dist_to_edge,
                              source = "Weibull3 model predictions")) |>
  dplyr::bind_rows(data.frame(windthrow_prct = nb_preds,
                              dist_to_edge = dat$dist_to_edge,
                              source = "Negative Binomial model predictions")) |>
  dplyr::bind_rows(data.frame(windthrow_prct = tw_preds,
                              dist_to_edge = dat$dist_to_edge,
                              source = "Tweedie model predictions")) |>
  dplyr::mutate(dist_to_edge = exp(dist_to_edge)) |> # back-transform the log scale
  ggplot(aes(x = dist_to_edge,
             y = windthrow_prct,
             color = source,
             group = source)) +
  geom_point() +
  theme_minimal()


# Well, they're all terrible, but notably for every single one the 
# relationship is significant. There's a significantly negative relationship
# between distance to edge and % windthrow.
summary(m0) # base linear model (AIC -263)
summary(w1) # best fit - Weibull 1 (AIC -573)
summary(tw) # tweedie (AIC 2153; though note data were transformed so AIC not comparable)

# Remove clutter
rm(fw, nb1, tw, w2, w3, linear_preds, nb_preds, tw_preds, weibull1_preds, 
   weibull2_preds, weibull3_preds)

## 03-3 Permutation test ----
# Extra bonus: Permutation Test!
# Let's say you took random combinations of X (dist_to_edge) and Y (windthrow_prct)
# from the dataset and fit a linear model to them. Would it be just as good of
# a fit as our original model, m0?

# First get the observed Pearson correlation coefficient
r.obs <- cor(dat$dist_to_edge, 
             dat$windthrow_prct) 

# Grab the observed slope from the linear model
slope.obs <- m0$coefficients[[2]]


# Setting the number of permutations (I started at 10,000 and kept 
# changing it until I the p value stopped changing)
nsim <- 100000

# initialize vector to store randomized r's and slopes #
perms <- matrix(ncol = 2, nrow = nsim) 

# Running the loop to randomize the data and calculate slope and r.
for(i in 1:nsim){
  
  # calculate the parameters from each permutation
  r <- cor(dat$dist_to_edge, 
           sample(dat$windthrow_prct))
  slope <- lm(sample(dat$windthrow_prct) ~ dat$dist_to_edge)$coefficients[[2]]
  
  # store values of randomly generated slopes and r's
  perms[i, 1] <- r 
  perms[i, 2] <- slope
}

# Adding up the number of simulated parameters that were more extreme than the
# observed values
test.r <- sum(abs(perms[,1]) >= abs(r.obs))
test.slope <- sum(abs(perms[,2]) >= abs(slope.obs))
test.r
test.slope

# dividing by the total number of simulations to get the p value
p.r <- test.r/nsim
p.slope <- test.slope/nsim
p.r
p.slope

# Generating a histogram to show the distribution of simulated values

# need to turn variables rand1 and rand2 into a data frame for it to be 
# recognized by ggplot
perms <- as.data.frame(perms)
names(perms) <- c("r", "slope")

# Plot the actual data
dat |>
  dplyr::mutate(dist_to_edge = exp(dist_to_edge)) |>
  ggplot(aes(x = dist_to_edge,
             y = windthrow_prct)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ log(x),
              #se = FALSE,
              color = "red") +
  labs(title = "Log-Linear model `m1`",
       subtitle = "Observed points + fit line",
       x = "Distance to Edge (m)",
       y = "Windthrow (%)") +
  theme_minimal()

# make the histograms
ggplot(perms, 
       aes(x = r)) + 
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = r.obs) +
  labs(x = "Randomly generated Pearson's R values",
       subtitle = paste0("Observed R = ", round(r.obs, 2), "; P = ", p.r),
       caption = "Vertical line indicates observed Pearson's R value") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

ggplot(perms, 
       aes(x = slope)) + 
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = slope.obs) +
  labs(x = "Randomly generated slope values",
       subtitle = paste0("Observed slope = ", round(slope.obs, 3), 
                        "; P = ", p.slope),
       caption = "Vertical line indicates observed slope") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# looking at the most extreme simulated r value (this should be greater
# than the observed)
max(abs(perms$r))
max(abs(perms$slope))

# Clean up 
rm(perms, i, nsim, p.r, p.slope, r, r.obs, slope, slope.obs, test.r, test.slope)


# 04 % WINDTHROW x DISTANCE BINS ------------------------------------------

# Let's chop up the distances to edge into discrete distance
# bins to examine any nice break points (which are clearly
# visible in the data when plotted on a continuous scale)
# in the pattern btwn % windthrow ~ distance.

# Our bins will be 0-30m, 31-60m, 61-100m, 100+m.

f$dist_bins <- cut(f$dist_to_edge, 
                   breaks = c(0, 30, 60, 100, 10000),
                   include.lowest = TRUE,
                   ordered_result = TRUE)

f |>
  dplyr::select(dist_bins, windthrow_prct) |>
  na.omit() |>
  ggplot(aes(x = dist_bins, y = windthrow_prct)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  geom_signif(comparisons = list(c("[0,30]", "(30,60]"),
                                 c("(30,60]", "(60,100]"),
                                 c("(60,100]", "(100,1e+04]")),
              map_signif_level = TRUE) +
  geom_signif(comparisons = list(c("[0,30]", "(100,1e+04]")),
              y_position = 105,
              map_signif_level = TRUE) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_x_discrete(labels = c("0-30 m", "31-60 m", "61-100 m", "100+ m")) +
  labs(x = "Distance from Edge",
       y = "Windthrow %") +
  theme_minimal()

# 05 WINDTHROW SEVERITY x DIST TO EDGE --------------------------------

# A variation of the analyses in section 3, only using a 
# categorical variable (`windthrow_sev`) as our response
# variable. 

# We remove the "At risk" category because it's not an
# objective outcome, but rather a prediction
# that the den will potentially experience windthrow 
# impacts in the future.


## 05-1 Ordered logistic regression ----

# We will now be doing a proportional odds logistic regression which works to 
# compare a ranked categorical output variable with a numeric input variable.
# this model outputs the log odds of being in a given output category based 
# on a given numeric input.

# Much like for the windthrow % ~ distance to edge model, we will
# subset the data to actually be modeled into a smaller `dat` dataframe
# to keep things nicely compartmentalized. (Note this will overwrite the
# previous `dat` df.)

dat <- f[,c("den_id", "windthrow_sev", "dist_to_edge")]
dat <- unique(dat) # remove pseudo replicates with multiple repeat obs per den id
dat <- dat[dat$windthrow_sev != "At risk", ] # remove 'At risk' category
dat$windthrow_sev <- factor(dat$windthrow_sev, levels = c("Insignificant", "Moderate", "Severe")) # Remove 'At risk' from levels
dat <- na.omit(dat)

# Fit the proportional odds logistic model
p1 <- polr(windthrow_sev ~ dist_to_edge, data = dat)

# When viewing and interpreting results, note that for patch, 
# "contiguous forest" is our baseline that everything is being
# compared to. 

# For some nice guidance on how to interpret these model outputs,
# see: https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
summary(p1)
sjPlot::plot_model(p1) # View odds ratios w confidence intervals (i.e., how strong is the effect?)

# Test the assumption of proportional odds using Brant's test
# This assumption basically means that the relationship between 
# each pair of outcome groups has to be the same. We will use 
# the Brant's test for this. If the p value is greater than 0.05 
# then the assumption holds true
brant(p1)
# Seems like the model fails the Brant test.

# Get quick summary of model with odds ratios and p-values
sjPlot::tab_model(p1)
# The response variable, dist_to_edge, is significant (p = 0.033).
# More importantly, the confidence interval is nice and narrow!
# 0.99-1.00!

# Or, if you want to manually calc the p-values & odds ratios:

# Grab model coefficients
t_vals <- coef(summary(p1)) # need to wrap `p1` in `summary()` so we can grab the t values

# calculate p-values from the t value that is given.
p_value <- (1 - pnorm(abs(t_vals[ ,"t value"]), 0, 1))*2

# combine the 2 tables
t_vals <- cbind(t_vals, p_value)

# calculate odds ratios
odds_ratio <- exp(t_vals[ ,"Value"])

# combine with coefficient and p_value
(coefficients <- cbind(
  t_vals [ ,c("Value", "p_value")],
  odds_ratio
))

# Clean up
rm(coefficients, t_vals, odds_ratio, p_value)


## 05-2 Stacked area plot ----

# See here for all the documentation on plot.effects() methods
# (i.e. plots that the `effects` package specifically supports):
# https://cran.r-project.org/web/packages/effects/vignettes/predictor-effects-gallery.pdf

plot(Effect(focal.predictors = "dist_to_edge",
            mod = p1))

plot(Effect(focal.predictors = "dist_to_edge",
            mod = p1,
            xlevels = list(dist_to_edge = 0:max(dat$dist_to_edge))), # add these levels to smoothe out the curves of the graph. Otherwise you get straight lines
     style = "stacked",
     main = FALSE,
     xlab = "Distance to edge (m)",
     ylab = "Probability")

# Plot it but without the outlier at 6000m 
plot(Effect(focal.predictors = "dist_to_edge",
            mod = p1,
            xlevels = list(dist_to_edge = 0:1000)), # add these levels to smoothe out the curves of the graph. Otherwise you get straight lines
     style = "stacked",
     main = FALSE,
     xlab = "Distance to edge (m)",
     ylab = "Probability")

# 06 WINDTHROW SEVERITY x HARVEST STATUS ----------------------------------

# Chi Square test to compare two categorical variables:
# Windthrow Severity and Harvest status

# We remove the "At risk" category because it's not an
# objective outcome, but rather a prediction
# that the den will potentially experience windthrow 
# impacts in the future.

## 06-1 Chi Square ----

f |>
  dplyr::select(den_id, windthrow_sev, harvested) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
  dplyr::select(-den_id) |> 
  na.omit() |>
  dplyr::group_by(windthrow_sev, harvested) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = windthrow_sev,
                     values_from = n) |>
  tibble::column_to_rownames(var = "harvested") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Windthrow Severity x Harvest Status")

chisq <- f |>
  dplyr::select(den_id, windthrow_sev, harvested) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
  dplyr::select(-den_id) |>
  na.omit() |>
  dplyr::group_by(windthrow_sev, harvested) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = windthrow_sev,
                     values_from = n) |>
  tibble::column_to_rownames(var = "harvested") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

chisq
chisq$observed
round(chisq$expected)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)

## 06-2 Cramer's V ----

# Calculate Cramer's V
# Total sample size
n <- sum(chisq$observed)
# Phi coefficient (sqrt of chi-square statistic divided by sample size)
phi <- sqrt(chisq$statistic / n) 
# Cramer's V
cramers_v <- phi / sqrt(min(dim(chisq$observed)) - 1)  

# Print Cramer's V
cramers_v

## 06-3 Contigency table ----
# the last step is to generate confidence intervals for the true probabilities.
# We will need to do this one variable at a time and use bonferroni correction 
# because we are running 3 tests

# generating the contingency table
cont.table <- 
  f |>
  dplyr::select(den_id, windthrow_sev, harvested) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
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
  new.cont.table[2] <- sum(cont.table[row.i,-col.i])
  
  # calculating the confidence interval
  result <- prop.test(new.cont.table, conf.level = alpha, correct = FALSE)
  
  # printing the results
  print(colnames(cont.table)[col.i])
  print(rownames(cont.table)[row.i])
  print(result)
}


## 06-4 Mosaic plot ----

# Create a mosaic plot with color that shows the distribution of the windthrow
# categories vs harvest status
f |> 
  dplyr::select(den_id, windthrow_sev, harvested) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
  dplyr::select(-den_id) |> 
  droplevels() |>
  na.omit() |>
  ggplot() +
  geom_mosaic(aes(x = product(harvested), 
                  fill = windthrow_sev), 
              alpha = 1) + 
  scale_fill_manual(values = c("forestgreen", "gold", "orange")) +
  labs(y = "Windthrow Severity", 
       x = "Harvest Status") + 
  theme_mosaic() +
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 14, vjust = -0.2), 
        axis.title.y = element_text(size = 14))
  

# Clean up
rm(chisq, result, alpha, col.i, cont.table, cramers_v, i,
   n, ncol, nrow, num.entries, phi, row.i)


# 07 WINDTHROW SEVERITY x PATCH TYPE --------------------------------------

# We remove the "At risk" category because it's not an
# objective outcome, but rather a prediction
# that the den will potentially experience windthrow 
# impacts in the future.

## 07-1 Chi Square ----

f |>
  dplyr::select(den_id, windthrow_sev, patch) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
  dplyr::select(-den_id) |> 
  na.omit() |>
  dplyr::group_by(windthrow_sev, patch) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = windthrow_sev,
                     values_from = n) |>
  tibble::column_to_rownames(var = "patch") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  as.table() |>
  gplots::balloonplot(main = "Windthrow Severity x Harvest Status")

chisq <- f |>
  dplyr::select(den_id, windthrow_sev, patch) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
  dplyr::select(-den_id) |>
  na.omit() |>
  dplyr::group_by(windthrow_sev, patch) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = windthrow_sev,
                     values_from = n) |>
  tibble::column_to_rownames(var = "patch") |>
  dplyr::mutate_all(~replace(., is.na(.), 0)) |>
  as.matrix() |>
  chisq.test()

chisq
chisq$observed
round(chisq$expected)
corrplot::corrplot(chisq$residuals, is.corr = FALSE)

## 07-2 Cramer's V ----

# Calculate Cramer's V
# Total sample size
n <- sum(chisq$observed)
# Phi coefficient (sqrt of chi-square statistic divided by sample size)
phi <- sqrt(chisq$statistic / n) 
# Cramer's V
cramers_v <- phi / sqrt(min(dim(chisq$observed)) - 1)  

# Print Cramer's V
cramers_v


## 07-3 Mosaic plot ----

# Note that this plot differs from the ribbon plot below
# because it simply visualizes the distribution of the data
# itself. The stacked area plot, on the other hand, visualizes
# the actual modeled odds ratios - i.e. after running some 
# stats what the odds are of each patch category being within
# each severity category. 
f |> 
  dplyr::select(den_id, windthrow_sev, patch) |>
  dplyr::filter(windthrow_sev != "At risk") |>
  dplyr::distinct() |> # remove pseudo replicated yearly repeats
  dplyr::select(-den_id) |> 
  droplevels() |>
  na.omit() |>
  ggplot() +
  geom_mosaic(aes(x = product(patch), 
                  fill = windthrow_sev), 
              alpha = 1) + 
  scale_fill_manual(values = c("forestgreen", "gold", "orange")) +
  labs(y = "Windthrow Severity", 
       x = "Patch size (category)") + 
  theme_mosaic() +
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 14, vjust = -0.2), 
        axis.title.y = element_text(size = 14))


## 07-4 Ordered logistic regression ----

# We will now be doing a proportional odds logistic regression which works to 
# compare a ranked categorical output variable with a numeric input variable.
# this model outputs the log odds of being in a given output category based 
# on a given numeric input.

# Much like for the windthrow % ~ distance to edge model, we will
# subset the data to actually be modeled into a smaller `dat` dataframe
# to keep things nicely compartmentalized. (Note this will overwrite the
# previous `dat` df.)

dat <- f[,c("den_id", "windthrow_sev", "patch")]
dat <- unique(dat) # remove pseudo replicates with multiple repeat obs per den id
dat <- dat[dat$windthrow_sev != "At risk", ] # remove 'At risk' category
dat$windthrow_sev <- factor(dat$windthrow_sev, levels = c("Insignificant", "Moderate", "Severe")) # Remove 'At risk' from levels
dat <- na.omit(dat)

# Fit the proportional odds logistic model
p2 <- polr(windthrow_sev ~ patch, data = dat)

# When viewing and interpreting results, note that for patch, 
# "contiguous forest" is our baseline that everything is being
# compared to. 

# For some nice guidance on how to interpret these model outputs,
# see: https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
summary(p2)
sjPlot::plot_model(p2) # View odds ratios w confidence intervals (i.e., how strong is the effect?)

# Test the assumption of proportional odds using Brant's test
# This assumption basically means that the relationship between 
# each pair of outcome groups has to be the same. We will use 
# the Brant's test for this. If the p value is greater than 0.05 
# then the assumption holds true
brant(p2)
# The omnibus test has a p value of 0.31 which means the assumption is valid

# Get quick summary of model with odds ratios and p-values
sjPlot::tab_model(p2)

# Or, if you want to manually calc the p-values & odds ratios:

# Grab model coefficients
t_vals <- coef(summary(p2)) # need to wrap `p2` in `summary()` so we can grab the t values

# calculate p-values from the t value that is given.
p_value <- (1 - pnorm(abs(t_vals[ ,"t value"]), 0, 1))*2

# combine the 2 tables
t_vals <- cbind(t_vals, p_value)

# calculate odds ratios
odds_ratio <- exp(t_vals[ ,"Value"])

# combine with coefficient and p_value
(coefficients <- cbind(
  t_vals [ ,c("Value", "p_value")],
  odds_ratio
))

# Clean up
rm(coefficients, t_vals, odds_ratio, p_value)


## 07-5 Stacked area plot ----

# See here for all the documentation on plot.effects() methods
# (i.e. plots that the `effects` package specifically supports):
# https://cran.r-project.org/web/packages/effects/vignettes/predictor-effects-gallery.pdf

plot(Effect(focal.predictors = "patch",
            mod = p2))

plot(Effect(focal.predictors = "patch",
            mod = p2),
     style = "stacked",
     main = FALSE,
     xlab = "Patch size (categories)",
     ylab = "Probability")






