# Important paper summary stats
# Run after doing data prep in bear_den_data_summary.Rmd

# Sample size of first-in-series data
f_analysis |>
  dplyr::arrange(den_id, date_inspected_den) |>
  dplyr::group_by(den_id) |>
  dplyr::slice(1) |>
  nrow()

# Sample size per den_status + den_status_binary category
f_analysis |> 
  sf::st_drop_geometry() |>
  dplyr::select(den_status, den_status_binary) |>
  plyr::count() |>
  knitr::kable()

f_analysis |>
  sf::st_drop_geometry() |>
  dplyr::select(den_status_binary) |>
  plyr::count() |> 
  knitr::kable()

# N per region
plyr::count(dens$region)

# N per district
plyr::count(dens$district)

# N in parks versus not
plyr::count(dens$in_park)

# Den characteristics summary stats
plyr::count(dens$den_tree_species)

dens |> 
  sf::st_drop_geometry() |>
  dplyr::ungroup() |>
  dplyr::mutate(chamber_dimensions = chamber_width * chamber_length) |>
  dplyr::select(elevation_m, slope_pct, slope_aspect,
                entrance_width_functional, entrance_height_functional,
                chamber_length, chamber_width, chamber_height,
                chamber_dimensions,
                den_tree_dbh) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = "variable") |>
  dplyr::filter(!is.na(value)) |>
  dplyr::group_by(variable) |>
  dplyr::summarise(n = dplyr::n(),
                   min = min(value, na.rm = TRUE),
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   mean = mean(value, na.rm = TRUE),
                   sd = sd(value, na.rm = TRUE))

# Circular mean for slope aspect
sinr <- sum(sin(dens$slope_aspect * pi/ 180), na.rm = TRUE)
cosr <- sum(cos(dens$slope_aspect * pi/ 180), na.rm = TRUE)
circmean <- atan2(sinr, cosr)
(circmean * 180 / pi) + 360


# This really should be a den variable, not f var
f_analysis |> 
  sf::st_drop_geometry() |>
  dplyr::ungroup() |>
  dplyr::select(den_id, nearest_tree_m) |>
  dplyr::distinct() |>
  dplyr::filter(!is.na(nearest_tree_m)) |>
  dplyr::select(nearest_tree_m) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = "variable") |>
  dplyr::group_by(variable) |>
  dplyr::summarise(n = n(),
                   min = min(value, na.rm = TRUE),
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   mean = mean(value, na.rm = TRUE),
                   sd = sd(value, na.rm = TRUE))

# Model vars summary stats
f_analysis |> 
  sf::st_drop_geometry() |>
  dplyr::ungroup() |>
  dplyr::select(year,
                nearest_tree_m, v_prop_forest_60m, 
                v_dist_lt40, v_dist_gt40, dist_from_edge,
                v_dist_road, windthrow_prct, 
                lt_3, gt_8, road_density_m2) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = "variable") |>
  dplyr::filter(!is.na(value)) |>
  dplyr::group_by(variable) |>
  dplyr::summarise(n = n(),
                   min = min(value, na.rm = TRUE),
                   max = max(value, na.rm = TRUE),
                   median = median(value, na.rm = TRUE),
                   mean = mean(value, na.rm = TRUE),
                   sd = sd(value, na.rm = TRUE))


# N dens exposed to X condition
length(unique(f[["den_id"]][f$windthrow_prct > 0])) 
length(unique(f[["den_id"]][f$lt_3 > 0])) # etc. etc
length(unique(f[["den_id"]][f$gt_8 == 0])) # etc. etc
