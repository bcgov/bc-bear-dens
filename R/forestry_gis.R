
# Forestry GIS scripts

# TODO: In an ideal world, make FVL it's own type of object class

#' Create the Forestry Verification Layer for a given year
#' 
#' This function is still in its infancy, and easily broken if incorrect VRI
#' or depletions data is supplied. Remember to subset the VRI and depletions
#' data only to your area of interest, as these datasets are huge!
#'
#' @param den_year Year that the den was visited, i.e., reference year to use for forestry data and forest age
#' @param vri sf object containing VRI data
#' @param depletions sf object containing depletions data
#'
#' @return An object of class `sf` containig two polygon areas: forested and non-forested
#' @export
create_fvl <- function(den_year, vri, depletions) {
  
  vri_year <- unique(lubridate::year(vri$projected_date))
  
  # If den_year > vri_year, use 2023 vri data, but just flash a warning
  # that the forestry data might not be up-to-date.
  if (vri_year < den_year) {
    warning("The VRI data (", vri_year, ") is older than your den year (", den_year, "). Forestry verifications for ", den_year, " may be inaccurate.")
    den_year <- vri_year
  }
  
  # Clean up names
  vri <- janitor::clean_names(vri)
  depletions <- janitor::clean_names(depletions)
  
  # Subtract the den year from the VRI reference year
  year_diff <- vri_year - den_year
  
  # Reclassify the age classes of the polygons to less than 40
  # or greater than 40 yo (at the time of the bear den visit).
  message("Extracting non-forested areas circa ", den_year, "...")
  lt40 <- vri[(vri$proj_age_1 - year_diff) <= 40,] |>
    dplyr::summarise()
  
  message("Extracting forested areas circa ", den_year, "...")
  gt40 <- vri[(vri$proj_age_1 - year_diff) > 40,] |>
    dplyr::summarise()
  
  # Extract depletions that occurred during the same year or before
  # the den visit.
  message("Extracting depletions circa ", den_year, "...")
  depletions <- depletions[depletions$depletion_year <= den_year, ]
  
  # Merge into single poly
  depletions <- dplyr::summarise(depletions)
  if (!sf::st_is_valid(depletions)) depletions <- sf::st_make_valid(depletions)
  
  # Then union the depletions to lt40 and 
  # substract the depletions from gt40 
  # for the correct years.
  message("Merging depletions layer with non-forested VRI...")
  lt40 <- sf::st_union(lt40, depletions) #|>
    #sf::st_collection_extract() |> # stupid we have to change back to multipolygon...
    #sf::st_cast('MULTIPOLYGON') |> 
    #sf::st_union()
  
  message("Deleting depletions layer from forested VRI...")
  gt40 <- sf::st_difference(gt40, depletions) #|>
    #sf::st_collection_extract() |> # stupid we have to change back to multipolygon...
    #sf::st_cast('MULTIPOLYGON') |> 
    #sf::st_union()
  
  rm(depletions)
  
  # Add classification
  gt40$forested <- "Forested"
  lt40$forested <- "Non-Forested"
  
  # Merge and spit out
  out <- dplyr::bind_rows(gt40, lt40)
  out <- sf::st_collection_extract(out)
  
  return(out)
  
}


#' Proportion of area forested within a given radius around a given point feature
#'
#' @param feature Point feature (e.g., den)
#' @param fvl Forest verification layer to use for proportion forested calculation
#' @param m Buffer radius around point feature, in meters
#'
#' @return Proportion (out of 1) of area within the circle covered by forest
#' @export
st_proportion_forested <- function(feature, fvl, m = 60) {
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = sf::st_geometry_type(feature) %in% 'POINT')
  stopifnot("`fvl` must be a sf class geometry." = inherits(fvl, "sf"))
  stopifnot("`fvl` must be a sf class MULTIPOLYGON or POLYGON geometry." = any(sf::st_geometry_type(fvl) %in% c('MULTIPOLYGON', 'POLYGON')))
  stopifnot("`feature` must be in the same CRS as `fvl`." = sf::st_crs(feature) == sf::st_crs(fvl))
  
  stopifnot("At this time this function only supports evaluating proportion forested for one feature at a time." = nrow(feature) == 1)
  
  circle <- sf::st_buffer(feature, dist = m)
  circle <- suppressWarnings(sf::st_intersection(circle, fvl))
  
  forested_m2 <- sum(sf::st_area(circle[circle$forested == "Forested",]))
  total_m2 <- sum(sf::st_area(circle))
  
  out <- units::drop_units(forested_m2/total_m2)
  return(out)
}



#' Distance from point feature to nearest road
#'
#' @param feature Point feature (e.g., den)
#' @param roads Linestring feature containing roads
#' @param date_col (optional) Column name in `feature` that contains a date to use as the reference year for roads subsetting
#' @param filter_by_date (boolean) Should the function filter roads by construction date when calculating distance to feature?
#'
#' @return
#' @export
st_distance_nearest_road <- function(feature, roads, date_col = "date_inspected", filter_by_date = TRUE) {
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = all(sf::st_geometry_type(feature) %in% 'POINT'))
  stopifnot("`roads` must be a sf class geometry." = inherits(roads, "sf"))
  stopifnot("`roads` must be a sf class LINESTRING geometry." = all(sf::st_geometry_type(roads) %in% 'LINESTRING'))
  
  
  if (filter_by_date) {
    stopifnot("Can only evaluate distance to road for one feature at a time if `filter_by_date == TRUE`" = nrow(feature) == 1)
    roads <- roads[which((roads$award_date <= feature[[date_col]] | is.na(roads$award_date)) & (roads$retirement_date >= feature[[date_col]] | is.na(roads$retirement_date) | roads$life_cycle_status_code == 'ACTIVE')),]
  }
  
  out <- suppressMessages(nngeo::st_nn(feature, roads, returnDist = T, progress = F))
  
  if (filter_by_date) {
    out <- out$dist[[1]][1]
  } else {
    out <- out$dist
  }
  
  return(out)
}


#' Calculate percent forest cover of each age class around a feature
#' 
#' This function requires both the VRI layer and the depletions layer
#' as inputs to accurately calculate age class forest cover around a 
#' den for a given year.
#'
#' @param feature Point feature (e.g., den)
#' @param buffer Buffer size around point feature, in meters (default 1500 m)
#' @param feature_date_col Date column in the feature layer
#' @param vri VRI polygon `sf` object
#' @param vri_year_col VRI reference date column (i.e., year of the dataset)
#' @param vri_age_col Column name containing forest age (typically, 'proj_age_1' if column names cleaned up)
#' @param depletions Depletions polygon `sf` object
#' @param depletions_year_col Column that contains polygon depletion year
#'
#' @return
#' @export
prct_age_class_buffer <- function(feature, buffer = 1500, feature_date_col = "date_inspected",
                                  vri, vri_year_col = "projected_date", vri_age_col = "proj_age_1",
                                  depletions, depletions_year_col = "depletion_year") {
  # Data health checks
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = sf::st_geometry_type(feature) %in% 'POINT')
  stopifnot("`vri` must be a sf class geometry." = inherits(vri, "sf"))
  stopifnot("`vri` must be a sf class MULTIPOLYGON or POLYGON geometry." = any(sf::st_geometry_type(vri) %in% c('MULTIPOLYGON', 'POLYGON')))
  stopifnot("`depletions` must be a sf class geometry." = inherits(depletions, "sf"))
  stopifnot("`depletions` must be a sf class MULTIPOLYGON or POLYGON geometry." = any(sf::st_geometry_type(depletions) %in% c('MULTIPOLYGON', 'POLYGON')))
  stopifnot("Your VRI layer and depletions layer are a different CRS." = sf::st_crs(vri) == sf::st_crs(depletions))
  
  # Extract feature year, vri year
  f1_year <- unique(lubridate::year(feature[[feature_date_col]]))
  vri_year <- unique(lubridate::year(vri[[vri_year_col]]))
  
  # Ensure name of the geometry column is the same for all features
  sf::st_geometry(feature) <- "geom"
  sf::st_geometry(vri) <- "geom"
  sf::st_geometry(depletions) <- "geom"
  
  # Buffer point feature
  f1 <- sf::st_buffer(feature, buffer)
  # Drop any non-geometry cols, as they may cause errors down the line
  f1 <- sf::st_geometry(f1)
  
  # Intersect f1 with VRI
  f1_vri <- suppressWarnings(sf::st_intersection(vri, f1))
  f1_vri$age <- f1_vri[[vri_age_col]] - (vri_year - f1_year) # subtract the age gap btwn vri year and den year - that's how many years older the VRI dataset is than the den visit
  f1_vri <- f1_vri[which(f1_vri$age > 0),]
  # Intersect f1 with depletions
  f1_deps <- suppressWarnings(sf::st_intersection(depletions, f1))
  f1_deps$age <- f1_year - f1_deps[[depletions_year_col]]
  f1_deps <- f1_deps[which(f1_deps$age > 0),]
  # Combine the two
  # Very important to st_combine the second feature first, otherwise you
  # get bizarre results. https://github.com/r-spatial/sf/issues/770
  f1_1 <- suppressWarnings(sf::st_difference(f1_vri, sf::st_make_valid(sf::st_combine(f1_deps)))) # cut out deps first
  f1_2 <- suppressWarnings(sf::st_intersection(f1_deps, sf::st_make_valid(sf::st_combine(f1_vri))))
  f1_final <- dplyr::bind_rows(f1_1, f1_2)
  # Categorize into age class
  f1_final <- f1_final |> 
    dplyr::mutate(age_class = dplyr::case_when(
      age < 21 ~ 1,
      age >= 21 & age < 41 ~ 2,
      age >= 41 & age < 61 ~ 3,
      age >= 61 & age < 81 ~ 4,
      age >= 81 & age < 101 ~ 5,
      age >= 101 & age < 121 ~ 6,
      age >= 121 & age < 141 ~ 7,
      age >= 141 & age < 251 ~ 8,
      age >= 251 ~ 9
    ))
  # Calculate area of each age class
  f1_final$area <- sf::st_area(f1_final)
  # Add dummy rows of 0 area so each age class shows up in the final
  # total
  dummy <- data.frame(age_class = 1:9, area = 0)
  dummy$area <- units::set_units(dummy$area, "m2")
  f1_final <- dplyr::bind_rows(f1_final, dummy)
  # Calculate area by age class
  out <- aggregate(area ~ age_class, f1_final, FUN = "sum")
  out$prct_forest_area <- units::drop_units(out$area / sum(out$area)) 
  out$prct_total_area <- units::drop_units(out$area / sum(sf::st_area(f1)))
  return(out)
  
}



#' Calculate road density (in m2) around a feature
#'
#' @param feature Point feature (e.g., den)
#' @param feature_buffer Buffer size around point feature, in meters (default 1500 m)
#' @param roads Linestring feature containing roads
#' @param roads_buffer_col Column name in `roads` that contains road buffering distance (e.g., meters buffer for highways vs roads)
#' @param feaure_date_col (optional) Column name in `feature` that contains a date to use as the reference year for roads subsetting
#' @param filter_by_date (boolean) Should the function filter roads by construction date when calculating distance to feature?
#'
#' @return
#' @export
road_density_buffer <- function(feature, feature_buffer = 1500, feature_date_col = "date_inspected",
                                roads, roads_buffer_col = "buffer", 
                                filter_by_date = TRUE, 
                                return_road_area = TRUE) {
  # Data health checks
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = all(sf::st_geometry_type(feature) %in% 'POINT'))
  stopifnot("`roads` must be a sf class geometry." = inherits(roads, "sf"))
  stopifnot("`roads` must be a sf class LINESTRING geometry." = all(sf::st_geometry_type(roads) %in% 'LINESTRING'))
  stopifnot("`feature` must be in the same CRS as `roads`." = sf::st_crs(feature) == sf::st_crs(roads))
  
  if (filter_by_date) {
    stopifnot("Can only evaluate distance to road for one feature at a time if `filter_by_date == TRUE`" = nrow(feature) == 1)
    roads <- roads[which((roads$award_date <= feature[[feature_date_col]] | is.na(roads$award_date)) & (roads$retirement_date >= feature[[feature_date_col]] | is.na(roads$retirement_date) | roads$life_cycle_status_code == 'ACTIVE')),]
  }
  
  # Select roads that touch feature
  roads <- roads[sf::st_is_within_distance(roads, feature, dist = feature_buffer, sparse = F),]
  
  # Buffer and union roads according to buffer col
  roads <- sf::st_buffer(roads, dist = roads[[roads_buffer_col]])
  roads <- sf::st_union(roads)
  
  # Buffer feature
  # If we're ignoring the road dates, we can just use the dens df
  feature <- sf::st_buffer(feature, feature_buffer)
  
  # Intersect buffered roads with feature
  intxn <- suppressWarnings(sf::st_intersection(feature, roads))
  
  # Calculate road area
  intxn$road_area <- sf::st_area(intxn)
  
  # Replace NA road areas with zero
  intxn$road_area <- ifelse(is.na(intxn$road_area), 0, intxn$road_area)
  
  # Calculate density
  out <- intxn$road_area / (pi * (feature_buffer^2))
  
  # If user wants road area returned as well
  if (return_road_area) {
    out <- list(out)
    out[[2]] <- intxn$road_area
    out[[3]] <- (pi * (feature_buffer^2))
    names(out) <- c("road_density_m2", "road_area", "total_area")
    out$road_density <- ifelse(length(out$road_density) == 0, 0, out$road_density)
    out$road_area <- ifelse(length(out$road_area) == 0, 0, out$road_area)
  }
  
  return(out)
  
}
