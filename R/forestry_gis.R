
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
#' @param vri_year Reference year of the VRI data (default assumption of the 2023 dataset)
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
  stopifnot("`fvl` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`fvl` must be a sf class MULTIPOLYGON geometry." = any(sf::st_geometry_type(fvl) %in% c('MULTIPOLYGON')))
  stopifnot("`feature` must be in the same CRS as `fvl`." = sf::st_crs(feature) == sf::st_crs(fvl))
  
  circle <- sf::st_buffer(feature, dist = m)
  circle <- sf::st_intersection(circle, fvl)
  
  forested_m2 <- sum(sf::st_area(circle[circle$forested == "Forested",]))
  total_m2 <- sum(sf::st_area(circle))
  
  out <- units::drop_units(forested_m2/total_m2)
  return(out)
}



#' Distance from point feature to nearest road
#'
#' @param feature Point feature (e.g., den)
#' @param date_col (optional) Column name in `feature` that contains a date to use as the reference year for roads subsetting
#' @param filter_by_date (boolean) Should the function filter roads by construction date when calculating distance to feature?
#'
#' @return
#' @export
#'
#' @examples
st_distance_nearest_road <- function(feature, date_col = "date_inspected", filter_by_date = TRUE) {
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = sf::st_geometry_type(feature) %in% 'POINT')
  
  if (filter_by_date) {
    stopifnot("Can only evaluate distance to road for one feature at a time if `filter_by_date == TRUE`" = nrow(feature) == 1)
    roads <- roads[which((roads$award_date <= feature[[date_col]] | is.na(roads$award_date)) & (roads$retirement_date >= feature[[date_col]] | is.na(roads$retirement_date) | roads$life_cycle_status_code == 'ACTIVE')),]
  }
  
  out <- nngeo::st_nn(feature, roads, returnDist = T, progress = F)
  
  if (filter_by_date) {
    out <- out$dist[[1]][1]
  } else {
    out <- out$dist
  }
  
  return(out)
}

