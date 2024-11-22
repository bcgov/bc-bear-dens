
# Forestry GIS scripts

# TODO: In an ideal world, make FVL it's own type of object class

#' Read like WKT files
#' 
#' Large GIS files are stored as CSVs, with the geometry stored in the
#' 'WKT_GEOM' column. This function reads the CSVs with `{arrow}`, then
#' converts it to an `sf` object with a standard CRS of 3005 (BC Albers).
#'
#' @param filepath Path to csv with spatial data.
#' @param wkt_col Column name containing WKT geometry. Default is `WKT_GEOM`.
#' @param crs Coordinate reference system of the spatial data. Default is `3005` (BC Albers).
#'
#' @return
#' @export
read_lrg_wkt <- function(filepath, wkt_col = "WKT_GEOM", crs = 3005) {
  x <- arrow::read_csv_arrow(filepath)
  x <- as.data.frame(x)
  x <- sf::st_as_sf(x, wkt = wkt_col)
  sf::st_crs(x) <- crs
  # Maybe drop the giant WKT column - don't need it anymore?
  return(x)
}

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
  
  sf::st_geometry(vri) <- "geom"
  sf::st_geometry(depletions) <- "geom"
  
  # Subtract the den year from the VRI reference year
  year_diff <- vri_year - den_year
  
  # Reclassify the age classes of the polygons to less than 40
  # or greater than 40 yo (at the time of the bear den visit).
  # Any polygons that were depleted AFTER the den year would be negative.
  # Any polygons that were depleted THE SAME YEAR as the den would == 0.
  vri$reference_age <- vri$proj_age_1 - year_diff 
  
  message("Extracting non-forested areas circa ", den_year, "...")
  lt40 <- vri[(vri$reference_age <= 40 & vri$reference_age >= 0),] |>
    dplyr::summarise()
  
  message("Extracting forested areas circa ", den_year, "...")
  gt40 <- vri[(vri$reference_age > 40 | vri$reference_age < 0),] |>
    dplyr::summarise()
  
  # Extract depletions that occurred during the same year or before
  # the den visit.
  message("Extracting depletions circa ", den_year, "...")
  depletions <- depletions[depletions$depletion_year <= den_year, ]
  
  # Clean up depletions
  # depletions <- 
  # depletions |>
  #   sf::st_cast("MULTIPOLYGON") |> # cast to multipolygon first, or some geometries will be deleted
  #   sf::st_cast("POLYGON") |>
  #   sf::st_make_valid() |>
  #   tibble::rownames_to_column(var = "objectid2") |>
  #   dplyr::mutate(area2 = sf::st_area(geom)) |>
  #   dplyr::filter(area2 > units::set_units(1600, "m2")) |> # remove little tiny shards polygons
  #   nngeo::st_remove_holes(max_area = 1600) |>
  #   sf::st_make_valid() |>
  #   dplyr::select(objectid2)
  
  # Extract wildlife retention patches from depletions layer
  # i.e., extract holes in depletions polygons
  # We're changing tack here, and simply doing st_difference to extract
  # message("Extracting wildlife retention patches circa ", den_year, "...")
  # depletions_no_holes <- nngeo::st_remove_holes(depletions)
  # #depletions_no_holes <- sf::st_as_sf(sfheaders::sf_remove_holes(depletions))
  # wrp <- sf::st_difference(depletions_no_holes, depletions)
  # wrp <- wrp[wrp$objectid2 == wrp$objectid2.1,] # this is wild and I'm not sure why this is happening, but this is the fix for now
  # wrp <- sf::st_combine(wrp)
  
  # Merge into single poly
  depletions <- dplyr::summarise(depletions)
  if (!sf::st_is_valid(depletions)) depletions <- sf::st_make_valid(depletions)
  
  # Extract wildlife retention area holes
  message("Extracting wildlife retention patches circa ", den_year, "...")
  wrp <-
    depletions |>
    sf::st_cast("MULTIPOLYGON") |> # cast to multipolygon first, or some geometries will be deleted
    sf::st_cast("POLYGON") |>
    dplyr::mutate(area2 = sf::st_area(geom)) |>
    dplyr::filter(area2 > units::set_units(1600, "m2")) |> # remove little tiny shards polygons
    nngeo::st_remove_holes(max_area = 1600) |> # remove tiny holes
    #sf::st_cast("MULTIPOLYGON") |>
    sf::st_coordinates() |>
    as.data.frame() |>
    dplyr::filter(L1 > 1) |>
    dplyr::mutate(objectid = paste0(L1, ".", L2)) |>
    sfheaders::sf_polygon(x = "X", y = "Y", polygon_id = "objectid" , keep = TRUE) |>
    sf::st_combine() |>
    sf::st_union() |>
    sf::st_set_crs(3005) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
  
  # Then union the depletions to lt40 and 
  # substract the depletions from gt40 
  # for the correct years.
  message("Merging depletions layer with non-forested VRI...")
  lt40 <- sf::st_union(lt40, depletions) |>
    sf::st_difference(sf::st_combine(wrp))
  
  message("Deleting depletions layer from forested VRI and merging WRPs...")
  gt40 <- sf::st_difference(gt40, depletions) |> # not sure why this is creating a geometrycollection??
    sf::st_collection_extract('POLYGON') |> # stupid we have to change back to multipolygon...
    sf::st_union() |>
    sf::st_as_sf() |>
    sf::st_union(wrp)
  
  rm(depletions)
  
  message("Combining forested and non-forested layers...")
  
  # Ensure geometry columns have the same name
  sf::st_geometry(lt40) <- "geom"
  sf::st_geometry(gt40) <- "geom"
  
  # Add classification
  gt40$forested <- "Forested"
  lt40$forested <- "Non-Forested"
  
  # Merge and spit out
  out <- dplyr::bind_rows(gt40, lt40)
  
  # Ensure the result spits out a (multi)polygon
  out <- sf::st_collection_extract(out, "POLYGON")
  
  out$year <- den_year
  
  return(out)
  
}


#' Proportion of area forested within a given radius around a given point feature
#'
#' @param feature Point feature (e.g., dens)
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
  
  # Assume each row is unique feature
  # This is necessary to do bc some dens/sample ids are duplicated, so if 
  # you try to group data by den/sample_id, you double the areas.
  feature$id_col <- 1:nrow(feature)
  
  circle <- sf::st_buffer(feature, dist = m)
  circle <- suppressWarnings(sf::st_intersection(circle, fvl))
  circle$area_m2 <- units::drop_units(sf::st_area(circle))
  
  x <- aggregate(area_m2 ~ forested + id_col, circle, FUN = "sum")
  # Add dummy row if one of the id's is missing (i.e., doesn't intersect
  # w FVL at all, as in the case of SAY_CampbellLake_1_20180808)
  if (any(!(feature$id_col %in% x$id_col))) {
    missing_ids <- feature$id_col[!(feature$id_col %in% x$id_col)]
    x <- rbind(x, data.frame(forested = "Forested",
                             id_col = missing_ids,
                             area_m2 = 0))
  }
  x <- tidyr::pivot_wider(x, 
                          id_cols = "id_col", 
                          names_from = "forested", 
                          values_from = "area_m2", 
                          values_fn = sum)
  # Add in 'Forested' or 'Non-Forested' column if it's not present
  if (length(x) < 3) {
    if (!('Forested' %in% names(x))) {
      x$Forested <- NA
    } else {
        x$`Non-Forested` <- NA
      }
  }
  x[is.na(x)] <- 0
  x$total <- rowSums(x[,2:3], na.rm = TRUE)
  
  x$prop_forested <- x$Forested / x$total
  
  out <- x$prop_forested
  return(out)
}


#' Calculate percent forest cover of each age class around a feature
#' 
#' This function requires both the VRI layer and the depletions layer
#' as inputs to accurately calculate age class forest cover around a 
#' den for a given year. If the verification is for a year prior to a
#' cutblock being taken out, the function will take the average age of
#' all forest stands in the radius and assign that average value as the
#' age of the stand prior to being cut.
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
st_proportion_age_class <- function(feature, buffer = 1500, feature_date_col = "date_inspected",
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
  
  # Assume each row is a unique feature
  # This is necessary to do bc some dens/sample ids are duplicated, so if 
  # you try to group data by den/sample_id, you double the areas.
  feature$id_col <- 1:nrow(feature)
  
  # Extract feature year, vri year
  f1_year <- unique(lubridate::year(feature[[feature_date_col]]))
  vri_year <- unique(lubridate::year(vri[[vri_year_col]]))
  
  stopifnot("Can only do proportion forested calcs for one year at a time." = length(f1_year) == 1)
  
  # Ensure name of the geometry column is the same for all features
  sf::st_geometry(feature) <- "geom"
  sf::st_geometry(vri) <- "geom"
  sf::st_geometry(depletions) <- "geom"
  
  # Buffer point feature
  f1 <- sf::st_buffer(feature, buffer)
  # Drop any data cols, as they may cause errors down the line
  f1 <- f1[,c("id_col", "sample_id", "geom")]
  #f1 <- sf::st_geometry(f1)
  
  # Intersect f1 with VRI
  f1_vri <- suppressWarnings(sf::st_intersection(vri, f1))
  # Calculate area of each stand 
  f1_vri$area <- sf::st_area(f1_vri)
  # Calculate age of each stand in the year the den was visited
  f1_vri$age <- f1_vri[[vri_age_col]] - (vri_year - f1_year) # subtract the age gap btwn vri year and den year - that's how many years older the VRI dataset is than the den visit
  # Assign mean forest stand age to "negative" aged stands
  w_mean_age <- weighted.mean(f1_vri[["age"]][f1_vri$age > 0], w = units::drop_units(f1_vri[["area"]][f1_vri$age > 0]))
  f1_vri[["age"]][f1_vri$age < 0] <- w_mean_age
  # Intersect f1 with depletions
  f1_deps <- suppressWarnings(sf::st_intersection(depletions, f1))
  f1_deps$age <- f1_year - f1_deps[[depletions_year_col]]
  # Remove negative depletion years - these hadn't been cut yet at the time of den visit year
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
      age >= 251 ~ 9,
      TRUE ~ 999 # in cases with negative stand-age, we don't know how old it was prior to being cut...
    )) |>
    dplyr::select(id_col, sample_id, age_class, area)
  
  # Calculate area of each age class
  f1_final$area <- units::drop_units(sf::st_area(f1_final))
  f1_final <- sf::st_drop_geometry(f1_final)
  # Add dummy rows of 0 area so each age class shows up in the final total
  dummy <- data.frame(id_col = rep(feature$id_col, 9),
                      sample_id = rep(feature$sample_id, 9),
                      age_class = rep(1:9, each = length(unique(feature$id_col))),
                      area = 0)
  f1_final <- dplyr::bind_rows(f1_final, dummy)
  # Calculate area by age class
  out <- aggregate(area ~ id_col + sample_id + age_class, f1_final, FUN = "sum", na.action = na.pass)
  
  out <- out |> 
    tidyr::pivot_wider(id_cols = c("id_col", "sample_id"), 
                       names_from = age_class, 
                       names_prefix = "age_class_", 
                       values_from = area, 
                       values_fn = sum)
  
  out$forest_area_m2 <- rowSums(out[,3:11])
  out$total_area_m2 <- pi * buffer^2
  
  # Convert to percentages
  out$age_class_1 <- round((out$age_class_1 / out$forest_area_m2 * 100), 1)
  out$age_class_2 <- round((out$age_class_2 / out$forest_area_m2 * 100), 1)
  out$age_class_3 <- round((out$age_class_3 / out$forest_area_m2 * 100), 1)
  out$age_class_4 <- round((out$age_class_4 / out$forest_area_m2 * 100), 1)
  out$age_class_5 <- round((out$age_class_5 / out$forest_area_m2 * 100), 1)
  out$age_class_6 <- round((out$age_class_6 / out$forest_area_m2 * 100), 1)
  out$age_class_7 <- round((out$age_class_7 / out$forest_area_m2 * 100), 1)
  out$age_class_8 <- round((out$age_class_8 / out$forest_area_m2 * 100), 1)
  out$age_class_9 <- round((out$age_class_9 / out$forest_area_m2 * 100), 1)
  
  # Drop id_col and remove and duplicated dens
  out <- out[,-1]
  out <- out[!duplicated(out),]
  
  # Add back some useful metadata
  out$year <- f1_year
  if (nrow(out) == 1) {
    out$den_id <- stringr::str_split(out$sample_id, "_", 4, simplify = TRUE)[,1:3] |> 
      paste(collapse = "_")
  } else {
    out$den_id <- stringr::str_split(out$sample_id, "_", 4, simplify = TRUE)[,1:3] |>
      as.data.frame() |>
      tidyr::unite(.) |>
      dplyr::pull()
  }
  
  out <- dplyr::select(out, den_id, sample_id, year, dplyr::everything())
  
  return(out)
}



query_roads <- function(den_date, retirement_buffer) {
  den_date <- as.Date(den_date)
  q <- paste0("SELECT * FROM roads WHERE (award_date <= '", 
         den_date, 
         "' OR award_date IS NULL) AND (retirement_date <= '", 
         den_date + lubridate::years(retirement_buffer), 
         "' OR retirement_date IS NULL OR life_cycle_status_code = 'ACTIVE')")
  return(q)
}


#' Distance from point feature to nearest road
#'
#' @param feature Point feature (e.g., den)
#' @param roads Linestring feature containing roads
#' @param date_col (optional) Column name in `feature` that contains a date to use as the reference year for roads subsetting
#' @param filter_by_date (boolean) Should the function filter roads by construction date when calculating distance to feature?
#' @param retirement_buffer Number of years by retirement date should be pushed back to still include in the roads filter. E.g., to include roads that were retired in 2013 in the calculation for a bear den that was visited in 2018, set `retirement_buffer = 5`.
#'
#' @return
#' @export
st_distance_nearest_road <- function(feature, roads, 
                                     date_col = "date_inspected", 
                                     filter_by_date = TRUE,
                                     retirement_buffer) {
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = all(sf::st_geometry_type(feature) %in% 'POINT'))
  stopifnot("`roads` must be a sf class geometry." = inherits(roads, "sf"))
  #stopifnot("`roads` must be a sf class LINESTRING geometry." = all(sf::st_geometry_type(roads) %in% 'LINESTRING'))
  
  
  if (filter_by_date) {
    stopifnot("Can only evaluate distance to road for one feature at a time if `filter_by_date == TRUE`" = nrow(feature) == 1)
    # Only road SECTIONS has these three columns
    sql_query <- query_roads(den_date = feature[[date_col]],
                             retirement_buffer = retirement_buffer)
    roads <- tidyquery::query(sql = sql_query)
  }
  
  out <- suppressMessages(nngeo::st_nn(feature, roads, returnDist = T, progress = F))
  
  if (filter_by_date) {
    out <- out$dist[[1]][1]
  } else {
    out <- unlist(out$dist)
  }
  
  return(out)
}





#' Calculate road density (in m2) around a feature
#'
#' @param feature Point feature (e.g., den)
#' @param feature_buffer Buffer size around point feature, in meters (default 1500 m)
#' @param roads Linestring feature containing roads
#' @param roads_buffer_col Column name in `roads` that contains road buffering distance (e.g., meters buffer for highways vs roads)
#' @param feaure_date_col (optional) Column name in `feature` that contains a date to use as the reference year for roads subsetting
#' @param filter_by_date (boolean) Should the function filter roads by construction date when calculating distance to feature? Default `TRUE`.
#' @param filter_by_year (boolean) If `filter_by_date` is `FALSE`, should the feature(s) instead be filtered by construction/retirement year, rather than a specific date? Default `FALSE`.
#' @param retirement_buffer Number of years by retirement date should be pushed back to still include in the roads filter. E.g., to include roads that were retired in 2013 in the calculation for a bear den that was visited in 2018, set `retirement_buffer = 5`.
#' @param return_road_area (boolean) Should the function return road areas or simply density of roads as a percentage? Default `TRUE`.
#'
#' @return
#' @export
st_road_density <- function(feature, 
                            feature_buffer = 1500, 
                            date_col = "date_inspected",
                            roads, 
                            roads_buffer_col = "buffer", 
                            filter_by_date = TRUE, 
                            filter_by_year = FALSE,
                            retirement_buffer,
                            return_road_area = TRUE) {
  # Data health checks
  stopifnot("`feature` must be a sf class geometry." = inherits(feature, "sf"))
  stopifnot("`feature` must be a sf class POINT geometry." = all(sf::st_geometry_type(feature) %in% 'POINT'))
  stopifnot("`roads` must be a sf class geometry." = inherits(roads, "sf"))
  #stopifnot("`roads` must be a sf class LINESTRING geometry." = all(sf::st_geometry_type(roads) %in% 'LINESTRING'))
  stopifnot("`feature` must be in the same CRS as `roads`." = sf::st_crs(feature) == sf::st_crs(roads))
  
  # Remove ferry routes
  roads <- roads[which(roads$fcode != "AQ10800000"),]
  
  # Assign road buffer distances
  # Highways vs. mainline roads vs. forestry sections will get 
  # a different amount of buffering.
  mainline <- c('DA25100190',
                'DA25100200',
                'DA25100210',
                'DA25100220',
                'DA25100350',
                'DA25100370',
                'DA25100380',
                'DA25100390',
                'DA25050180')
  
  roads <- roads |> 
    dplyr::mutate(buffer = dplyr::case_when(roads$fcode %in% mainline ~ 15,
                                              # For now, it's complex and time consuming to pull mainline roads vs forest roads
                                              #roads$file_type_description %in% "Forest Service Road" ~ 5,
                                              TRUE ~ 7.5))
  
  if (filter_by_date) {
    stopifnot("Can only evaluate distance to road for one feature at a time if `filter_by_date == TRUE`" = length(feature) == 1)
    sql_query <- query_roads(den_date = feature[[date_col]],
                             retirement_buffer = retirement_buffer)
    roads <- tidyquery::query(sql = sql_query)
  } else if (filter_by_year) { 
    # TODO: handle year vs day filter a bit better, somehow. It's confusing and error prone as-is.
    f_year <- unique(lubridate::year(feature[[date_col]]))
    stopifnot("Can only evaluate distance to road for one year at a time if `filter_by_year == TRUE`" = length(f_year) == 1)
    sql_query <- query_roads(den_date = paste0(as.character(f_year), "-12-31"),
                             retirement_buffer = retirement_buffer)
    roads <- tidyquery::query(sql = sql_query)
  }
  
  # Select roads that touch feature
  touches <- sf::st_is_within_distance(roads, feature, dist = feature_buffer, sparse = F)
  if (ncol(touches) > 1) {
    touches <- rowSums(touches)
    touches <- ifelse(touches == 0, FALSE, TRUE)
  }
  roads <- roads[touches,]
  
  # Buffer and union roads according to buffer col
  roads <- sf::st_buffer(roads, dist = roads[[roads_buffer_col]])
  roads <- sf::st_union(roads)
  
  # Buffer feature
  feature <- sf::st_buffer(feature, feature_buffer)
  
  # Intersect buffered roads with feature
  intxn <- suppressWarnings(sf::st_intersection(feature, roads))
  
  # Filter down to relevant cols
  intxn$year <- f_year
  intxn <- intxn[,c("den_id", "sample_id", "year")]
  
  # Calculate road area
  intxn$road_area <- sf::st_area(intxn)
  
  # Replace NA road areas with zero
  intxn$road_area <- ifelse(is.na(intxn$road_area), 0, intxn$road_area)
  
  # If user wants road area returned as well
  if (return_road_area) {
    out <- intxn
    out$total_area <- pi * (feature_buffer^2)
    out$road_density_m2 <- out$road_area / (pi * (feature_buffer^2))
    out <- sf::st_drop_geometry(out)
  } else {
    # Else just spit out the density values as a vector
    # Calculate density
    out <- intxn$road_area / (pi * (feature_buffer^2))
  }
  
  return(out)
  
}


#### TARGETS FUNCTIONS ####

# Functions here bundle up individual targets in the _targets.R file

merge_bcgw_lyrs <- function(bcgw_list) {
  out <- dplyr::bind_rows(bcgw_list)
  out <- janitor::clean_names(out)
  # TODO: re-run pipeline overnight with this change
  #sf::st_geometry(out) <- "geometry" 
  return(out)
}

load_depletions <- function(regions, 
                            path = "../../GIS/BC/Depletions/2023_Depletions.gpkg") {
  deps <- sf::st_read(path)
  deps <- sf::st_intersection(deps, regions)
  deps <- janitor::clean_names(deps)
  
  # Drop giant wkt column
  deps <- deps[,!(names(deps) %in% 'wkt')]
  
  return(deps)
}

# Runs all 4 forestry verification scripts in one go and
# outputs a dataframe of results.
verify_forestry <- function(feature, fvl, roads, 
                            year, retirement_buffer,
                            date_col = "date_inspected", 
                            id_col = "sample_id") {
  # Params check
  # Wishlist
  # TODO: handle year params better in this function/overall targets pipeline....
  if (year != 2024) stopifnot("The FVL year and supplied `year` do not match!" = unique(fvl$year) == year)
  # Subset `feature` to the correct year
  feature <- feature[which(lubridate::year(feature[[date_col]]) == year), ]
  if(nrow(feature) == 0) stop("There are no features in the year ", year)
  # Check if roads supplied
  if(missing(roads)) stop("`roads` is required")
  # Run the verifications
  # 01 Proportion forested within 60m
  message("Calculating proportion forested...")
  out_prop_forest <- st_proportion_forested(feature = feature, fvl = fvl)
  # 02 Distance to <40 yo forest
  message("Calculating distance to <40 yo forest...")
  out_lt40 <- unlist(suppressMessages(nngeo::st_nn(feature, fvl[fvl$forested == 'Non-Forested',], returnDist = T, progress = F)$dist))
  # 03 Distance to >40 yo forest
  message("Calculating distance to >40 yo forest...")
  out_gt40 <- unlist(suppressMessages(nngeo::st_nn(feature, fvl[fvl$forested == 'Forested',], returnDist = T, progress = F)$dist))
  # 04 Distance to nearest road
  message("Calculating distance to nearest road...")
  sql_query <- query_roads(den_date = paste0(as.character(year), "-12-31"),
                           retirement_buffer = retirement_buffer)
  roads <- tidyquery::query(sql = sql_query)
  out_dist_road <- st_distance_nearest_road(feature, roads, filter_by_date = FALSE)
  # Combine
  out <- data.frame(sample_id = feature[[id_col]],
                    prop_forest_60m = out_prop_forest,
                    dist_lt40 = out_lt40,
                    dist_gt40 = out_gt40,
                    dist_road = out_dist_road)
  return(out)
}


# Compare forestry verification results to raw data,
# provided those two datasets are supplied
compare_forestry_verifications <- function(orig_data,
                                           verification_results) {
  # Pare down to bare (hah) minimum number of columns needed, 
  # so inspecting the results is a bit easier
  orig_data <- orig_data[,c("den_id", "sample_id", 
                            "proportion_forested_field", "proportion_forested",
                            "distance_less40yr_forest_field", "v_distance_less40yr_forest",
                            "distance_grtr40yr_forest_field", "v_distance_grtr40year_forest",
                            "distance_nearest_road", "v_distance_nearest_road")]
  # Rename cols - orig_data
  names(orig_data) <- c("den_id", "sample_id", "raw_prop_forest_60m", "v_prop_forest_60m",
                        "raw_dist_lt40", "v_dist_lt40",
                        "raw_dist_gt40", "v_dist_gt40",
                        "raw_dist_road", "v_dist_road")
  # Rename cols - verification_results
  names(verification_results) <- paste0("new_", names(verification_results))
  # Merge
  f_v <- merge(orig_data, verification_results, by.x = "sample_id", by.y = "new_sample_id", all = TRUE)
  
  ## DIFFERENCE BTWN LEGACY VERIFICATIONS AND NEW VERIFICATIONS ##
  
  # > Proportion forested 60m
  # First round to nearest whole number
  f_v$new_prop_forest_60m <- round(f_v$new_prop_forest_60m * 100)
  # Take absolute difference
  f_v$prop_forest_60m_VDIFF <- abs(f_v$v_prop_forest_60m - f_v$new_prop_forest_60m)
  
  # > Distance to <40yo forest
  f_v$new_dist_lt40 <- round(f_v$new_dist_lt40)
  f_v$dist_lt40_VDIFF <- abs(f_v$v_dist_lt40 - f_v$new_dist_lt40)
  
  # > Distance to >40yo forest
  f_v$new_dist_gt40 <- round(f_v$new_dist_gt40)
  f_v$dist_gt40_VDIFF <- abs(f_v$v_dist_gt40 - f_v$new_dist_gt40)
  
  # > Distance to nearest_road
  f_v$new_dist_road <- round(f_v$new_dist_road)
  f_v$dist_road_VDIFF <- abs(f_v$v_dist_road - f_v$new_dist_road)
  
  
  ## DIFFERENCE BTWN RAW VALUES AND NEW VERIFICATIONS ##
  
  # > Proportion forested 60m
  f_v$prop_forest_60m_RAWDIFF <- abs(f_v$raw_prop_forest_60m - f_v$new_prop_forest_60m)
  
  # > Distance to <40yo forest
  f_v$dist_lt40_RAWDIFF <- abs(f_v$raw_dist_lt40 - f_v$new_dist_lt40)
  
  # > Distance to >40yo forest
  f_v$dist_gt40_RAWDIFF <- abs(f_v$raw_dist_gt40 - f_v$new_dist_gt40)
  
  # > Distance to nearest_road
  f_v$dist_road_RAWDIFF <- abs(f_v$raw_dist_road - f_v$new_dist_road)
  
  ## FLAGS ##
  
  # Flag any records that are >10m or 30% difference from raw/legacy verifications
  
  # Legacy verification value flags
  f_v$flag_legacy_prop_forest_60m <-  f_v$prop_forest_60m_VDIFF >= 30
  f_v$flag_legacy_dist_lt40 <- f_v$dist_lt40_VDIFF >= 10 | (f_v$dist_lt40_VDIFF/f_v$v_dist_lt40) >= 0.3
  f_v$flag_legacy_dist_gt40 <- f_v$dist_gt40_VDIFF >= 10 | (f_v$dist_gt40_VDIFF/f_v$v_dist_gt40) >= 0.3
  f_v$flag_legacy_dist_road <- f_v$dist_road_VDIFF >= 10 | (f_v$dist_road_VDIFF/f_v$v_dist_road) >= 0.3
  
  # Raw value flags
  f_v$flag_raw_prop_forest_60m <- f_v$prop_forest_60m_RAWDIFF >= 30
  f_v$flag_raw_dist_lt40 <- f_v$dist_gt40_RAWDIFF >= 10 | (f_v$dist_gt40_RAWDIFF/f_v$raw_dist_gt40) >= 0.3
  f_v$flag_raw_dist_gt40 <- f_v$dist_gt40_RAWDIFF >= 10 | (f_v$dist_gt40_RAWDIFF/f_v$raw_dist_gt40) >= 0.3
  f_v$flag_raw_dist_road <- f_v$dist_road_RAWDIFF >= 10 | (f_v$dist_road_RAWDIFF/f_v$raw_dist_road) >= 0.3
  
  
  ## CLEAN OUTPUT ##
  f_v <- f_v[,c("den_id", "sample_id", 
                "raw_prop_forest_60m", "v_prop_forest_60m", "new_prop_forest_60m", "prop_forest_60m_VDIFF", "prop_forest_60m_RAWDIFF",
                "flag_legacy_prop_forest_60m", "flag_raw_prop_forest_60m",
                "raw_dist_lt40", "v_dist_lt40", "new_dist_lt40", "dist_lt40_VDIFF", "dist_lt40_RAWDIFF",
                "flag_legacy_dist_lt40", "flag_raw_dist_lt40",
                "raw_dist_gt40", "v_dist_gt40", "new_dist_gt40", "dist_gt40_VDIFF", "dist_gt40_RAWDIFF",
                "flag_legacy_dist_gt40", "flag_raw_dist_gt40",
                "raw_dist_road", "v_dist_road", "new_dist_road", "dist_road_VDIFF", "dist_road_RAWDIFF",
                "flag_legacy_dist_road", "flag_raw_dist_road")]
  
  return(f_v)
}
  

# Summarize forestry verification comparison results
summarize_verifications <- function(f_v) {
  # Check how many fail the 10m or 30% difference cutoff...
  # Compare vs previous verifications
  out <- 
    data.frame(metric = c("prop_forest_60m",
                          "dist_lt40",
                          "dist_gt40",
                          "dist_road"),
               prct_legacy_verif_overlap = c(round(100 - sum(f_v$flag_legacy_prop_forest_60m, na.rm = TRUE) / nrow(f_v) * 100),
                                       round(100 - sum(f_v$flag_legacy_dist_lt40, na.rm = TRUE) / nrow(f_v) * 100),
                                       round(100 - sum(f_v$flag_legacy_dist_gt40, na.rm = TRUE) / nrow(f_v) * 100),
                                       round(100 - sum(f_v$flag_legacy_dist_road, na.rm = TRUE) / nrow(f_v) * 100)
                                       ))
  
  # Compare vs raw data
  out <- cbind(
    out,
    data.frame(prct_raw_overlap = c(round(100 - sum(f_v$flag_raw_prop_forest_60m, na.rm = TRUE) / nrow(f_v) * 100),
                                    round(100 - sum(f_v$flag_raw_dist_lt40, na.rm = TRUE) / nrow(f_v) * 100),
                                    round(100 - sum(f_v$flag_raw_dist_gt40, na.rm = TRUE) / nrow(f_v) * 100),
                                    round(100 - sum(f_v$flag_raw_dist_road, na.rm = TRUE) / nrow(f_v) * 100)
                                    ))
  )
  
  return(out)
}


# Rank den_ids in order of priority for checking and fixing manually
prioritize_den_checks <- function(f_v) {
  # Count the number of flags
  f_v$n_legacy_flags <- rowSums(f_v[, grep("flag_legacy", names(f_v))], na.rm = TRUE)
  f_v$n_raw_flags <- rowSums(f_v[, grep("flag_raw", names(f_v))], na.rm = TRUE)
  f_v$n_flags <- rowSums(f_v[, grep("^flag", names(f_v))], na.rm = TRUE)
  
  out <- aggregate(n_flags ~ den_id, f_v, FUN = "sum") |> 
    dplyr::filter(n_flags > 0) |> 
    dplyr::arrange(desc(n_flags))
  
  out <- out |> 
    dplyr::mutate(priority = dplyr::case_when(n_flags < 6 ~ "Low",
                                              n_flags >= 6 & n_flags < 15 ~ "Medium",
                                              n_flags >= 15 ~ "High",
                                              TRUE ~ NA))
  
  return(out)

}
