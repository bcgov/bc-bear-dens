# Functions to generate random samples on the study area
# landscape to compare to our actual sample

generate_study_area <- function(fvl = FVL_2024,
                                private_land = hg_vi_private_land) {
  # Given a FVL polygon and a polygon of private land area,
  # create a polygon of our study area extent.
  # Dissolve all boundaries in FVL
  fvl <- sf::st_union(fvl)
  
  # Remove private land
  study_area <- sf::st_difference(fvl, private_land)
  
  # Clean up 
  study_area <- sf::st_as_sf(study_area)
  return(study_area)
}

generate_random_dens <- function(study_area, years, n_dens, random_seed = 24) {
  # Set seed
  # So we consistently regenerate the same random dens each 
  # time this code is re-run. Otherwise pipeline will constantly 
  # be re-triggered
  set.seed(random_seed) 
  
  # Generate random points within study_area polygon
  pseudo_dens <- sf::st_sample(study_area, size = n_dens)
  pseudo_dens <- sf::st_as_sf(pseudo_dens)
  
  # Create pseudo-den_id
  pseudo_dens$den_id <- paste0("RANDOM_", row.names(pseudo_dens))
  
  # Create pseudo field visits
  pseudo_visits <- expand.grid(den_id = pseudo_dens$den_id, 
                               year = years)
  
  # Merge pseudo dens with pseudo field visits
  pseudo_dens <- merge(pseudo_dens, pseudo_visits)
  
  # Just pretend every den is visited on Sept 1 (not that it matters,
  # we're only tracking forestry data by the year, not date)
  pseudo_dens$date_inspected <- lubridate::as_date(paste0(pseudo_dens$year, "-09-01"))
  
  # Generate sample_id
  # Important so that % forested functions work, which rely on sample_id
  # to correctly merge results back to original data
  pseudo_dens$sample_id <- paste0(pseudo_dens$den_id, "_", pseudo_dens$year, "0901")
  
  return(pseudo_dens)
  
} 