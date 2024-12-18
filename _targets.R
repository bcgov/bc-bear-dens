# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("qs",
               "RJDBC",
               "keyring",
               "DBI",
               "arrow",
               "sf",
               "lubridate"), # Packages that your targets need for their tasks.
  format = "qs", # Optionally set the default storage format. qs is fast.
  
  # Set other options as needed.
  memory = "transient", # unload memory for each target line after it's completed
  garbage_collection = TRUE, # run gc() (clean up your RAM) prior to each target
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# SET MANUAL VARIABLES 
# Load API tokens
source("temp/token.R")

# Provide a list of years to run each forestry verification on
# While we could automatically pull that from the den data, it
# is simpler to just manually supply a list of years to the pipeline
# (aka 'static branching') rather than have the pipeline extract the 
# years itself (aka 'dynamic branching')
fvl_years <- data.frame(years = c(2014, 2015, 2016, 2017, 2018, 2020, 2021, 2022, 2023, 2024))

# Add a retirement buffer to the road verifications - i.e.,
# how many years after the retirement date should be added to keep forestry road sections in?
retirement_buffer <- 5 # 5 years

# Run tar_make() to execute the pipeline
list(
  #tar_target(bcgw_keys, bcgw_set_keys()), # need a better way to handle this... if the keys aren't set, the pipeline will fail
  # Query BCGW
  tar_target(test_poly, test_bcgw()),
  tar_target(regions, read_regions()),
  tar_target(hg_vri, query_hg_vri(regions)),
  tar_target(vi_vri, query_vi_vri(regions)),
  tar_target(hg_vi_roads, query_basemapping_roads(regions)),
  tar_target(hg_vi_forestry_sections, query_forestry_roads(regions)),
  tar_target(hg_vi_private_land, query_private_land(regions)),
  # Query AGOL
  tar_target(dens_raw, fetch_bears(token = token, layer = "current")),
  tar_target(f_raw, fetch_bears(token = token, layer = "field visits")),
  tar_target(p_raw, fetch_bears(token = token, layer = "potential")),
  tar_target(backup, backup_bears(dens_raw, f_raw, p_raw)), # Even if token changes, if dens, f, and p don't change, it won't create a backup!
  # Clean AGOL (keep separate from raw so as to save original colnames & data, in case cleaning causes data issue)
  tar_target(dens, clean_bears(dens_raw)),
  tar_target(f, clean_bears(f_raw)),
  tar_target(p, clean_bears(p_raw)),
  # Create f_full
  tar_target(f_full, sf::st_as_sf(merge(f, dens, by = "den_id")) |> sf::st_transform(3005)),
  # Prepare GIS layers for FVL creation
  tar_target(vri, merge_bcgw_lyrs(bcgw_list = list(hg_vri, vi_vri)) |>
               sf::st_as_sf(wkt = "wkt_geom", crs = 3005)),
  tar_target(deps, load_depletions(regions = regions)),
  tar_target(roads, merge_bcgw_lyrs(bcgw_list = list(hg_vi_roads, hg_vi_forestry_sections)) |>
               sf::st_as_sf(wkt = "wkt_geom", crs = 3005)),
  # Actually create FVLs (will take ~5-6 hours)
  # TODO wishlist item: organize the pipeline to track each yearly VRI 
  # and yearly depletion layers, so that the FVL is only re-created
  # if the underlying VRI and depletion layer is updated.
  # Pull the lat/long data from each den, to run the verifications on
  tar_target(f_geom, f_full |> # Create an object that is JUST sample_id + sf geometry to run the verifications on. Otherwise, this pipeline gets triggered each time there's a simple data change to any of the text columns.
               dplyr::mutate(year = lubridate::year(date_inspected)) |> 
               dplyr::select(den_id, sample_id, date_inspected)), 
  # Run tar_map() - i.e., for each year as defined above in `fvl_years`,
  # run the following 4 functions: create_fvl(), verify_forestry(), 
  # st_proportion_age_class(), and st_road_buffer()
  mapped <- tar_map(
    values = fvl_years, # params need to be passed as a df/tibble, defined OUTSIDE the pipeline
    # Create FVLs for each ayear
    tar_target(FVL,
               create_fvl(den_year = years, # `years` in this case refers to the `years` column in `fvl_years` df
                          vri = vri,
                          depletions = deps)
               ),
    # Run forestry verification algorithms (% forested 60m, dist <40yo forest, dist >40yo forest, dist road) for each year
    tar_target(forestry_verification,
               verify_forestry(feature = f_geom,
                               fvl = FVL, # referring to the target `FVL` created in the previous step
                               roads = roads,
                               year = years, # `years` in this case refers to the `years` column in `fvl_years` df
                               retirement_buffer = retirement_buffer, # years buffer to add to road retirement date to still include recently retired, but still driveable, roads in verification checks
                               )),
    # % age class around each den
    tar_target(prct_age_class_yearly,
               st_proportion_age_class(feature = f_geom[lubridate::year(f_geom$date_inspected) == years, ], # `years` in this case refers to the `years` column in `fvl_years` df
                                       buffer = 1500,
                                       vri = vri,
                                       depletions = deps)),
    # Road density around each den
    tar_target(road_density_yearly,
               st_road_density(feature = f_geom[lubridate::year(f_geom$date_inspected) == years, ],
                               roads = roads,
                               filter_by_date = FALSE,
                               filter_by_year = TRUE,
                               retirement_buffer = retirement_buffer))
    ),
  # Combine all the fruits of our labor into one df!
  tar_combine(forestry_verifications_full,
              mapped[[2]], # merge the second item in `mapped` ('forestry_verification') into one df 
              command = dplyr::bind_rows(!!!.x)),
  tar_combine(prct_age_class_1.5km,
              mapped[[3]], # merge the third item in `mapped` ('prct_age_class_yearly') into one df
              command = dplyr::bind_rows(!!!.x) |> dplyr::arrange(den_id, year)),
  tar_combine(road_density,
              mapped[[4]], # merge the fourth item in `mapped` ('road_density_yearly') into one df
              command = dplyr::bind_rows(!!!.x) |> dplyr::arrange(den_id, year)),
  # Data QC
  # Non-forestry column QC checks
  tar_target(nonforest_qc, verify_bears(dens, f)),
  # Compare forestry verifications to legacy verifications and raw data
  tar_target(forest_qc, compare_forestry_verifications(orig_data = f, verification_results = forestry_verifications_full)),
  # Summarize forestry verification results
  tar_target(forest_qc_summary, summarize_verifications(f_v = forest_qc)),
  # Organize the dens into a fix priority list based on the number of flags it tripped
  tar_target(den_fix_priority, prioritize_den_checks(f_v = forest_qc))
  # Summary statistics
  # For now, these summary stats scripts live in the "Data summary" folder
  # and haven't been incorporated into the targets pipeline directly.
  # TODO: move stats from other script to here
  # Analysis
  
)
