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
  
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
  memory = "transient", # unload memory for each target line after it's completed
  garbage_collection = TRUE, # run gc() prior to each target
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# API tokens
source("temp/token.R")

fvl_years <- data.frame(years = c(2014, 2015, 2016, 2017, 2018, 2020, 2021, 2022, 2023, 2024))

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
  tar_target(den_years, pull_den_years(f)), # In this case, not using it for the static FVL tar_map() function. Instead using the manually created `fvl_years` df definted outside the pipeline.
  tar_target(vri, merge_bcgw_lyrs(bcgw_list = list(hg_vri, vi_vri)) |>
               sf::st_as_sf(wkt = "wkt_geom", crs = 3005)),
  tar_target(deps, load_depletions(regions = regions)),
  tar_target(roads, merge_bcgw_lyrs(bcgw_list = list(hg_vi_roads, hg_vi_forestry_sections)) |>
               sf::st_as_sf(wkt = "wkt_geom", crs = 3005)),
  # Actually create FVLs (will take ~5-6 hours)
  # Wishlist: organize the pipeline to track each yearly VRI 
  # and yearly depletion layers, so that the FVL is only re-created
  # if the underlying VRI and depletion layer is updated.
  # The low number of FVL years means that static branching might be
  # a better fit here. 
  mapped <- tar_map(
    values = fvl_years, # params need to be passed as a df/tibble, defined OUTSIDE the pipeline
    # Create FVLs for each year
    tar_target(FVL, 
               create_fvl(den_year = years, # `years` in this case referes to the `years` column in `fvl_years` df
                          vri = vri,
                          depletions = deps)
               ),
    # Run forestry verification algorithms (% forested 60m, dist <40yo forest, dist >40yo forest, dist road) for each year
    tar_target(forestry_verification,
               verify_forestry(feature = f_full,
                               fvl = FVL, # referring to the target `FVL` created in the previous step
                               roads = roads,
                               year = years, # `years` in this case referes to the `years` column in `fvl_years` df
                               ))
    # TODO: % age class around each den
    # TODO: road density around each den
    ),
  # Combine all the fruits of our labor into one df!
  tar_combine(forestry_verifications_full,
              mapped[[2]],
              command = dplyr::bind_rows(!!!.x)),
  # Data QC
  # Non-forestry column QC checks
  tar_target(nonforest_qc, verify_bears(f)),
  # Compare forestry verifications to legacy verifications and raw data
  tar_target(forest_qc, compare_forestry_verifications(orig_data = f, verification_results = forestry_verifications_full)),
  tar_target(forest_qc_summary, summarize_verifications(f_v = forest_qc))
  # TODO: fxns to rank dens in order of manual checking priority
)
