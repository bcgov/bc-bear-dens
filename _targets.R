# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("qs",
               "RJDBC",
               "keyring",
               "DBI",
               "arrow",
               "sf"), # Packages that your targets need for their tasks.
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

# Run tar_make() to execute the pipeline
list(
  #tar_target(bcgw_keys, bcgw_set_keys()), # need a better way to handle this... if the keys aren't set, the pipeline will fail
  tar_target(test_poly, test_bcgw()),
  tar_target(regions, read_regions()),
  tar_target(hg_vri, query_hg_vri(regions)),
  tar_target(vi_vri, query_vi_vri(regions)),
  tar_target(hg_vi_roads, query_basemapping_roads(regions)),
  tar_target(hg_vi_forestry_sections, query_forestry_roads(regions)),
  tar_target(hg_vi_private_land, query_private_land(regions)),
  #tar_target(hg_vi_private_land, sf::st_as_sf(tar_read(hg_vi_private_land))) # If you wanted to then use the sf later in the pipeline
)
