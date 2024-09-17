
# Calculate % forest age class polygons within 1.5km of den

#### SET UP ####

# Recommended to first allocate more memory to R, then restart R.
# R_MAX_VSIZE=32Gb
usethis::edit_r_environ()

# Set token
# Token seems to expire anytime your ArcGIS Online session expires -
# seems to be roughly ~30 mins.
# Even though it expires super quickly, better practice to isolate token
# value in some file that's not tracked by git
source("temp/token.R")

# Load functions
source("R/fetch_agol_data.R")
source("R/clean_bear_data.R")
source("R/forestry_gis.R")

# Fetch bear data

# `dens` for 'current' data
dens <- fetch_bears(token = token,
                    layer = "current")

# `f` for 'field visits' data
f <- fetch_bears(token = token,
                 layer = "field visits")

# No backups here, as this data won't go onto AGOL

#### GENERAL CLEANUP ####
dens <- clean_bears(dens)
f <- clean_bears(f)

dens <- sf::st_transform(dens, 3005)

#### MERGE ####

# 2024-09-12 For now, drop SAN_FishermanRiver_1 because it's duplicated
# in the dens table.
dens <- dens[dens$den_id != "SAN_FishermanRiver_1",]
f <- f[f$den_id != "SAN_FishermanRiver_1",]

# Merge and throw warning if the number of records changes (this indicates
# duplicated den IDs in the dens table).
f_full <- merge(dens, f, by = "den_id")

if (nrow(f) != nrow(f_full)) stop("WARNING: You have duplicated den IDs in the dens table!!")

#### LOAD GIS LAYERS ####

# Define function to read in large WKT csv files
read_lrg_wkt <- function(filepath, wkt_col = "WKT_GEOM", crs = 3005) {
  x <- arrow::read_csv_arrow(filepath)
  x <- as.data.frame(x)
  x <- sf::st_as_sf(x, wkt = wkt_col)
  sf::st_crs(x) <- crs
  # Maybe drop the giant WKT column - don't need it anymore?
  return(x)
}

# > VRI ----

vri_hg <- read_lrg_wkt("GIS/HG_VEG_COMP_LYR_L1_POLY_202409060839.csv")
vri_vi <- read_lrg_wkt("GIS/arrow_VI_VEG_COMP_LYR_R1_POLY_202409061234.csv")

vri <- dplyr::bind_rows(vri_hg, vri_vi)

rm(vri_hg, vri_vi)
gc()

vri <- janitor::clean_names(vri)

vri <- vri[,c("feature_id", "proj_age_1", "projected_date")]

# > Depletions ----

regions <- sf::st_read("GIS/regions_wkt.csv")
sf::st_crs(regions) <- 3005

deps <- sf::st_read("../../GIS/BC/Depletions/2023_Depletions.gpkg")
deps <- sf::st_intersection(deps, regions)
deps <- janitor::clean_names(deps)

# Drop giant wkt column
deps <- deps[,!(names(deps) %in% 'wkt')]

rm(regions)

deps <- deps[,c("objectid", "depletion_year")]


#### CALCULATE AGE CLASS AMT ####

# Start - 1:03 pm-1:34 pm
percentages <- lapply(1:nrow(f_full), function(x) {
  tryCatch({
    message("Calculating % age class for ", f_full[["sample_id"]][x], " (", round((x / nrow(f_full))*100, 2), "% done)")
    out <- prct_age_class_buffer(feature = f_full[x,], vri = vri, depletions = deps)
    out$sample_id <- f_full[["sample_id"]][x]
    return(out)
    }, 
    error = function(e) {
      message("Error with ", f_full[["sample_id"]][x])
    })
  })
beepr::beep()
Sys.time()

percentages <- dplyr::bind_rows(percentages)
percentages <- percentages |> 
  dplyr::group_by(sample_id) |>
  dplyr::mutate(forest_area = sum(area))
percentages$total_area <- sum(sf::st_area(sf::st_buffer(f_full[1,], 1500)))

percentages$prct_total_area <- units::drop_units(percentages$area / percentages$total_area)

write.csv(percentages, "temp/age_class_areas_20240917.csv",
          na = "",
          row.names = FALSE)

# Pivot wider
percentages2 <- 
  percentages |> 
  dplyr::mutate(prct_area = round(prct_area * 100)) |>
  tidyr::pivot_wider(id_cols = sample_id,
                     names_from = age_class,
                     names_prefix = "age_class_",
                     values_from = prct_area, #area,
                     values_fn = mean # ideally delete this - issue caused by duplicate sample IDs 
                     )

write.csv(percentages2, "temp/age_class_percentages_20240917.csv",
          na = "",
          row.names = FALSE)
