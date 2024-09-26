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


# > Roads ----

trans_roads <- read_lrg_wkt("GIS/HG_VI_roads.csv")
forest_roads <- read_lrg_wkt("GIS/HG_VI_forestry_sections.csv")

trans_roads <- janitor::clean_names(trans_roads)
forest_roads <- janitor::clean_names(forest_roads)

roads <- dplyr::bind_rows(trans_roads, forest_roads)
rm(trans_roads, forest_roads)

# Remove ferry routes
roads <- roads[which(roads$fcode != "AQ10800000"),]

# Buffer up!

# Highways vs. other roads will get a different amount of buffering.

hwy <- c("DA25100190",
         "DA25100200",
         "DA25100210",
         "DA25100220",
         "DA25100350",
         "DA25100370",
         "DA25100380",
         "DA25100390")

roads$hwy <- roads$fcode %in% hwy

# Now we want highways to be around 30m wide, and any other roads to be
# around 10m wide. The highways in this dataset are for the most part
# already 'double stranded' - that is, there is a line for each side of
# the highway. Therefore, we only need to buffer the highway lines by 
# 30 / 2 / 2, or 7.5m, to achieve a buffered highway width of 30m.
# Regular roads need to be buffered by 5m to achieve 10m of width.
roads$buffer <- ifelse(roads$hwy,
                       7.5,
                       5)

bbox <- data.frame(x = c(1141541.2, 1142383.1),
                   y = c(470259.0, 470887.6)) |>
  sf::st_as_sf(coords = c("x", "y")) |>
  sf::st_set_crs(3005) |>
  sf::st_bbox() |>
  sf::st_as_sfc()
  
test <- sf::st_intersection(roads, bbox)
test <- sf::st_buffer(test, dist = test$buffer)
plot(test['hwy']) # looks good 
test <- sf::st_union(test)
plot(test) # also looks good
rm(test, bbox)

# JK, do it differently
# TODO: intersect in 1.5 km radius, subset to date, then buffer roads

# Surprisingly quick
roads <- sf::st_buffer(roads, dist = roads$buffer)
roads <- sf::st_union(roads) # 4 mins
beepr::beep()

# Buffer bear dens by 1.5 km
# If we're ignoring the road dates, we can just use the dens df
d <- sf::st_buffer(dens, 1500)

# Intersect w roads
d_r <- sf::st_intersection(d, roads)
d_r$road_area <- sf::st_area(d_r)

# Merge back with buffered dens
d <- merge(d, sf::st_drop_geometry(d_r[,c("den_id", "road_area")]), all.x = TRUE)
d$radius_area <- sf::st_area(d)

# Replace NA road areas with zero
d$road_area <- ifelse(is.na(d$road_area), 0, d$road_area)

# Calculate density
d$road_density_m2 <- d$road_area / d$radius_area

d |> 
  dplyr::select(den_id, road_area, radius_area, road_density_m2) |>
  sf::st_drop_geometry() |>
  write.csv("temp/road_density_20240918.csv", na = "", row.names = F)

rm(d, d_r, roads)


#### ROAD DENSITY BY YEAR ####

# If you wanted to subset it by year, follow similar methodology
# as distance to nearest road

# > Re-load Roads ----

trans_roads <- read_lrg_wkt("GIS/HG_VI_roads.csv")
forest_roads <- read_lrg_wkt("GIS/HG_VI_forestry_sections.csv")

trans_roads <- janitor::clean_names(trans_roads)
forest_roads <- janitor::clean_names(forest_roads)

roads <- dplyr::bind_rows(trans_roads, forest_roads)
rm(trans_roads, forest_roads)

# Remove ferry routes
roads <- roads[which(roads$fcode != "AQ10800000"),]

# Buffer up!
# Highways vs. other roads will get a different amount of buffering.
roads$hwy <- roads$fcode %in% hwy
roads$buffer <- ifelse(roads$hwy, 7.5, 5)

# Start 10:44- 11:00 am
densities <- lapply(1:nrow(f_full), function(x){
  tryCatch({
    f_x <- f_full[x,]
    message("Calculating road density for ", f_x$sample_id, " (", round((x / nrow(f_full))*100, 2), "% done)")
    out <- road_density_buffer(feature = f_x, 
                               roads = roads,
                               return_road_area = TRUE)
    #out <- data.frame(sample_id = f_x$sample_id, road_density = out)
    out <- as.data.frame(out)
    out$sample_id <- f_x$sample_id
    out <- out[,c(4,1:3)] # re-order cols
    return(out)
    }, error = function(e) {
      message("Error with ", f_full[["sample_id"]][x])
    })
  })
beepr::beep()
Sys.time()

densities <- dplyr::bind_rows(densities)

densities <- densities[order(densities$sample_id),]

# Reorder cols to match the other densities csv
names(densities)[4] <- "radius_area"
densities <- densities[,c("sample_id", "road_area", "radius_area", "road_density_m2")]

write.csv(densities, "temp/road_density_yearly_20240918.csv",
          na = "",
          row.names = F)
