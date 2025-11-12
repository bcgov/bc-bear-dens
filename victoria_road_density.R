# Road density from a random point in downtown Victoria

pt <- sf::st_point(c(-123.3398819, 48.4380405)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_transform(crs = 3005) |>
  sf::st_as_sf() |>
  dplyr::mutate(date_inspected = "2025-11-12") |> # add dummy date col
  dplyr::mutate(date_inspected = lubridate::as_datetime(date_inspected))
sf::st_geometry(pt) <- "geometry"

pt

tar_load(roads)
sf::st_bbox(roads)

# Also load up the following fxns from `forestry_gis.R`:
# query_roads()
# st_road_density()
st_road_density(pt, 
                roads = roads, 
                filter_by_date = FALSE, 
                filter_by_year = FALSE,
                retirement_buffer = 10)
