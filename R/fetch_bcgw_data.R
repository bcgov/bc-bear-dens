# BCGW GIS scripts

# Pull data from the BCGW database

# A few things of note:

# 1. You need to have the Oracle JDBC driver downloaded 
#     and installed on your machine. 
# 2. You should increase the RAM allocated to JDBC processes
#     to speed up data downloads.
# 3. ENSURE YOU ARE CONNECTED TO THE VPN! Can't connect to 
#     the db without it.
# 4. For the {targets} pipeline specifically, you need to
#     manually set your BCGW keys first by running `bcgw_set_keys()`.
# 5. For the {targets} pipeline specifically, you need to 
#     connect and disconnect from the database within each
#     fetching function separately.


# Wishlist
# TODO: Write tests that confirm whether JDBC driver properly
#       connected e.g. this should fail:
#       RJDBC::JDBC("oracle.jdbc.driver.OracleDriver", "C://Users//SPOPOV//Oracle//some//random//crap.jar")
# TODO: test that polygon can successfully be queried from BCGW db


#### DATABASE CONNECTION FXNS ####

#' Connect to Oracle JDBC drivers
#'
#' @param ram_allocation Amount of RAM to allocate to Java processes for future database queries. Default is 16GB
#' @param driver_path Path on local machines to JDBC drivers. Accepts multiple arguments as a vector (e.g., `c("first/path/ojdbc.jar", "second/path/xdb.jar")`). 
#' Note that to succesfully pull spatial data from the BCGW database, you need to specify both the location of `ojdbc8.jar` **and** `xdb.jar`.
#'
#' @return On success, a RJDBC driver object
#' @export
prepare_jdbc <- function(ram_allocation = 16,
                         driver_path = c("C://Users//SPOPOV//Oracle//instantclient_23_4//ojdbc8.jar",
                                         "C://Users//SPOPOV//Oracle//instantclient_23_4//xdb.jar")) {
  stopifnot(is.numeric(ram_allocation))
  # Allocate RAM to Java processes
  jdbc_ram <- paste0("-Xmx", ram_allocation, "g")
  options(java.parameters = c("-XX:+UseConcMarkSweepGC", jdbc_ram))
  # Load JDBC drivers
  # For some reason, you need to run both these lines? Otherwise pulling
  # spatial data fails from BCGW
  for (path in driver_path) {
    # JDBC fxn handles the error if an incorrect `path` is supplied
    drv <- RJDBC::JDBC("oracle.jdbc.driver.OracleDriver", path)
  }
  # Check it's loaded properly
  rjdbc_out <- RJDBC::findDrivers()
  if (length(rjdbc_out) == 0) { 
    # In theory this should be handled by JDBC fxn, but 
    # I have in here as a failsale regardless
    error("Failed to connect to JDBC drivers.")
  } else {
    return(drv)
  }
}


bcgw_set_keys <- function(username = "SPOPOV", change_pw = FALSE) {
  # Only set the keys if they don't exist yet OR change_pw == TRUE
  if (nrow(keyring::key_list(service = "BCGW-keys")) == 0 | change_pw) {
    keyring::key_set(service = "BCGW-keys",
                     username = username,
                     prompt = "BCGW password: ") 
  }
}

connect_bcgw <- function(drv) {
  bcgw_username <- keyring::key_list(service = "BCGW-keys")$username
  conn <- RJDBC::dbConnect(drv,
                           "jdbc:oracle:thin:@bcgw.bcgov/idwprod1.bcgov",
                           # Use keyring to pull credentials from local machine
                           bcgw_username,
                           keyring::key_get("BCGW-keys", username = bcgw_username))
  return(conn)
}

test_bcgw <- function() {
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Test - get that forest age polygon!!
  test_poly <- DBI::dbGetQueryArrow(conn, "SELECT GEOMETRY,
                                FEATURE_ID, BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3,
                                BCLCS_LEVEL_4, BCLCS_LEVEL_5, FOR_MGMT_LAND_BASE_IND,
                                PROJECTED_DATE, EARLIEST_NONLOGGING_DIST_TYPE,
                                EARLIEST_NONLOGGING_DIST_DATE, HARVEST_DATE, REFERENCE_DATE,
                                PROJ_AGE_1, PROJ_AGE_CLASS_CD_1,
                                PROJ_AGE_2, PROJ_AGE_CLASS_CD_2
                               FROM WHSE_FOREST_VEGETATION.VEG_COMP_LYR_R1_POLY 
                               WHERE PROJ_AGE_1 IS NOT NULL
                                  fetch next 10 rows only")
  test_poly <- as.data.frame(test_poly)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(test_poly)
}

# Read in regions
read_regions <- function() {
  regions <- sf::st_read("GIS/regions_wkt.csv")
  sf::st_crs(regions) <- 3005
  return(regions)
}


#### QUERY VRI ####

# Query BCGW VRI table - Haida Gwaii
query_hg_vri <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  spatial_query <- paste0("SELECT FEATURE_ID, BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3,
                                BCLCS_LEVEL_4, BCLCS_LEVEL_5, FOR_MGMT_LAND_BASE_IND,
                                PROJECTED_DATE, EARLIEST_NONLOGGING_DIST_TYPE,
                                EARLIEST_NONLOGGING_DIST_DATE, HARVEST_DATE, REFERENCE_DATE,
                                PROJ_AGE_1, PROJ_AGE_CLASS_CD_1,
                                PROJ_AGE_2, PROJ_AGE_CLASS_CD_2,
                                SDO_UTIL.TO_WKTGEOMETRY(geometry) as WKT_GEOM
                               FROM WHSE_FOREST_VEGETATION.VEG_COMP_LYR_R1_POLY 
                               WHERE PROJ_AGE_1 IS NOT NULL
                               AND sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == "HG"], "', 3005)
                               ) = 'TRUE'")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
  out_poly <- as.data.frame(out_poly) # this sort of defeats the purpose of arrow, but can't figure out how to make it play nice with targets atm
  # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(out_poly)
}


# Query BCGW VRI table - Vancouver Island
query_vi_vri <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  spatial_query <- paste0("SELECT FEATURE_ID, BCLCS_LEVEL_1, BCLCS_LEVEL_2, BCLCS_LEVEL_3,
                                BCLCS_LEVEL_4, BCLCS_LEVEL_5, FOR_MGMT_LAND_BASE_IND,
                                PROJECTED_DATE, EARLIEST_NONLOGGING_DIST_TYPE,
                                EARLIEST_NONLOGGING_DIST_DATE, HARVEST_DATE, REFERENCE_DATE,
                                PROJ_AGE_1, PROJ_AGE_CLASS_CD_1,
                                PROJ_AGE_2, PROJ_AGE_CLASS_CD_2,
                                SDO_UTIL.TO_WKTGEOMETRY(geometry) as WKT_GEOM
                               FROM WHSE_FOREST_VEGETATION.VEG_COMP_LYR_R1_POLY 
                               WHERE PROJ_AGE_1 IS NOT NULL
                               AND sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == "VI"], "', 3005)
                               ) = 'TRUE'")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
  out_poly <- as.data.frame(out_poly) # this sort of defeats the purpose of arrow, but can't figure out how to make it play nice with targets atm
  # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(out_poly)
}


#### QUERY BASEMAPPING ROADS ####

# Base Mapping - Transportation Roads

query_basemapping_roads <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  spatial_query <- paste0("SELECT OBJECTID, FCODE, 
                         SDO_UTIL.TO_WKTGEOMETRY(geometry) as WKT_GEOM 
                         from WHSE_BASEMAPPING.TRIM_TRANSPORTATION_LINES
                         WHERE sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'HG'], "', 3005)
                               ) = 'TRUE'
                        OR sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'VI'], "', 3005)
                               ) = 'TRUE'")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
  out_poly <- as.data.frame(out_poly) # this sort of defeats the purpose of arrow, but can't figure out how to make it play nice with targets atm
  # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(out_poly)
}



#### QUERY FORESTRY ROADS ####

# We want *sections* over road *segments* because the sections table contains both 
# road cut start date and end date.

query_forestry_roads <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  spatial_query <- paste0("SELECT OBJECTID, FOREST_FILE_ID, ROAD_SECTION_ID, ROAD_SECTION_NAME,
                         AWARD_DATE, EXPIRY_DATE, RETIREMENT_DATE, FILE_TYPE_DESCRIPTION,
                         GEOGRAPHIC_DISTRICT_NAME, CLIENT_NUMBER, CLIENT_NAME, LOCATION,
                         LIFE_CYCLE_STATUS_CODE, MAP_LABEL, 
                         SDO_UTIL.TO_WKTGEOMETRY(geometry) as WKT_GEOM 
                         from WHSE_FOREST_TENURE.FTEN_ROAD_SECTION_LINES_SVW
                         WHERE sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'HG'], "', 3005)
                               ) = 'TRUE'
                        OR sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'VI'], "', 3005)
                               ) = 'TRUE'")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
  out_poly <- as.data.frame(out_poly) # this sort of defeats the purpose of arrow, but can't figure out how to make it play nice with targets atm
  # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(out_poly)
}


#### PRIVATE LAND AREAS ####

# This will be used to throw a warning for bear dens that are close to 
# private forestry land, so forestry metrics may be inaccurate

query_private_land <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  spatial_query <- paste0("SELECT OWNERSHIP_DESCRIPTION, SDO_UTIL.TO_WKTGEOMETRY(geometry) as WKT_GEOM
                          FROM WHSE_FOREST_VEGETATION.F_OWN fo
                          WHERE OWNERSHIP_DESCRIPTION = 'Private' 
                          AND (sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'HG'], "', 3005)
                               ) = 'TRUE'
                        OR sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'VI'], "', 3005)
                               ) = 'TRUE')")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
    # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  # Then merge into single polygon
  private_lands <- as.data.frame(out_poly)
  private_lands <- sf::st_as_sf(private_lands, wkt = "WKT_GEOM")
  sf::st_crs(private_lands) <- 3005
  # Spatially aggregate
  private_lands <- private_lands |> 
    dplyr::group_by(OWNERSHIP_DESCRIPTION) |> 
    dplyr::summarise()
  return(private_lands)
}



#### QUERY PARK LANDS ####

# Base Mapping - Provincial/Tantalis 'All' Parks

query_tantalis_parks <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  # NOTE in this table, the geometry column is called 'shape'!
  spatial_query <- paste0("SELECT PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION, 
                         SDO_UTIL.TO_WKTGEOMETRY(shape) as WKT_GEOM 
                         from WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW
                         WHERE sdo_anyinteract(shape,
                               sdo_geometry('", regions[['WKT']][regions$region == 'HG'], "', 3005)
                               ) = 'TRUE'
                        OR sdo_anyinteract(shape,
                               sdo_geometry('", regions[['WKT']][regions$region == 'VI'], "', 3005)
                               ) = 'TRUE'")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
  out_poly <- as.data.frame(out_poly) # this sort of defeats the purpose of arrow, but can't figure out how to make it play nice with targets atm
  # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(out_poly)
}



# Base Mapping - Federal Parks

query_federal_parks <- function(regions) {
  # Function health checks
  stopifnot("`regions` must be an `sf` object with POLYGON geometery." = all(sf::st_is(regions, "POLYGON")))
  # Connect to db
  drv <- prepare_jdbc()
  conn <- connect_bcgw(drv)
  # Create query
  spatial_query <- paste0("SELECT NATIONAL_PARK_ID, ENGLISH_NAME, 
                         SDO_UTIL.TO_WKTGEOMETRY(geometry) as WKT_GEOM 
                         from WHSE_ADMIN_BOUNDARIES.CLAB_NATIONAL_PARKS
                         WHERE sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'HG'], "', 3005)
                               ) = 'TRUE'
                        OR sdo_anyinteract(geometry,
                               sdo_geometry('", regions[['WKT']][regions$region == 'VI'], "', 3005)
                               ) = 'TRUE'")
  # Execute query
  out_poly <- DBI::dbGetQueryArrow(conn, spatial_query)
  out_poly <- as.data.frame(out_poly) # this sort of defeats the purpose of arrow, but can't figure out how to make it play nice with targets atm
  # Exit
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))
  return(out_poly)
}

