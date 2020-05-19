## code to prepare default CRS (coord ref systems) goes here

# (default) crs that uses lat and long
# useful for converting matrices of long-lat to spatial
crs_latlong <- 4326

#two map projections
#54030 is Robinson
crs_Atlantic <- 54030
#same as Robinson, but centred on long +150
crs_Pacific <- sp::CRS("+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# polar options
# WGS 84 / Arctic Polar Stereographic
crs_N <- 3995
# WGS 84 / Antarctic Polar Stereographic
crs_S <- 3031

usethis::use_data(crs_latlong,
                  crs_Atlantic, crs_Pacific,
                  crs_N, crs_S, overwrite = TRUE)

