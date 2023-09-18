## code to prepare default CRS (coord ref systems) goes here

# (default) crs that uses lat and long
# useful for converting matrices of long-lat to spatial
crs_longlat <- "EPSG:4326"

#two map projections
#54030 is not in the standard EPSG database, so define Robinson
crs_Atlantic <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#same as Robinson, but centred on long +180
crs_Pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#same as Robinson, but centred on long +120
crs_120E <- "+proj=robin +lon_0=120 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# polar options
# WGS 84 / Arctic Polar Stereographic
# this is inconsistent with the others, but st_crs(3995) has a non-ascii character which CRAN does not like
crs_N <- "EPSG:3995"
#WGS 84 / Antarctic Polar Stereographic
crs_S <- "EPSG:3031"

usethis::use_data(crs_longlat,
                  crs_Atlantic, crs_Pacific,
                  crs_120E,
                  crs_N, crs_S, overwrite = TRUE)

