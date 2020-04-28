## code to prepare default CRS (coord ref systems) goes here


#two map projections
#54030 is Robinson
crs_Atlantic <- 54030
#same as Robinson, but centred on long +150
crs_Pacific <- sp::CRS("+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

usethis::use_data(crs_Atlantic)
usethis::use_data(crs_Pacific)
