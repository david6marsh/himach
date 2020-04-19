library(testthat)
library(twospeed)

test_that("Grid creation works", {
  crs_Pacific <- sp::CRS("+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  # We project the in-built test maps
  NZ_buffer_Pac <- sf::st_transform(twospeed::NZ_buffer30, crs=crs_Pacific)

  expect_known_output(make_route_grid(NZ_buffer_Pac,"NZ lat-long at 300km",
                                     target_km = 300, classify = TRUE,
                                     lat_min = -49, lat_max = -32,
                                     long_min = 162, long_max = 182),
                      "known/NZ 300km grid")
})

