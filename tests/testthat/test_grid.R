library(testthat)
library(twospeed)

test_that("Grid creation works", {
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting

  # We project the in-built test maps
  NZ_buffer_Pac <- sf::st_transform(twospeed::NZ_buffer30, crs=crs_Pacific)

  expect_known_output(make_route_grid(NZ_buffer_Pac,"NZ lat-long at 300km",
                                     target_km = 300, classify = TRUE,
                                     lat_min = -49, lat_max = -32,
                                     long_min = 162, long_max = 182),
                      "known/NZ 300km grid")

  options("quiet" = 1)
  # check messaging comes on - all the messages contain one of these words
  expect_message(make_route_grid(NZ_buffer_Pac,"NZ lat-long at 300km",
                                      target_km = 300, classify = TRUE,
                                      lat_min = -49, lat_max = -32,
                                      long_min = 162, long_max = 182),
                 "(lattice)|(Classified)|(Calculated)")

  options("quiet" = old_quiet)
})

