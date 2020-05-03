library(testthat)
library(twospeed)
library(sf)
library(dplyr)

old_quiet <- getOption("quiet", default=0)
NZ_buffer_Pac <- sf::st_transform(twospeed::NZ_buffer30, crs=crs_Pacific)

test_that("Grid creation", {
  options("quiet" = 0) #for no reporting

  # We project the in-built test maps
  rg <- make_route_grid(NZ_buffer_Pac,"NZ lat-long at 300km",
                        target_km = 300, classify = TRUE,
                        lat_min = -49, lat_max = -32,
                        long_min = 162, long_max = 182)

    expect_equal(rg@name, "NZ lat-long at 300km")

  expect_known_value(subset(rg@points, select = -xy),
                      "known/NZ 300km grid points")
  expect_known_value(sf::st_as_binary(rg@points$xy),
                     "known/NZ 300km grid points xy")

  expect_known_value(subset(rg@lattice, select = -geometry),
                     "known/NZ 300km grid lattice")
  expect_known_value(sf::st_as_binary(rg@lattice$geometry),
                     "known/NZ 300km grid lattice geometry")

})

test_that("Grid creation messaging", {

  options("quiet" = 1)
  # check messaging comes on - all the messages contain one of these words
  expect_message(make_route_grid(NZ_buffer_Pac,"NZ lat-long at 300km",
                                      target_km = 300, classify = TRUE,
                                      lat_min = -49, lat_max = -32,
                                      long_min = 162, long_max = 182),
                 "(lattice)|(Classified)|(Calculated)")

})


options("quiet" = old_quiet)
