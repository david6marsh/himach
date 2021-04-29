library(sf)
library(dplyr)

old_quiet <- getOption("quiet", default=0)
NZ_buffer30 <- hm_get_test("buffer")

test_that("Using s2", {
  expect_true(sf_use_s2())
})

test_that("Grid creation", {
  options("quiet" = 0) #for no reporting

  # We project the in-built test maps
  rg <- make_route_grid(NZ_buffer30, "NZ lat-long at 500km",
                        target_km = 500, classify = TRUE,
                        lat_min = -49, lat_max = -32,
                        long_min = 162, long_max = 182)

    expect_equal(rg@name, "NZ lat-long at 500km")

  expect_known_value(subset(rg@points, select = -xy),
                      "known/NZ_500km_grid_points")

  expect_known_value(subset(rg@lattice, select = -geometry),
                     "known/NZ_500km_grid_lattice")

})

test_that("Grid creation messaging", {

  options("quiet" = 1)
  # check messaging comes on - all the messages contain one of these words
  expect_message(make_route_grid(NZ_buffer30, "NZ lat-long at 300km",
                                      target_km = 300, classify = TRUE,
                                      lat_min = -49, lat_max = -32,
                                      long_min = 162, long_max = 182),
                 "(lattice)|(Classified)|(Calculated)")

})


options("quiet" = old_quiet)
