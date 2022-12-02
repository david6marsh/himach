
old_quiet <- getOption("quiet", default=0)
# given some solaris CMD Check errors (old GDAl?)
# redefine crs_Pacific
#same as Robinson, but centred on long +180
# crs_Pacific <- sf::st_crs("+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

NZ_buffer30 <- hm_get_test("buffer")

test_that("Using s2", {
  z <- sf::sf_use_s2()
  expect_true(z)
})

test_that("Grid creation", {
  options("quiet" = 0) #for no reporting

  # We project the in-built test maps
  rg <- make_route_grid(NZ_buffer30, "NZ lat-long at 500km",
                        target_km = 500, classify = TRUE,
                        lat_min = -49, lat_max = -32,
                        long_min = 162, long_max = 182)

    expect_equal(rg@name, "NZ lat-long at 500km")

  expect_snapshot_value(subset(rg@points, select = -xy), style = "serialize")

  expect_snapshot_value(subset(rg@lattice, select = -geometry), style = "serialize")

})

# in testthat v3, drop the checking of this messaging, which is hardly critical if the results are ok
test_that("Grid creation messaging", {

  options("quiet" = 1)
  # check messaging comes on - all the messages contain one of these words
  suppressMessages(
    expect_message(make_route_grid(NZ_buffer30, "NZ lat-long at 300km",
                                   target_km = 300, classify = TRUE,
                                   lat_min = -49, lat_max = -32,
                                   long_min = 162, long_max = 182),
                   "(lattice)|(Classified)|(Calculated)")
  )
})


options("quiet" = old_quiet)
