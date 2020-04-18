library(testthat)
library(twospeed)

test_that("Default aircraft data loads", {
  expect_warning(expand_aircraft())
  expect_known_output(expand_aircraft(warn = FALSE), "known/ac_default_load")
})

test_that("Aircraft data loads", {
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
                   arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
  expect_known_output(expand_aircraft(ac), "known/ac_load")
  #missing vbl
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9,
                   arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
  expect_error(expand_aircraft(ac))
  # vbl not numeric
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
                   arrdep_kph = 300, range_km = "6,000", stringsAsFactors=FALSE)
  expect_error(expand_aircraft(ac))

})

test_that("Default airport data loads", {
  expect_message(expand_airports())
  expect_known_output(expand_airports(), "known/ap_default_load")
})

test_that("Airport data loads", {
  airports <- data.frame(APICAO = "TEST", lat = 10, long = 10, stringsAsFactors = FALSE)
  expect_known_output(expand_airports(airports), "known/ap_load")
})

test_that("NZ maps available", {
  expect_true("sfc_MULTIPOLYGON" %in% class(NZ_u))
  expect_true("sfc_MULTIPOLYGON" %in% class(NZ_b))
})

test_that("can make AP2",{
  z <- make_AP2("EGLL","NZCH")
  expect_equal(z$AP2, "EGLL<>NZCH")
  expect_equal(signif(z$gcdistance_km,3), 19000)
  #don't mind which order they're in
  expect_setequal(round(z[1,c("from_long", "to_long")],2),
                  c(-0.46, 172.53))
  aps <- expand_airports()
  z <- make_AP2("BIKF", "EDDF")
  #check sort order
  expect_equal(z$AP2, "EDDF<>BIKF")
})
