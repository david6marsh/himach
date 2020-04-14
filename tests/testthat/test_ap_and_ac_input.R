library(testthat)
library(twospeed)

test_that("Default aircraft data loads", {
  expect_warning(expand_aircraft())
  expect_known_output(expand_aircraft(), "known/ac_default_load")
})

test_that("Aircraft data loads", {
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
                   arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
  expect_known_output(expand_aircraft(ac), "known/ac_load")
})

test_that("Default airport data loads", {
  expect_warning(expand_airports())
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
