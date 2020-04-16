library(testthat)
library(twospeed)

test_that("Route envelope", {
  ac <- expand_aircraft()
  ap <- expand_airports()
  z <- routeEnvelope(ac[1,],NA, make_AP2("EGLL","KJFK",ap),4326)
  expect_type(z, "list")
  expect_s3_class(z, "sfc_POLYGON")
  #default is 200 points
  expect_gte(nrow(as.matrix(z[[1]])), 200)
})



