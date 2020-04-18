library(testthat)
library(twospeed)

test_that("Route envelope", {
  ac <- expand_aircraft(warn = FALSE)
  ap <- expand_airports()
  z <- routeEnvelope(ac[1,],NA, make_AP2("EGLL","KJFK",ap),4326)
  expect_type(z, "list")
  expect_s3_class(z, "sfc_POLYGON")
  # default is 200 points
  expect_gte(nrow(as.matrix(z[[1]])), 200)
})

# avoid testing against stored result, since that's sensitive to
# adding fields later, which is likely
# so - add fields but don't fiddle with the list of test routes ;-)
test_that("Route summary", {
  ap <- expand_airports(crs=crs_Pacific)
  rs1 <- summarise_routes(NZ_routes, ap)
  expect_equal(rs1[1, ]$refuel_ap, "NZWN")
  expect_equal(rs1[2, ]$M084_h, 1.68)
  expect_equal(rs1[3, ]$sea_dist_frac, 0.753)
  expect_equal(rs1[4, ]$n_phases, 5)
  expect_true(rs1[5, ]$best)
  expect_true(is.na(rs1[6, ]$time_h))
  # parameter behaviour?
  rs2 <- summarise_routes(NZ_routes, ap, arrdep_h = 1.0)
  expect_equal(rs2[1, ]$advantage_h - rs1[1, ]$advantage_h, 0.5)
})

test_that("Find Leg",{
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting
  # need to load some of the built-in data
  aircraft <- expand_aircraft(warn = FALSE)
  airports <- expand_airports()
  NZ_buffer <- sf::st_transform(NZ_b, crs=crs_Pacific)

  # fail with unmatched CRS
  expect_error(
  routes <- findLeg(aircraft[4,],
                    make_AP2("NZAA","NZCH",airports),
                    onMap = NZ_buffer,
                    pg = NZ_grid,
                    apLoc = airports))

  airports <- expand_airports(crs = crs_Pacific)
  expect_known_output(
    routes <- findLeg(aircraft[4,],
                      make_AP2("NZAA","NZCH",airports),
                      onMap = NZ_buffer,
                      pg = NZ_grid,
                      apLoc = airports),
    "known/findLeg_default")

  options("quiet" = old_quiet)
})


test_that("Find Route",{
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting
  # need to load some of the built-in data
  aircraft <- expand_aircraft(warn = FALSE)
  airports <- expand_airports()
  NZ_buffer <- sf::st_transform(NZ_b, crs=crs_Pacific)
#
#   # fail with unmatched CRS - ??fails
#   expect_error(
#     routes <- findRoute(aircraft[4,],
#                       make_AP2("NZAA","NZCH", airports),
#                       onMap = NZ_buffer,
#                       pg = NZ_grid,
#                       apLoc = airports))

  airports <- expand_airports(crs = crs_Pacific)
  expect_known_output(
    routes <- findRoute(aircraft[4,],
                      make_AP2("NZAA","NZCH",airports),
                      onMap = NZ_buffer,
                      pg = NZ_grid,
                      apLoc = airports),
    "known/findRoute_default")

  options("quiet" = old_quiet)
})