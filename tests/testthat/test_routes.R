library(testthat)
library(Mach2)
library(dplyr)

test_that("Route envelope", {
  ac <- make_aircraft(warn = FALSE)
  ap <- make_airports()
  z <- make_route_envelope(ac[1,],NA, make_AP2("EGLL","KJFK",ap),4326)
  expect_type(z, "list")
  expect_s3_class(z, "sfc_POLYGON")
  # default is 200 points
  expect_gte(nrow(as.matrix(z[[1]])), 200)
})

# avoid testing against stored result, since that's sensitive to
# adding fields later, which is likely
# so - add fields but don't fiddle with the list of test routes ;-)
test_that("Route summary", {
  ap <- make_airports(crs=crs_Pacific)
  rs1 <- summarise_routes(NZ_routes, ap)
  expect_equal(rs1[1, ]$refuel_ap, "NZWN")
  expect_equal(rs1[2, ]$M084_h, 1.68)
  expect_equal(rs1[2, ]$advantage_h, -0.88)
  expect_equal(rs1[3, ]$sea_dist_frac, 0.753)
  expect_equal(rs1[4, ]$n_phases, 5)
  expect_equal(rs1[4, ]$circuity, 1.21)
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
  aircraft <- make_aircraft(warn = FALSE)
  airports <- make_airports()
  NZ_buffer_Pac <- sf::st_transform(NZ_buffer30, crs=crs_Pacific)

  # fail with unmatched CRS
  expect_error(
  find_leg(aircraft[4,],
                    make_AP2("NZAA","NZCH",airports),
                    fat_map = NZ_buffer_Pac,
                    route_grid = NZ_grid,
                    ap_loc = airports))

  airports <- make_airports(crs = crs_Pacific)
  # can't test against a route with a timestamp!
  routes <- find_leg(aircraft[4,],
                     make_AP2("NZAA","NZCH",airports),
                     fat_map = NZ_buffer_Pac,
                     route_grid = NZ_grid,
                     ap_loc = airports) %>%
    select(-timestamp)
  expect_known_hash(routes, hash = "9b24caa126")

  options("quiet" = old_quiet)
})


test_that("Find Route",{
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting
  # need to load some of the built-in data
  aircraft <- make_aircraft(warn = FALSE)
  airports <- make_airports()
  NZ_buffer_Pac <- sf::st_transform(NZ_buffer30, crs=crs_Pacific)

  airports <- make_airports(crs = crs_Pacific)
  routes <- find_route(aircraft[4,],
                       make_AP2("NZAA","NZCH",airports),
                       fat_map = NZ_buffer_Pac,
                       route_grid = NZ_grid,
                       ap_loc = airports) %>%
    select(-timestamp)
  expect_known_hash(routes, hash = "9b24caa126")

  # test with parallel subsonic aircraft
  routes <- find_route(aircraft[1,],
                       make_AP2("NZGS","NZDN",airports),
                       fat_map = NZ_buffer_Pac,
                       route_grid = NZ_grid,
                       ap_loc = airports,
                       cf_subsonic = aircraft[3,]) %>%
    select(-timestamp)
  expect_known_hash(routes, hash = "e21017d2a7")

  options("quiet" = old_quiet)
})

test_that("Find Routes",{
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting
  # need to load some of the built-in data
  aircraft <- make_aircraft(warn = FALSE)
  airports <- make_airports(crs = crs_Pacific)
  refuel_ap <- airports %>%
    filter(APICAO=="NZWN")
  NZ_buffer_Pac <- sf::st_transform(NZ_buffer30, crs=crs_Pacific)

  ap2 <- as.data.frame(matrix(c("NZAA","NZCH","NZAA","NZDN"),
                              ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  ac <- aircraft[c(1,4), ]$id

  invisible(capture.output(
    routes <- find_routes(ac, ap2, aircraft, airports,
                        fat_map = NZ_buffer_Pac,
                        route_grid = NZ_grid,
                        refuel = refuel_ap) %>%
    select(-timestamp)
  ))
  expect_known_hash(routes, hash = "045569d467")

  # and again with a no-fly zone
  Buller_nofly <- sf::st_transform(NZ_Buller_buffer40, crs=crs_Pacific)
  attr(Buller_nofly, "avoid") <- "Buller+40km" #required for correct caching

  invisible(capture.output(
    routes <- find_routes(ac, ap2, aircraft, airports,
                          fat_map = NZ_buffer_Pac,
                          route_grid = NZ_grid,
                          refuel = refuel_ap,
                          avoid = Buller_nofly) %>%
      select(-timestamp)
  ))
  expect_known_hash(routes, hash = "0780500293")

  options("quiet" = old_quiet)
})
