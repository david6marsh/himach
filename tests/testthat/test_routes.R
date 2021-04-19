library(dplyr)

NZ_coast <- hm_get_test("coast")
NZ_buffer30 <- hm_get_test("buffer")
NZ_Buller_buffer40 <- hm_get_test("nofly")
NZ_grid <- hm_get_test("grid")
NZ_routes <- hm_get_test("route")
old_tolerance <- testthat::testthat_tolerance()
testthat::testthat_tolerance(5e-3) # relatively high tolerance for differences

test_that("Route envelope", {
  ac <- make_aircraft(warn = FALSE)
  ap <- make_airports()
  z <- make_route_envelope(ac[1,], make_AP2("EGLL","KJFK",ap),
                           envelope_points = 50)
  expect_type(z, "list")
  expect_s3_class(z, "sfc_POLYGON")
  expect_gte(nrow(as.matrix(z[[1]])), 50)
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
  expect_equal(rs1[3, ]$sea_dist_frac, 0.70)
  expect_equal(rs1[4, ]$n_phases, 5)
  expect_equal(rs1[2, ]$n_accel, 2)
  expect_equal(rs1[2, ]$ave_fly_speed_M, 0.80)
  expect_equal(rs1[1, ]$fly_time_h, 1.67)
  expect_equal(rs1[4, ]$circuity, 0.21)
  expect_true(rs1[5, ]$best)
  expect_true(is.na(rs1[6, ]$time_h))
  # parameter behaviour?
  rs2 <- summarise_routes(NZ_routes, ap, arrdep_h = 1.0)
  expect_equal(rs2[1, ]$advantage_h - rs1[1, ]$advantage_h, 0.5)
})

test_that("find_leg catches input error",{
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting
  hm_clean_cache() #start without cache
  # need to load some of the built-in data
  aircraft <- make_aircraft(warn = FALSE)
  # airports <- make_airports()
  airports <- make_airports(crs = crs_Pacific)
  options("quiet" = old_quiet)
  # for visual check:
  # ggplot(NZ_buffer30) + geom_sf() + geom_sf(data = routes$gc)

  # fail nicely with bad aircraft index
  expect_error(find_leg(aircraft[400,],
                        make_AP2("NZAA","NZCH",airports),
                        fat_map = NZ_buffer30,
                        route_grid = NZ_grid,
                        ap_loc = airports),
                 "Aircraft invalid")
})


test_that("find_route works with subsonic option",{
  old_quiet <- getOption("quiet", default = 0)
  options("quiet" = 3) #for full reporting
  hm_clean_cache() #start without cache
  # need to load some of the built-in data
  aircraft <- make_aircraft(warn = FALSE)
  airports <- make_airports(crs = crs_Pacific)

  # test with parallel subsonic aircraft
  # just ditch the output
  invisible(capture.output(
    routes <- find_route(aircraft[1, ],
                       make_AP2("NZGS", "NZDN", airports),
                       fat_map = NZ_buffer30,
                       route_grid = NZ_grid,
                       ap_loc = airports,
                       cf_subsonic = aircraft[3, ]) %>%
    select(-timestamp) %>%
      slice(c(1, 3, 8)) %>%  # for a small sample
      # wkt is machine-dependent so just extract length/area
      mutate(across(c(gc, crow), st_length)) %>%
      mutate(envelope = st_area(envelope))
  ))
  # test a couple of rows
  expect_known_value(routes, "known/test_route_subsonic_NZGS_NZDN")

  # and test saving of cache
  tmp_dir <- tempdir()
  full_filename <- hm_save_cache("test_that", NZ_grid, aircraft, path = tmp_dir)
  hm_clean_cache() #empty cache
  hm_load_cache(full_filename)
  expect_true(length(.hm_cache) == 2)
  expect_true(length(.hm_cache$route_cache) == 2)
  expect_true(length(.hm_cache$star_cache) == 4)
  unlink(full_filename) # remove the temporary cache file to pass CRAN test

  options("quiet" = old_quiet)
})

test_that("Find multiple routes for multiple aircraft",{
  old_quiet <- getOption("quiet", default=0)
  options("quiet" = 0) #for no reporting
  hm_clean_cache() #start without cache
  # need to load some of the built-in data
  aircraft <- make_aircraft(warn = FALSE)
  airports <- make_airports(crs = crs_Pacific)
  refuel_ap <- airports %>%
    filter(APICAO == "NZWN")

  ap2 <- as.data.frame(matrix(c("NZAA","NZCH","NZAA","NZDN"),
                              ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  ac <- aircraft[c(1,4), ]$id

  invisible(capture.output(
    routes <- find_routes(ac, ap2, aircraft, airports,
                        fat_map = NZ_buffer30,
                        route_grid = NZ_grid,
                        refuel = refuel_ap) %>%
    select(-timestamp) %>%
      slice(c(4, 7, 25)) %>%  # for a small sample
      # wkt is machine-dependent so just extract length/area
      mutate(across(c(gc, crow), st_length)) %>%
      mutate(envelope = st_area(envelope))
  ))
  # just test a sample
  expect_known_value(routes, "known/test_multiroute")

  # and again with a no-fly zone - and just one AP2
  invisible(capture.output(
    routes <- find_routes(ac, ap2[1, ], aircraft, airports,
                          fat_map = NZ_buffer30,
                          route_grid = NZ_grid,
                          refuel = refuel_ap,
                          avoid = NZ_Buller_buffer40) %>%
      select(-timestamp) %>%
      slice(c(2, 5, 12)) %>%  # for a small sample
      # wkt is machine-dependent so just extract length/area
      mutate(across(c(gc, crow), st_length)) %>%
      mutate(envelope = st_area(envelope))
  ))
  # check one row from each route
  expect_known_value(routes, "known/test_multiroute_nofly")

  # check for faulty airports
  ap2 <- as.data.frame(matrix(c("ZZZZ", "NZAA", "NZCH", "NZAA"),
                              ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  expect_error(find_routes(ac, ap2, aircraft, airports,
                             fat_map = NZ_buffer30,
                             route_grid = NZ_grid),
               "unknown")

  options("quiet" = old_quiet)
})

testthat::testthat_tolerance(old_tolerance)
