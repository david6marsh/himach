library(dplyr)
library(ggplot2)

# Hadley rules out hash as not helpful: https://groups.google.com/forum/#!msg/ggplot2/JEvC86l_otA/i7k0yTDt2_UJ
# vdiffr says may not be useful for sf objects - which is everything here :-(

# for the moment, limit to testing that the maps return ggplot objects, without fail

NZ_coast <- hm_get_test("coast")
NZ_buffer30 <- hm_get_test("buffer")
NZ_routes <- hm_get_test("route")
NZ_Buller <- hm_get_test("nofly")
# given some solaris CMD Check errors (old GDAl?)
# redefine crs_Pacific and crs_Atlantic
#same as Robinson, but centred on long +180
crs_Pacific <- sf::st_crs("+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
crs_Atlantic <- sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

test_that("Route mapping", {
  airports <- make_airports(crs = crs_Pacific, warn = FALSE)
  # speed map
  expect_silent(z <- map_routes(NZ_coast, NZ_routes,
                               crs = crs_Pacific,
                               show_route="speed"))
  expect_true("ggplot" %in% class(z))

  # aircraft map
  expect_silent(z <- map_routes(NZ_coast, NZ_routes,
                                 crs = crs_Pacific,
                                 show_route="aircraft"))
  expect_true("ggplot" %in% class(z))

   # time advantage - auto calculated
  # add a no-fly zone to test those lines - even if routes ignore it
 expect_silent(z <- map_routes(NZ_coast, NZ_routes,
                                crs = crs_Pacific,
                               avoid_map = NZ_Buller))
 expect_true("ggplot" %in% class(z))

 # circuity - auto calculated - on crs_Atlantic
 expect_silent(z <- map_routes(NZ_coast, NZ_routes,
                                crs = crs_Pacific,
                                show_route = "circuity"))
 expect_true("ggplot" %in% class(z))

 # time advantage calculated explicitly + frills
 rtes <- summarise_routes(NZ_routes, airports) %>%
   st_set_geometry(NULL) %>%
   select(fullRouteID, advantage_h)
 routes <- NZ_routes %>%
   filter(!is.na(phase)) %>% # remove non-routes
   left_join(rtes, by = "fullRouteID") %>%
   arrange(advantage_h)
 expect_silent(z <- map_routes(NZ_coast, routes,
                              crs = crs_Pacific,
                              fat_map = NZ_buffer30,
                              ap_loc = airports,
                              refuel_airports =
                                airports %>% filter(APICAO=="NZWN"),
                              crow = TRUE,
                              route_envelope = TRUE))
 expect_true("ggplot" %in% class(z))

})

test_that("can make range envelope", {
  airports <- make_airports(crs = crs_Atlantic, warn = FALSE)
  aircraft <- make_aircraft(warn = FALSE)
  # test only a sample of points
  # wkt seems to be machine-dependent so test fails on windows
  # so strip out the values only
  expect_known_value(himach:::make_range_envelope(aircraft[1, ],
                                                "LFPG",
                                                airports,
                                                envelope_points = 20) %>%
                       unlist(),
                     "known/LFPG_envelope_20",
                     tolerance = 0.05)

})


