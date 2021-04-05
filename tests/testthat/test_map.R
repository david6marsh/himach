library(dplyr)
library(ggplot2)

# Hadley rules out hash as not helpful: https://groups.google.com/forum/#!msg/ggplot2/JEvC86l_otA/i7k0yTDt2_UJ
# vdiffr says may not be useful for sf objects - which is everything here :-(

# for the moment, limit to testing that the maps return ggplot objects, without fail

test_that("Route mapping", {
  NZ_thin <- sf::st_transform(NZ_coast, crs=crs_Pacific)
  airports <- make_airports(crs = crs_Pacific, warn = FALSE)

  # speed map
  expect_silent(z <- map_routes(NZ_thin, NZ_routes,
                               crs = crs_Pacific,
                               show_route="speed"))
  expect_true("ggplot" %in% class(z))

  # aircraft map
  expect_silent(z <- map_routes(NZ_thin, NZ_routes,
                                 crs = crs_Pacific,
                                 show_route="aircraft"))
  expect_true("ggplot" %in% class(z))

   # time advantage - auto calculated
 expect_silent(z <- map_routes(NZ_thin, NZ_routes,
                                crs = crs_Pacific))
 expect_true("ggplot" %in% class(z))

 # circuity - auto calculated - on crs_Atlantic
 expect_silent(z <- map_routes(NZ_thin, NZ_routes,
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
 expect_silent(z <- map_routes(NZ_thin, routes,
                              crs = crs_Pacific,
                              fat_map = NZ_buffer30,
                              ap_loc = airports,
                              refuel_airports =
                                airports %>% filter(APICAO=="NZWN"),
                              crow = TRUE,
                              route_envelope = TRUE))
 expect_true("ggplot" %in% class(z))

})

# test_that("World wrapping", {
#   world <- sf::st_as_sf(rnaturalearthdata::countries110)
#
#   expect_known_hash(st_slice_transform(world,
#                                         crs_Pacific),
#                       "62ae830d048da98dc436924c678")
#
# })

test_that("can make range envelope", {
  airports <- make_airports(crs = crs_Atlantic, warn = FALSE)
  aircraft <- make_aircraft(warn = FALSE)
  # test only a sample of points
  expect_known_value(himach:::make_range_envelope(aircraft[1, ],
                                                "LFPG",
                                                airports,
                                                envelope_points = 20)[c(1, 5, 10, 15), ],
                     "known/LFPG_envelope_20")

})
