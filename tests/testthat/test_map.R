Sys.setenv("R_TESTS" = "") # https://github.com/r-lib/testthat/issues/86 refers, but does not solve my issue with test_coverage
library(Mach2)
library(dplyr)


test_that("Route mapping", {
  NZ_thin <- sf::st_transform(NZ_coast, crs=crs_Pacific)
  airports <- make_airports(crs = crs_Pacific, warn = FALSE)

  # speed map
  expect_known_hash(map_routes(NZ_thin, NZ_routes,
                               crs = crs_Pacific,
                               show_route="speed"),
                    hash = "4a360e96f9" )
  # aircraft map
  expect_known_hash(map_routes(NZ_thin, NZ_routes,
                                 crs = crs_Pacific,
                                 show_route="aircraft"),
                      hash = "a3db60cfc3")

   # time advantage - auto calculated
 expect_known_hash(map_routes(NZ_thin, NZ_routes,
                                crs = crs_Pacific),
                     hash = "c87d33b068")

 # circuity - auto calculated - on crs_Atlantic
 expect_known_hash(map_routes(NZ_thin, NZ_routes,
                                crs = crs_Pacific,
                                show_route = "circuity"),
                     hash = "8a1c6f5f21")

 # time advantage calculated explicitly + frills
 rtes <- summarise_routes(NZ_routes, airports)
 routes <- NZ_routes %>%
   left_join(rtes %>% select(fullRouteID, advantage_h), by = "fullRouteID") %>%
   arrange(advantage_h)
 expect_known_hash(map_routes(NZ_thin, routes,
                              crs = crs_Pacific,
                              fat_map = NZ_buffer30,
                              ap_loc = airports,
                              refuel_airports =
                                airports %>% filter(APICAO=="NZWN"),
                              crow = TRUE,
                              route_envelope = TRUE),
                   hash = "0580bd566b")

})

test_that("World wrapping", {
  world <- st_as_sf(rnaturalearthdata::countries110)

  expect_known_hash(st_wrap_transform(world,
                                        crs_Pacific),
                      "3edfc51f513f2bda70a29a542a9")

})
