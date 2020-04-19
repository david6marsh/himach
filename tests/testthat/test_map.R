library(testthat)
library(twospeed)

test_that("Route mapping", {
  NZ_thin <- sf::st_transform(NZ_coast, crs=crs_Pacific)
  airports <- make_airports(crs = crs_Pacific, warn = FALSE)
  # speed map
  expect_known_output(map_routes(NZ_thin, NZ_routes,
                                crs = crs_Pacific,
                                show_route="speed"),
                      "known/NZ speed map")
  # time advantage - auto calculated
 expect_known_output(map_routes(NZ_thin, NZ_routes,
                                crs = crs_Pacific),
                      "known/NZ time map")

 # time advantage calculated explicitly + frills
 rtes <- summarise_routes(NZ_routes, airports)
 routes <- NZ_routes %>%
   left_join(rtes %>% select(fullRouteID, advantage_h), by = "fullRouteID") %>%
   arrange(advantage_h)
 expect_known_output(map_routes(NZ_thin, routes,
                                crs = crs_Pacific,
                                fat_map = NZ_buffer30,
                                ap_loc = airports,
                                refuel_airports =
                                  airports %>% filter(APICAO=="NZWN"),
                                crow = TRUE,
                                range_envelope = TRUE),
                     "known/NZ time and frills map")

})

