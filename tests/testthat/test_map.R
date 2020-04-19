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
  # time advantage
  rtes <- summarise_routes(NZ_routes, airports)
  routes <- NZ_routes %>%
    left_join(rtes, by = "fullRouteID") %>%
    arrange(advantage_h)
  expect_known_output(map_routes(NZ_thin, routes,
                                crs = crs_Pacific),
                      "known/NZ time map")
})

