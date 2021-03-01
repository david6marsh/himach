## code to prepare `NZ_coast`, `NZ_buffer30`, `NZ_Buller_buffer40` dataset goes here
#sf_use_s2(TRUE) ??
NZ_shp <- sf::read_sf("~/R/2Speed/data/statsnzterritorial-authority-2020-clipped-generalised-SHP/territorial-authority-2020-clipped-generalised.shp")
NZ_coast <- sf::st_simplify(sf::st_union(NZ_shp), dTolerance = 1000)
NZ_buffer30 <- sf::st_union(sf::st_buffer(NZ_coast, 30 * 1000))

usethis::use_data(NZ_coast, overwrite = TRUE)
usethis::use_data(NZ_buffer30, overwrite = TRUE)

NZ_Buller <- NZ_shp %>%
  filter(TA2020_V_1 == "Buller District")
NZ_Buller_u <- sf::st_union(sf::st_simplify(NZ_Buller, dTolerance = 1000))
NZ_Buller_buffer40 <- sf::st_union(sf::st_buffer(NZ_Buller_u, 40 * 1000))
attr(NZ_Buller_buffer40, "avoid") <- "Buller+40km"

usethis::use_data(NZ_Buller_buffer40, overwrite = TRUE)
