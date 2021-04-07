library(tidyverse)
library(sf)

## code to prepare `NZ_coast`, `NZ_buffer30`, `NZ_Buller_buffer40` dataset goes here
NZ_shp <- read_sf("ignore/statsnzterritorial-authority-2020-clipped-generalised-SHP/territorial-authority-2020-clipped-generalised.shp")
NZ_coast <- NZ_shp  %>%
  st_union() %>%
  st_as_s2() %>%
  s2::s2_simplify(1000) %>% # don't recommend simplify at this stage for real analysis
  st_as_sfc()

NZ_buffer30 <- NZ_coast %>%
  st_as_s2() %>%
  s2::s2_buffer_cells(distance = 30*1000, max_cells = 10000) %>%
  s2::s2_simplify(2000) %>%
  st_as_sfc()

usethis::use_data(NZ_coast, overwrite = TRUE)
usethis::use_data(NZ_buffer30, overwrite = TRUE)

NZ_Buller <- NZ_shp %>%
  filter(TA2020_V_1 == "Buller District")
NZ_Buller_u <- NZ_Buller %>%
  st_union() %>%
  st_as_s2() %>%
  s2::s2_simplify(1000) %>% # don't recommend simplify at this stage for real analysis
  st_as_sfc()
NZ_Buller_buffer40 <- NZ_Buller_u %>%
  st_as_s2() %>%
  s2::s2_buffer_cells(distance = 40*1000, max_cells = 10000) %>%
  s2::s2_simplify(2000) %>%
  st_as_sfc()
attr(NZ_Buller_buffer40, "avoid") <- "Buller+40km"

usethis::use_data(NZ_Buller_buffer40, overwrite = TRUE)
