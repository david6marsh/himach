# Internal data

## code to prepare `lattice_phases` dataset goes here
# several functions need this to ensure factor levels are the same
lattice_phases <- c("sea","transition","land","arr/dep","refuel")


# standard points
pole_N <- st_sfc(st_point(c(0,90)), crs = crs_N)
pole_S <- st_sfc(st_point(c(0,-90)), crs = crs_S)

# standard lines
pts <- data.frame(lo = 0, la = seq(-89.5, -89.5, length.out = 200))
long_0 <- st_sfc(st_linestring(as.matrix(pts)), crs = crs_longlat)
long_180 <- st_sfc(st_linestring(as.matrix(pts %>% mutate(lo = 180))), crs = crs_longlat)

# now the map data etc
# all test data are prepared here
# used internally and accessed by a function

library(tidyverse)
library(sf)

## code to prepare `NZ_coast`, `NZ_buffer30`, `NZ_Buller_buffer40` dataset goes here
NZ_shp <- read_sf("ignore/statsnzterritorial-authority-2020-clipped-generalised-SHP/territorial-authority-2020-clipped-generalised.shp")
NZ_coast <- NZ_shp  %>%
  st_union() %>%
  st_as_s2() %>%
  s2::s2_simplify(1000) %>% # don't recommend simplify at this stage for real analysis
  st_as_sfc() %>%
  st_transform(crs = crs_Pacific)

NZ_buffer30 <- NZ_coast %>%
  st_as_s2() %>%
  s2::s2_buffer_cells(distance = 30*1000, max_cells = 10000) %>%
  s2::s2_simplify(2000) %>%
  st_as_sfc() %>%
  st_transform(crs = crs_Pacific)

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
  st_as_sfc() %>%
  st_transform(crs = crs_Pacific)
attr(NZ_Buller_buffer40, "avoid") <- "Buller+40km"

## code to prepare `NZ_grid` dataset goes here
# to help with testing and vignettes
target_km=30
system.time(
  NZ_grid <- make_route_grid(NZ_buffer30, "NZ lat-long at 30km",
                             target_km = target_km, classify = TRUE,
                             lat_min = -49, lat_max = -32,
                             long_min = 162, long_max = 182)
)


# need to load some of the built-in data
ac <- make_aircraft()
airports <- make_airports(crs = crs_Pacific) %>%
  filter(substr(APICAO,1,1)=="N") #just around New Zealand
refuel_ap <- airports %>%
  filter(APICAO=="NZWN")

aps <- rbind(make_AP2("NZAA","NZCH",airports), make_AP2("NZAA","NZDN",airports),
             make_AP2("NZAA","NZQN",airports), make_AP2("NZWN","NZQN",airports),
             make_AP2("NZGS","NZCH",airports))

options("quiet"= 1)
NZ_routes <- purrr::reduce(lapply(
  1:nrow(aps),
  function(x) find_route(ac[4,],
                         aps[x,],
                         NZ_buffer30,
                         refuel = refuel_ap, refuel_topN = 1,
                         refuel_only_if = TRUE,
                         route_grid = NZ_grid,
                         ap_loc = airports)),
  rbind)
NZ_routes <- st_set_geometry(NZ_routes, "gc") # convert to sf


# write to a common file
usethis::use_data(lattice_phases, pole_N, pole_S,
                  long_0, long_180,
                  NZ_coast, NZ_buffer30, NZ_Buller_buffer40, NZ_grid, NZ_routes,
                  internal = TRUE, overwrite = TRUE)
