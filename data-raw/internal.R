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

usethis::use_data(lattice_phases, pole_N, pole_S,
                  long_0, long_180,
                  internal = TRUE, overwrite = TRUE)
