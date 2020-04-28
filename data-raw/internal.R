# Internal data

## code to prepare `lattice_phases` dataset goes here
# several functions need this to ensure factor levels are the same
lattice_phases <- c("sea","transition","land","arr/dep","refuel")


# (default) crs that uses lat and long
# useful for converting matrices of long-lat to spatial
crs_latlong <- 4326


usethis::use_data(lattice_phases, crs_latlong,
                  internal = TRUE, overwrite = TRUE)
