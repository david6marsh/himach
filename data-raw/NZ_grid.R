## code to prepare `NZ_grid` dataset goes here
# to help with testing and vignettes

NZ_buffer <- sf::st_transform(Mach2::NZ_b, crs=crs_Pacific)
target_km=30
system.time(
  NZ_grid <- make_route_grid(NZ_buffer,"NZ lat-long at 30km",
                           target_km = target_km, classify = TRUE,
                           lat_min = -49, lat_max = -32,
                           long_min = 162, long_max = 182)
)

usethis::use_data(NZ_grid, overwrite = TRUE)
