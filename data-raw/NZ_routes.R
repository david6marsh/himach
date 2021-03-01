## code to prepare `NZ_routes` dataset goes here
# to help with testing and vignettes

# need to load some of the built-in data
ac <- make_aircraft()
airports <- make_airports(crs = crs_Pacific) %>%
  filter(substr(APICAO,1,1)=="N") #just around New Zealand
refuel_ap <- airports %>%
  filter(APICAO=="NZWN")
NZ_buffer <- sf::st_transform(Mach2::NZ_buffer30, crs=crs_Pacific)

aps <- rbind(make_AP2("NZAA","NZCH",airports), make_AP2("NZAA","NZDN",airports),
             make_AP2("NZAA","NZQN",airports), make_AP2("NZWN","NZQN",airports),
             make_AP2("NZGS","NZCH",airports))

options("quiet"= 1)
NZ_routes <- purrr::reduce(lapply(
  1:nrow(aps),
  function(x) find_route(ac[4,],
                        aps[x,],
                        NZ_buffer,
                        refuel = refuel_ap, refuel_topN = 1,
                        refuel_only_if = TRUE,
                        route_grid = NZ_grid,
                        ap_loc = airports)),
  rbind)
NZ_routes <- st_set_geometry(NZ_routes, "gc") # convert to sf

usethis::use_data(NZ_routes, overwrite = TRUE)
