# maps
#
# Functions for plotting nice maps of routes


# return polygon square based on sf bbox
bbox_to_poly <- function(b, crs){
  st_sfc(st_polygon(list(matrix(c(b$xmin,b$ymin,
                                  b$xmax,b$ymin,
                                  b$xmax,b$ymax,
                                  b$xmin,b$ymax,
                                  b$xmin,b$ymin),
                                ncol=2, byrow=TRUE))),
         crs=crs)
}


#wrapper around ggplot/geom_sf for quick map
plot_map <- function(msf,
                     c_land = "green1",
                     c_border = "grey70",
                     w_border = 0.1){
  ggplot(msf) +
    geom_sf(size = w_border, colour = c_border, fill = c_land) +
    # coord_sf() + #not needed for already-projected feature sets
    theme_minimal()
}


# achievable range from a point
# not used for route finding
rangeEnvelope <- function(ac, pg, ap2, onMap,
                          envelope_points=70){
  #range envelope shows how far from an airport you can go  with a given range
  #it takes the first airport of the given airport pair
  #create no-wind range ellipse
  #transform the origin
  projC <- st_transform(st_sfc(st_point(
    c(ap2$from_long, ap2$from_lat)),
    crs=4326),crs=st_crs(onMap))

  r <- ac$range_km * 1000

  theta <- seq(0, 2* pi, length.out = envelope_points)
  circle <- matrix(c(r*cos(theta), r*sin(theta)), ncol=2)

  ellipse <- t(circle) +
    as.vector(st_coordinates(projC))

  st_convex_hull(st_sfc(st_multipoint(t(ellipse)),crs=st_crs(onMap)))
}


#all-singing layered map with lots of options
route_map <- function(
  routes=NA, show_route="time",
  crow=FALSE, range_envelope=FALSE, #show crow-flies
  crs=54030, #projection default
  bound=TRUE, bound_margin_km=200, #by default bound on the set of gc and add 1000km
  fat_map=NA, thin_map_file="data/Map4326 simp pt03deg no Ant.RDS", #map defaults
  avoid_map=m_avoid_u,
  land_f="grey90", buffer_f="grey70", avoid_f="grey80", #fill colours for map
  l_alpha=0.6, l_size=0.4, #line settings
  crow_col="grey70", crow_size=0.2, #crow line settings
  airports=TRUE, refuel_airports=TRUE, ap_locations=ap_loc, #plot or not
  ap_col="darkblue", ap_size=0.4, #airport settings
  rap_col="red", rap_size=0.4 #airport settings
){
  (stopifnot(is.na(show_route) || show_route %in% c("speed","aircraft","time")))

  #thin map is the one without buffer
  thin_map <- readRDS(thin_map_file) #simplified
  thin_map <- prj(thin_map, crs=crs) #force to CRS used for this map

  #layer 1 (one or two base maps)
  if (is.na(fat_map)) {
    m <- plot_map(thin_map, c_border=NA, c_land=land_f)
  } else {
    m <- plot_map(prj(fat_map, crs=crs), c_border=NA, c_land=buffer_f) +
      geom_sf(data=thin_map, fill=land_f, colour=NA)
  }

  #layer 1b (no fly-zone)
  if (!is.na(avoid_map)){
    m <- m +
      geom_sf(data = prj(m_avoid_u, crs=crs), colour=avoid_f, fill=avoid_f)
  }

  #layer 2 (main lines)
  if (show_route=="time"){
    m <- m +  labs(colour="Time Advantage") +
      geom_sf(data = routes,
              aes(geometry=st_wrap(prj(gc, crs=crs)), colour=advantage_h), fill="white",
              size=l_size, lineend="round", alpha=l_alpha) +
      scale_colour_viridis_c()
  }
  if (show_route=="speed"){
    m <- m +  labs(colour="Average speed on segment (kph)") +
      geom_sf(data = routes,
              aes(geometry=st_wrap(prj(gc, crs=crs)), colour=speed_kph),
              size=l_size, lineend="round", alpha=l_alpha) +
      scale_colour_viridis_c()
  }

  #layer 3: crow-flies
  if (crow){
    m <- m +
      geom_sf(data = st_wrap(prj(route$crow, crs=crs)), colour=crow_col, size = crow_size)
  }

  # #layer 4: range envelope
  # if (range_envelope){
  #   m <- m +
  #     geom_sf(data=st_wrap(prj(route$envelope, crs=crs)), fill=NA,
  #             colour="grey70", alpha=0.5, size = 0.6)
  # }

  #layer 6: airports
  if (airports){
    APs <- sort(unique(unlist(lapply(unique(routes$routeID),
                                     function(x)strsplit(x, "<>")))))
    m <- m +
      geom_sf(data = ap_locations %>% filter(APICAO %in% APs),
              aes(geometry=prj(ap_locs, crs=crs)), colour=ap_col, size=ap_size)
  }

  #layer 7: refuel airports
  if (refuel_airports){
    refuel_APs <- sort(unique(routes$refuel_ap))
    m <- m +
      geom_sf(data = ap_locations %>% filter(APICAO %in% refuel_APs),
              aes(geometry=prj(ap_locs, crs=crs)), colour=rap_col, size=rap_size)
  }

  #apply bounds?
  if (!is.na(routes) && bound){
    #crop based on the bounding box of all of the routes
    bbox <- st_bbox(prj(routes$gc, crs=crs)) +
      # c(-1,-1,1,1)*10 #zoomed box + 10deg
      c(-bound_margin_km,-bound_margin_km,bound_margin_km,bound_margin_km)*10^3 #zoomed box + 1000km - Robinson or orig
    m <- m +
      xlim(bbox$xmin, bbox$xmax) + ylim(bbox$ymin, bbox$ymax)
  }

  m <- m +
    theme(legend.position = "bottom")

  return(m)
}



#wrapper for st_wrap_dateline, because it needs lat-longs
st_wrap <- function(m){
  #4326 is in lat-long
  #this can be used wihtin a geom_sf plot statement
  st_transform(st_wrap_dateline(st_transform(m, crs=4326)),crs=st_crs(m))
}
