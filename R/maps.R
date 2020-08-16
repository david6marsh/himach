# maps
#
# Functions for plotting nice maps of routes

#' Wrap the 'dateline' before \code{st_transform}
#'
#' \code{st_slice_transform} handles the 'far side' break first, then
#' \code{st_tranform}
#'
#' \code{\link[sf:st_transform]{st_wrap_dateline}} should handle the break in a
#' map projection but uses `GDAL` for this. Given persistent issues in
#' installing  GDAL, \code{st_slice_transform} achieves the same, at least for
#' Atlantic to Pacific, without needing GDAL. This is only needed for
#' polygons, otherwise \code{\link[sf:st_transform]{st_wrap_dateline}} works
#' without the GDAL issues.
#'
#' Here 'simple' means, with a dateline that is a single line of longitude: ie
#' the proj4string contains either "longitude_of_center", so the dateline is
#' that +180; or not, in which case it assumes the "longitude_of_center" is 0.
#'
#' Caution: Use other than for \code{crs_Atlantic} to \code{crs_Pacific} is not
#' guaranteed! Check by plotting!
#'
#'
#' @param m A map dataframe, ie of class \code{sf} and \code{data.frame}, or an
#'   \code{sfc_MULTIPOLYGON}
#' @param new_crs Destination coordinate reference system, as in \code{st_tranform}
#' @param n_pts Number of steps used in creating the slice (per side).
#' @param margin Any polygons that cross the dateline lose a small margin, for
#'   safety
#'
#' @return \code{sf} dataframe, same as the parameter \code{m}
#'
#' @import sf
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' world <- sf::st_as_sf(rnaturalearthdata::coastline110)
#' w_pacific <- st_slice_transform(world, crs_Pacific)
#' ggplot2::ggplot(w_pacific) + ggplot2::geom_sf()
#'
#' # bad - not run - dateline problem example
#' # ggplot2::ggplot(st_transform(world, crs_Pacific)) +
#' #   ggplot2::geom_sf()
#'
#' @export
st_slice_transform <- function(m, new_crs = crs_Pacific,
                       n_pts = 30, margin = 0.05){
  crs = st_crs(m)
  # quick exit if nothing to change
  if (crs == st_crs(new_crs) | long_cent(m) == long_cent(new_crs)) return(m)

  # slice centred for the 'new' crs
  longit <- mod_long(long_cent(new_crs) + 180)

  longit <- longit - margin/2
  long2 <- longit + margin/2
  latN <- 89.9
  latS <- -89.9
   pts <- bind_rows(
    data.frame(lo = longit, la = seq(latN, latS, length.out = n_pts)),
    data.frame(lo = seq(longit, long2, length.out = n_pts %/% 3), la = latS),
    data.frame(lo = long2, la = seq(latS, latN, length.out = n_pts)),
    data.frame(lo = seq(long2, longit, length.out = n_pts %/% 3), la = latN)) %>%
    mutate(lo = mod_long(.data$lo)) %>%
    #remove sequential repeats
    filter(row_number() == 1 |
             !(.data$lo == lag(.data$lo) & .data$la == lag(.data$la)))
 sl <- st_transform(
    st_sfc(st_polygon(list(as.matrix(pts))), crs = crs_longlat),
    crs = crs)
 # remove the slice
  # transform to new crs
 suppressWarnings(suppressMessages({
   m_less <- st_difference(m, sl)
   y <- st_transform(m_less, crs = new_crs)
 }))
 return(y)
}



#wrapper around ggplot/geom_sf for quick map
plot_map <- function(msf,
                     c_land = "green1",
                     c_border = "grey70",
                     w_border = 0.1){
  ggplot2::ggplot(msf) +
    ggplot2::geom_sf(size = w_border, colour = c_border, fill = c_land) +
    # coord_sf() + #not needed for already-projected feature sets
    ggplot2::theme_minimal()
}


# achievable range from a point
# not used for route finding
make_range_envelope <- function(ac, ap, ap_locs = make_airports(),
                          envelope_points=70){
  #range envelope shows how far from an airport you can go  with a given range
  #create no-wind range ellipse

  ap_loc <- ap_locs %>%
    filter(APICAO == ap)
  # centre of range
  geo_c <- c(ap_loc$long, ap_loc$lat)

  # use CRS centred on cetnre of route envelopes
  cen_prj <- sp::CRS(paste0("+proj=laea +lat_0=", round(geo_c[2],1),
                            " +lon_0=", round(geo_c[1],1),
                            " +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
  dist <- ac$range_km * 1000
  theta <- seq(0, 360, length.out = envelope_points)

  geod <- geosphere::geodesic(geo_c, theta, dist)

  # convert to simple feature
  pg <- st_multipoint(geod[,1:2]) %>%
    st_sfc(crs=crs_longlat) %>%
    st_cast('LINESTRING') %>%
    st_cast('POLYGON') %>%
    st_transform(cen_prj) %>%
    # occasionally fails as self-intersection when later st_intersection
    # so this should solve that
    st_make_valid()
}


#' Map a set of routes
#'
#' \code{map_routes} plots routes, with many options
#'
#' This function plots the routes, with options for additional layers. Multiple
#' routes are expected, and they can be coloured by time advantage, by speed
#' along each segment, or by aircraft type.
#'
#' The option \code{show_route} "time" requires 'advantage_h' to have been added to
#' the routes set, from the route summary. If it hasn't then this is done in a local
#' version, then discarded. Running \code{summarise_routes} to do this requires an
#' airport dataset; if \code{is.na(ap_loc)} then this is not available, so a default set
#' is used. You can turn on \code{warn} to see if this is happening, but by default it
#' is silent.
#'
#' @param thin_map The minimum is a \code{MULTIPOLYGON} map, 'thin' in
#'   that it is without buffer, so a normal coastline map.
#' @param routes as generated by \code{\link{find_route}}
#' @param crs Coordinate reference system, default \code{crs_Atlantic}.
#' @param show_route  one of "speed", "aircraft", "time", "circuity" to
#'   indicate what goes in the legend.
#' @param fat_map optional coast + buffer map, default NA.
#' @param avoid_map optional map of no-fly zones, default NA.
#' @param ap_loc Show used origin and destination airports if this
#'   is a set of airports from \code{\link{make_airports}}, or not if NA
#'   (default). This dataset can be all airports, and is filtered to those used
#'   by \code{routes}.
#' @param ap_col,ap_size Colour and size of used airport markers (dark
#'   blue, 0.4)
#' @param refuel_airports Show the used refuel airports using these
#'   locations, or nothing if NA. (Defaults to same as \code{ap_loc}.)
#' @param rap_col,rap_size Colour and size of refuel airport markers
#'   (red, 0.4)
#' @param crow,crow_col,crow_size If TRUE, show the 'crow-flies' direct
#'   great circle, in colour \code{crow_col} and thickness \code{crow_size}.
#'   Default FALSE, "grey70", 0.2
#' @param route_envelope show the route envelope (default FALSE).
#' @param e_col,e_alpha,e_size colour, alpha and width for the range
#'    envelope. Default "grey70", 0.4, 0.6
#' @param bound,bound_margin_km If bound=TRUE (default) crop to bounding
#'   box of the \code{routes}, with additional \code{bound_margin_km} in km
#'   (default 200)
#' @param land_f,buffer_f,avoid_f fill colours for thin, fat and no-fly
#'   maps, default grey 90, 70 and 80, respectively
#' @param l_alpha,l_size line (route) settings for alpha (transparency)
#'   and width, defaults 0.6 and 0.4.
#' @param scale_direction Passed to scale_colour_viridis, either -1 (default) or
#'   or 1.
#' @param title,subtitle Passed to ggplot.
#' @param warn if TRUE show some warnings (when defaults loaded) (default FALSE)
#'
#' @return Dataframe with details of the leg
#'
#' @import sf
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @examples
#' #see introductory vignette
#'
#' @export
map_routes <- function(
  thin_map, routes=NA, crs=crs_Atlantic, show_route="time",
  fat_map=NA, avoid_map=NA,
  ap_loc=NA, ap_col="darkblue", ap_size=0.4,
  crow=FALSE, crow_col="grey70", crow_size=0.2,
  route_envelope=FALSE,
  bound=TRUE, bound_margin_km=200,
  land_f="grey90", buffer_f="grey60", avoid_f="grey80",
  l_alpha=0.8, l_size=0.5,
  e_alpha=0.4, e_size=0.6, e_col="grey70",
  refuel_airports=ap_loc, rap_col="red", rap_size=0.4,
  scale_direction = -1,
  title = "", subtitle = "",
  warn = FALSE
){
  (stopifnot(is.na(show_route) || show_route %in% c("speed","aircraft","time", "circuity")))

  #only support crs_Atlantic for the moment
  # stopifnot(st_crs(crs) == st_crs(crs_Atlantic))

  # remove the non-routes (have time = NA)
  # these are where refuelling was needed
  if (is.data.frame(routes)) routes <- routes %>% filter(!is.na(time_h))

  #thin map is the one without buffer
  thin_map <- st_slice_transform(thin_map, new_crs=crs) #force to CRS used for this map

  #layer 1 (one or two base maps)
  if (is.na(fat_map)) {
    m <- plot_map(thin_map, c_border=NA, c_land=land_f)
  } else {
    m <- plot_map(st_slice_transform(fat_map, new_crs=crs),
                  c_border=NA, c_land=buffer_f) +
      geom_sf(data=thin_map, fill=land_f, colour=NA)
  }

  #layer 2 (no fly-zone)
  if (!is.na(avoid_map)){
    m <- m +
      geom_sf(data = prj(avoid_map, crs=crs), colour=avoid_f, fill=avoid_f)
  }

  # 3: prelim: check if summarise_routes has been run
  # by seeing if 'advantage_h' has been calculated
  if (is.data.frame(routes)) {
    #remove empty routes
    routes <- routes %>%
      filter(!is.na(time_h))

    if ( !("advantage_h" %in% names(routes))) {
      if (warn) message("Adding advantage_h temporarily to the routes set.")
      airports <- ap_loc
      if (!is.data.frame(airports)){
        airports <- make_airports(warn = FALSE)
        if (warn) message("Using default airport set for temporary route summary.")
      }
      rtes <- summarise_routes(routes, airports)
      routes <- routes %>%
        left_join(rtes %>% select(.data$fullRouteID, .data$advantage_h, .data$circuity),
                  by = "fullRouteID") %>%
        arrange(.data$advantage_h)

      routes$gc <- st_wrap(routes$gc, new_crs=crs)
      routes <- st_set_geometry(routes, "gc") # default geometry for plots
    }

    #layer 3 (main lines)
    if (show_route=="time"){
      m <- m +  labs(colour = "Time Advantage") +
        geom_sf(data = routes,
                aes(colour = .data$advantage_h),
                fill="white",
                size=l_size, lineend="round", alpha=l_alpha) +
        scale_colour_viridis_c(direction = scale_direction)
    }
    if (show_route=="circuity"){
      m <- m +  labs(colour = "Circuity\n(best=0)") +
        geom_sf(data = routes,
                aes(colour = .data$circuity), fill = "white",
                size=l_size, lineend="round", alpha=l_alpha) +
        scale_colour_viridis_c(direction = scale_direction,
                               labels = scales::percent)
    }
    if (show_route == "speed"){
      m <- m +  labs(colour = "Average speed on segment (kph)") +
        geom_sf(data = routes,
                aes(colour = .data$speed_kph),
                size=l_size, lineend="round", alpha=l_alpha) +
        scale_colour_viridis_c(direction = scale_direction)
    }
    if (show_route == "aircraft"){
      m <- m +  labs(colour = "Aircraft") +
        geom_sf(data = routes,
                aes(colour = .data$acID),
                size=l_size, lineend="round", alpha=l_alpha,
                show.legend = "line")+
        scale_colour_viridis_d(direction = scale_direction)
    }
  }

  #layer 4: crow-flies
  if (crow){
    m <- m +
      geom_sf(data = st_wrap(routes$crow, new_crs = crs),
              colour=crow_col, size = crow_size)
  }

  #layer 5: range envelope
  if (route_envelope){
    m <- m +
      geom_sf(data = st_wrap(st_cast(routes$envelope, 'MULTILINESTRING'), new_crs=crs),
              fill = NA,
              colour = e_col, alpha = e_alpha, size = e_size)
  }

  #layer 6: airports
  if (is.data.frame(ap_loc)){
    used_APs <- sort(unique(unlist(lapply(unique(routes$routeID),
                                     function(x)strsplit(x, "<>")))))
    APs <- ap_loc %>%
      filter(.data$APICAO %in% used_APs)
    APs$ap_locs <- prj(APs$ap_locs, crs = crs)
    APs <- st_set_geometry(APs, "ap_locs")

    m <- m +
      geom_sf(data = APs,
              colour=ap_col, size=ap_size)
  }

  #layer 7: refuel airports
  if (is.data.frame(refuel_airports)){
    used_refuel_APs <- sort(unique(routes$refuel_ap))
    rAPs <- refuel_airports %>%
      filter(.data$APICAO %in% used_refuel_APs)
    rAPs$ap_locs <- prj(rAPs$ap_locs, crs = crs)
    rAPs <- st_set_geometry(rAPs, "ap_locs")

    m <- m +
      geom_sf(data = rAPs,
              colour=rap_col, size=rap_size)
  }

  #apply bounds?
  if (is.data.frame(routes) && bound){
    #crop based on the bounding box of all of the routes
    bbox <- st_bbox(prj(routes$gc, crs=crs)) +
      # c(-1,-1,1,1)*10 #zoomed box + 10deg
      c(-1,-1,1,1) * bound_margin_km * 10^3 #zoomed box + 1000km
    m <- m +
      xlim(bbox$xmin, bbox$xmax) + ylim(bbox$ymin, bbox$ymax)
  }

  m <- m +
    labs(title = title, subtitle = subtitle) +
    theme(legend.position = "bottom")

  return(m)
}



# wrapper for st_wrap_dateline, because it needs lat-longs
# works for multilines without GDAL problems
st_wrap <- function(m, new_crs){
  #this is for use within a geom_sf plot statement
  stopifnot(st_is_longlat(m)) # should be called with gc which is in longlat

  m %>%
    st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
    st_transform(crs = new_crs)
  # merid <- long_cent(new_crs)
  # # rotate (to apply wrap dateline at 180)
  # m_minus <- mod_long(m - c(merid, 0))
  # attr(m_minus, "crs") <- attributes(m)$crs #reset the crs
  # # wrap dateline at the new 180
  # m_minus <- st_wrap_dateline(m_minus,
  #                             options = c("WRAPDATELINE=YES"))
  # # rotate back
  # m <- mod_long(m_minus + c(merid, 0))
  # attr(m, "crs") <- attributes(m_minus)$crs
  # st_transform(m, crs=new_crs)

}


#wrapper for st_bbox that always returns lat-longs
st_bbox_longlat <- function(m){
  bb <- st_bbox(m)
  if (!st_is_longlat(bb)) {
    bb <- st_bbox(st_transform(m,
                       crs = crs_longlat))
  }
  return(bb)
}

#find the longitude of centre
long_cent <- function(m){
  #full text version of crs
  tx <- st_as_text(st_crs(m))
  #search as vector
  txv <- t(stringr::str_split(tx,",", simplify = TRUE))
  txv <- stringr::str_remove_all(txv, "\\]") #get rid of these
  # centre is made up of primemeridiand and long of center if either exists
  w <- which(stringr::str_detect(txv, "PRIMEM"))
  if (length(w) == 0){
    # if not mentioned, default to 0
    pm <- 0
  } else {
    pm <- as.numeric(txv[w + 1])
  }

  w <- which(stringr::str_detect(txv, "longitude_of_center"))
  if (length(w) == 0){
    # if not mentioned, default to 0
    c_long <- 0
  } else {
    c_long <- as.numeric(txv[w + 1])
  }
  pm + c_long
}
