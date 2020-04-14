# grids
#
# grid-related functions



#' Create lat-long grid for routes
#'
#' \code{newLatLongGrid} Create, and optionally classify, a lat-long route grid
#'
#' This function creates a \code{gridLat} object that contains
#' a set of point on a lat long grid (ie all the points are on
#' lines of latitude). It also joins these points into a lattice.
#' Optionally, but required later, it classifies each link as land, sea,
#' or transition, with reference to a given map (typically including a
#' coastal buffer).
#'
#' The definitions are:
#'
#' \itemize{
#'   \item land: both ends of the link are on land
#'   \item sea: both ends are on sea, and the link does not intersect the land
#'   \item transition: otherwise
#' }
#'
#' The length of the links will be around \code{target_km} or 50% larger
#' for the diagonal links.
#'
#' For more details see the help vignette:
#' \code{vignette("Supersonic Routing", package = "twospeed")}
#'
#' @param onMap MULTIPOLYGON map defining land regions
#' @param name Assigned to the name slot of the result
#' @param target_km Target length. Default 800km
#'     only to avoid accidentally starting heavy compute.
#'     30-50km would be more useful.
#' @param lat_min,lat_max Latitude extent of grid
#' @param long_min,long_max Longitude extend of grid
#' @param classify Whether to classify each link. Defaults to
#'     FALSE only to avoid accidentally starting heavy compute.
#'
#' @return \code{gridLat} object containing points and lattice.
#'
#' @examples
#' # do minimal version
#' ac <- expand_aircraft()
#'
#' # on-the-fly example
#' ac <- data.frame(id = "test", type = "test aircraft",
#'                  over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
#'                  arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
#' ac <- expand_aircraft(ac)
#'
#'
#' @import dplyr
#' @import sf
#'
#' @export
newLatLongGrid <- function(onMap, name,
                           target_km=800,
                           lat_min= -60.0, lat_max = 86.0,
                           long_min = -180.0, long_max = 179.95,
                           classify = FALSE){
  #with target_km you expect N/S to have around this distance, diagonals about 50% longer.
  tstart <- Sys.time()

  #initialise object
  g <- new("GridLat", name=name)

  lat_dec <- 3 #round lat-long to 2 decimal places
  long_fudge <- 10^(-(lat_dec + 2)) #avoid dropping a step in DIV

  # standardised levels for phase, include some not used in this function
  phases <- twospeed:::lattice_phases

  lat_step <- round(geosphere::destPointRhumb(c(0,0), 0, target_km * 1000)[1,2],
                    lat_dec) #latitude step, in degrees

  #data for both grid and lattice,
  ll_grid_seed <- data.frame(lat = round(seq(lat_min, lat_max, by = lat_step),
                                         lat_dec)) %>%
    mutate(long_dist_km = geosphere::distGeo(matrix(c(rep(0,n()),lat),ncol=2),
                                  matrix(c(rep(1,n()),lat),ncol=2))/1000,
           long_step = round(target_km/long_dist_km,
                             lat_dec)) %>%
    rowwise() %>%
    mutate(long = list(round(seq(long_min, long_max, by = long_step),
                             lat_dec)),
           last_long = last(unlist(long))) %>%
    ungroup() %>%
    #don't worry that this creates NAs at the ends - it's useful
    mutate(next_long_step = lead(long_step),
           #need these to handle the date line later
           next_last_long = lead(last_long)) %>%
    tidyr::unnest(c(long)) %>%
    mutate(id = row_number())

  #create the grid from this
  g@points <- ll_grid_seed %>%
    select(id, long, lat) %>%
    mutate(xy = st_cast(st_transform(st_sfc(st_multipoint(matrix(c(long, lat), ncol=2)),
                                            crs=4326),
                                     crs=st_crs(onMap)),'POINT'))
  if (getOption("quiet", default=0)>0) message("Made the grid:", round(Sys.time() - tstart, 1))

  #and also the lattice
  #we need 4 neighbours,  1- next one with the same lat and higher long (roughly E)
  #2, 3 & 4 the 3 with the next higher lat and nearest 3 longitudes either side of this long (roughly NW, N and NE)
  #add a 4th for safety.
  n_cand <- 3
  ll_lattice <- ll_grid_seed %>%
    group_by(id) %>%
    mutate(to_lat = list(round(c(lat + c(0, rep.int(lat_step, n_cand))),lat_dec)),
           nlong_cands = list(round(seq.int(long_min, long_max,
                                            by = coalesce(next_long_step,1.0)),
                                    lat_dec)),
           lc_diff = list(abs(unlist(nlong_cands) - long)),
           lc_sel = list(which(rank(unlist(lc_diff)) <= n_cand)),
           to_long = list(round(c(long_min + ((long+long_fudge - long_min) %/% long_step + 1)*long_step,
                                unlist(nlong_cands)[unlist(lc_sel)[1]],
                                unlist(nlong_cands)[unlist(lc_sel)[2]],
                                unlist(nlong_cands)[unlist(lc_sel)[3]]),lat_dec)),
           long_wrap = list(c(last_long, rep.int(next_last_long, n_cand)))) %>%
    select(-nlong_cands, -lc_diff, -lc_sel)  %>%
    tidyr::unnest(c(to_lat, to_long, long_wrap)) %>%
    filter(!is.na(to_long)) %>% #possible cases
    ungroup() %>%
    filter(to_lat>= lat_min & to_lat<=lat_max) %>%
    #handle the date line
    #wrap is useful for plotting
    mutate(wrap = (to_long >= last_long | to_long <= long_min),
           to_long = case_when(
             to_long > long_max ~ long_min,
             to_long < long_min ~ long_wrap,
             TRUE ~ to_long
           )) %>%
    #tidy up
    rename(from_long=long, from_lat=lat, from=id) %>%
    # select(from, from_long, from_lat, to_long, to_lat) %>%
    #get the to id
    left_join(ll_grid_seed %>% select(id, long, lat) %>% rename(to=id),
              by=c("to_long"="long", "to_lat"="lat"))
  if (getOption("quiet",default=0)>0) message("Made the basic lattice:",round(Sys.time() - tstart,1))

  g@lattice <- ll_lattice %>%
    # add geometry
    rowwise() %>%
    mutate(geometry = st_transform(st_sfc(st_linestring(matrix(c(from_long, from_lat, to_long, to_lat),
                                                               ncol=2, byrow = TRUE)),
                                          crs=4326),
                                   crs=st_crs(onMap))) %>%
    ungroup() %>%
    #calculate distance
    mutate(dist_km = geosphere::distGeo(matrix(c(from_long, from_lat), ncol=2),
                             matrix(c(to_long, to_lat), ncol=2))/1000) %>%
    select(from, to, geometry, dist_km, wrap)
  if (getOption("quiet", default=0)>0) message("Added geo & distance to the lattice:",round(Sys.time() - tstart,1))

  #classify the points as sea or not
  if (classify){
    g@points$land <- as.vector(st_within(g@points$xy, onMap, sparse=FALSE, prepared = TRUE))
    id_land <- g@points %>%
      as.data.frame() %>%
      select(id, land)
  }

  #classify the lines in the lattice by their intersection
  #st_intersects is faster than st_within
  #simpler than old version, since only pure-sea and sea-land is of real interest
  if (classify) {
    if (getOption("quiet", default=0)>0) message("Classifying lines in the lattice as land.")
    g@lattice$Xland <- as.vector(st_intersects(g@lattice$geometry, onMap, sparse=FALSE, prepared = TRUE))
    if (getOption("quiet",default=0)>0) message("Classified as land:", round(Sys.time() - tstart,1))
    z <- g@lattice %>%
      left_join(id_land, by=c("from"="id")) %>%
      left_join(id_land, by=c("to"="id"), suffix=c("_1","_2")) %>%
      mutate(ph = case_when(
        !land_1 & !land_2 & !Xland ~ "sea",
        land_1 & land_2 ~ "land",
        TRUE ~ "transition"
      ),
      phase = factor(ph, levels = phases))
    if (getOption("quiet",default=0)>0) message("Calculated all phases:",round(Sys.time() - tstart,1))
    g@lattice<- g@lattice %>%
      mutate(phase = z$phase) %>%
      select(-Xland)
  }

  return(g)

}
