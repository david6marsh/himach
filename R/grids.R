# grids
#
# grid-related functions

utils::globalVariables(c("."))

# round longitude back to -180 <= x < 180
mod_long <- function(x){
  (x + 180) %% 360 - 180
}

#' Make lat-long grid for route finding
#'
#' \code{make_route_grid} creates, and optionally classifies, a lat-long route grid
#'
#' This function creates a \linkS4class{GridLat} object that contains
#' a set of point on a lat long grid (ie all the points are on
#' lines of latitude). It also joins these points into a lattice.
#' Optionally, but required later, it classifies each link as land, sea,
#' or transition, with reference to a given map (typically including a
#' coastal buffer).
#'
#' The definitions are
#'
#' \itemize{
#'   \item land: both ends of the link are on land
#'   \item sea: both ends are on sea, and the link does not intersect the land
#'   \item transition: otherwise
#' }
#'
#' The length of the links will be around \code{target_km} or 50pct longer
#' for the diagonal links.
#'
#' For more details see the help vignette:
#' \code{vignette("Supersonic Routing", package = "himach")}
#'
#' @param fat_map MULTIPOLYGON map defining land regions
#' @param name String assigned to the name slot of the result
#' @param target_km Target length. Default 800km
#'     only to avoid accidentally starting heavy compute.
#'     30-50km would be more useful.
#' @param lat_min,lat_max Latitude extent of grid
#' @param long_min,long_max Longitude extend of grid.
#'     Two allow small grids crossing the 180 boundary, the function
#'     accepts values outside [-180,180), then rounds to within this
#'     range.
#' @param classify Whether to classify each link. Defaults to
#'     FALSE only to avoid accidentally starting heavy compute.
#'
#' @return \code{gridLat} object containing points and lattice.
#'
#' @examples
#' NZ_buffer <- hm_get_test("buffer")
#' system.time(
#'   p_grid <- make_route_grid(NZ_buffer,"NZ lat-long at 300km",
#'                            target_km = 300, classify = TRUE,
#'                            lat_min = -49, lat_max = -32,
#'                            long_min = 162, long_max = 182)
#' )
#'
#' @import dplyr
#' @import sf
#'
#' @export
make_route_grid <- function(fat_map, name,
                           target_km=800,
                           lat_min= -60.0, lat_max = 86.0,
                           long_min = -180.0, long_max = 179.95,
                           classify = FALSE){
  #with target_km you expect N/S to have around this distance, diagonals about 50% longer.
  tstart <- Sys.time()

  #initialise object
  g <- methods::new("GridLat", name=name)

  lat_dec <- 3 #round lat-long to 2 decimal places
  long_fudge <- 10^(-(lat_dec + 2)) #avoid dropping a step in DIV

  # standardised levels for phase, include some not used in this function
  phases <- lattice_phases

  lat_step <- round(geosphere::destPointRhumb(c(0,0), 0, target_km * 1000)[1,2],
                    lat_dec) #latitude step, in degrees

  #data for both grid and lattice,
  ll_grid_seed <- data.frame(lat = round(seq(lat_min, lat_max, by = lat_step),
                                         lat_dec)) %>%
    mutate(long_dist_km = geosphere::distGeo(matrix(c(rep(0,n()), .data$lat),ncol=2),
                                  matrix(c(rep(1,n()), .data$lat),ncol=2))/1000,
           long_step = round(target_km/.data$long_dist_km,
                             lat_dec)) %>%
    rowwise() %>%
    mutate(long = list(round(seq(long_min, long_max, by = .data$long_step),
                             lat_dec)),
           last_long = last(unlist(.data$long))) %>%
    ungroup() %>%
    #don't worry that this creates NAs at the ends - it's useful
    mutate(next_long_step = lead(.data$long_step),
           #need these to handle the date line later
           next_last_long = lead(.data$last_long)) %>%
    tidyr::unnest(c(.data$long)) %>%
    mutate(id = row_number())

  #create the grid from this
  g@points <- ll_grid_seed %>%
    select(.data$id, .data$long, .data$lat) %>%
    # handle the 'overflow longitude' - slightly over the dateline
    mutate(long = mod_long(.data$long)) %>%
    mutate(xy = st_cast(st_transform(st_sfc(st_multipoint(matrix(c(.data$long, .data$lat), ncol=2)),
                                            crs=4326),
                                     crs=st_crs(fat_map)),'POINT'))
  if (getOption("quiet", default=0)>0) message("Made the grid:", round(Sys.time() - tstart, 1))

  #and also the lattice
  #we need 4 neighbours,  1- next one with the same lat and higher long (roughly E)
  #2, 3 & 4 the 3 with the next higher lat and nearest 3 longitudes either side of this long (roughly NW, N and NE)
  #add a 4th for safety.
  # wrap this in a split - map - map_dfr framework, and show tick on the first mutate statement
  # big ll_grid_seed (30km) has 500,000 elements, so 1,000 chunks should be plenty
  if (getOption("quiet",default=0)>0) message("Making the basic lattice:")
  sample_n <- nrow(ll_grid_seed)
  grp_size <- 1000
  # pb <- progress_estimated(sample_n , min_time = 3)
  pb <- progress::progress_bar$new(total = sample_n, format = "[:bar] :percent :eta")

  n_cand <- 3
  ll_lattice <- ll_grid_seed %>%
    ungroup() %>%
    mutate(grp  = row_number() %/% grp_size)  %>%
    split(.$grp) %>%
    purrr::map(
      ~ group_by(., .data$id) %>%
        mutate(to_lat = list(round(c(.data$lat + c(0,
                                             withProgress(pb,
                                                          rep.int,
                                                          lat_step,
                                                          n_cand))),
                                   lat_dec)),
               nlong_cands = list(round(seq.int(long_min, long_max,
                                                by = coalesce(next_long_step,1.0)),
                                        lat_dec)),
               lc_diff = list(abs(unlist(nlong_cands) - .data$long)),
               lc_sel = list(which(rank(unlist(lc_diff)) <= n_cand)),
               to_long = list(round(c(long_min + ((.data$long+long_fudge - long_min) %/% .data$long_step + 1)*.data$long_step,
                                      unlist(nlong_cands)[unlist(lc_sel)[1]],
                                      unlist(nlong_cands)[unlist(lc_sel)[2]],
                                      unlist(nlong_cands)[unlist(lc_sel)[3]]),lat_dec)),
               long_wrap = list(c(.data$last_long, rep.int(next_last_long, n_cand)))) %>%
        select(-nlong_cands, -lc_diff, -lc_sel)  %>%
        tidyr::unnest(c(.data$to_lat, .data$to_long, long_wrap)) %>%
        filter(!is.na(.data$to_long)) %>% #possible cases
        ungroup() %>%
        filter(.data$to_lat>= lat_min & .data$to_lat<=lat_max) %>%
        #handle the date line
        #wrap is useful for plotting
        mutate(wrap = (.data$to_long >= .data$last_long | .data$to_long <= long_min),
               to_long = case_when(
                 .data$to_long > long_max ~ long_min,
                 .data$to_long < long_min ~ long_wrap,
                 TRUE ~ .data$to_long
               )) %>%
        #tidy up
        rename(from_long=.data$long, from_lat=.data$lat, from=.data$id) %>%
        # select(from, from_long, from_lat, to_long, to_lat) %>%
        #get the to id
        left_join(ll_grid_seed %>% select(.data$id, .data$long, .data$lat) %>% rename(to=.data$id),
                  by=c("to_long"="long", "to_lat"="lat"))
    ) %>%
    purrr::map_dfr(~ as.data.frame(.)) %>%
    select(-.data$grp)
  if (getOption("quiet",default=0)>0) message("") #new line


  if (getOption("quiet", default=0)>0) message("Adding geo & distance to the lattice...")
  pb <- progress::progress_bar$new(total = nrow(ll_lattice),
                                   format = "[:bar] :percent :eta")
  g@lattice <- ll_lattice %>%
    # add geometry
   rowwise() %>%
    mutate(geometry = s2::s2_make_line(c(.data$from_long, .data$to_long),
                                       c(.data$from_lat, .data$to_lat))) %>%
    ungroup() %>%
    mutate(dist_km = s2::s2_length(.data$geometry)/1000) %>%
    mutate(geometry = sf::st_as_sfc(.data$geometry),
           geometry = sf::st_transform(.data$geometry, crs = st_crs(fat_map))) %>%
    select(.data$from, .data$to, .data$geometry, .data$dist_km, .data$wrap)
  message("")
  if (getOption("quiet", default=0)>0) message("Added geo & distance to the lattice:",round(Sys.time() - tstart,1))

  #classify the points as sea or not
  if (classify){
    if (getOption("quiet", default=0)>0) message("Classifying points in the lattice as land.")
    g@points$land <- as.vector(st_within(g@points$xy, fat_map, sparse=FALSE, prepared = TRUE))
    if (getOption("quiet",default=0)>0) message("Classified as land:", round(Sys.time() - tstart,1))
    id_land <- g@points %>%
      as.data.frame() %>%
      select(.data$id, .data$land)
  }

  #classify the lines in the lattice by their intersection
  #st_intersects is faster than st_within
  #simpler than old version, since only pure-sea and sea-land is of real interest
  if (classify) {
    if (getOption("quiet", default=0)>0) message("Classifying lines in the lattice as land.")
    g@lattice$Xland <- as.vector(st_intersects(g@lattice$geometry, fat_map, sparse=FALSE, prepared = TRUE))
    if (getOption("quiet",default=0)>0) message("Classified as land:", round(Sys.time() - tstart,1))
    z <- g@lattice %>%
      left_join(id_land, by=c("from"="id")) %>%
      left_join(id_land, by=c("to"="id"), suffix=c("_1","_2")) %>%
      mutate(ph = case_when(
        !.data$land_1 & !.data$land_2 & !.data$Xland ~ "sea",
        .data$land_1 & .data$land_2 ~ "land",
        TRUE ~ "transition"
      ),
      phase = factor(.data$ph, levels = phases))
    if (getOption("quiet",default=0)>0) message("Calculated all phases:",round(Sys.time() - tstart,1))
    g@lattice<- g@lattice %>%
      mutate(phase = z$phase) %>%
      select(-.data$Xland)
  }

  if (getOption("quiet", default=0)>0) message("Converting points and lattice to data table.")
  g@points <- data.table::as.data.table(g@points)
  g@lattice <- data.table::as.data.table(g@lattice)

  return(g)

}
