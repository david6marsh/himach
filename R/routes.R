# routes
#
# functions for finding routes on a grid

utils::globalVariables(c("crs_longlat"))

#time in phase for an aircraft to  cover distance
time_h <- function(ph, d_km, ac){
  #calculate travel time
  #this will potentially be different to the lattice,
  #where all the cost of transition has to be on a single leg

  case_when(
    ph == "land" ~ d_km/ac$over_land_kph,
    ph == "sea" ~ d_km/ac$over_sea_kph,
    TRUE ~ d_km/ac$trans_kph + ac$trans_h)
}

#assign aircraft-specific costs to a lattice
costLattice <- function(route_grid,ac){
  #given a GridLat and an Aircraft return a datatable lattice with costs
  stopifnot(class(route_grid)=="GridLat")

  cl <- route_grid@lattice %>%
    mutate(cost = case_when(
      phase == "land" ~ dist_km/ac$over_land_kph,
      phase == "sea" ~ dist_km/ac$over_sea_kph,
      TRUE ~ dist_km/ac$trans_kph + ac$trans_h)) %>%
    data.table::as.data.table()
}

distFromLand <- function(long, lat, land){
  #return a distance for each point from land, in km
  #long and lat are vectors, land is a map (sfc_MULTIPOLYGON)
  if (is.na(land)) return(0)
  mp <- st_transform(st_cast(st_sfc(
    st_multipoint(matrix(c(long, lat),ncol=2)), crs = crs_longlat), 'POINT'),
    crs=st_crs(land))
  as.vector(st_distance(mp, land))/1000
}


#this will be called recursively
# for trans-Pacific, depth of > 150 is quite possible
#
findGC <- function(subp, withMap_s2, avoidMap_s2, max_depth = 250){

  if(getOption("quiet", default=0)>2) message("  ",first(subp$phase), " ",
                                             first(subp$phaseID), "  ", nrow(subp))
  # check if recursed too far
  too_deep <- FALSE
  if(nchar(first(subp$phaseID)) > max_depth) {
    too_deep <- TRUE
    warning("Gone too deep at: ", first(subp$phaseID), "for", first(subp$id))
  }

  #if is.na(avoidMap_s2) then a simplified approach for the non-sea phases
  #for sea phases use withMap_s2 (avoiding land & avoid areas, already union)
  #for other phases use avoidMap_s2
  if (first(subp$phase) != "sea") useMap <- avoidMap_s2
  else useMap <- withMap_s2

  #land or transition, we assume a single GC
  #sea, but single step, then the same
  # if more than 10km from coast, then deep sea, and the splitting rule is not reliable
  #   - instead the shortcuts method is more reliable
  deep_sea <- min(subp$fromLand_km[2:(nrow(subp)-2)]) > 10
  if (too_deep |
      (first(subp$phase) != "sea" & is.na(useMap)) |
      nrow(subp) <= 2 |
      deep_sea){
    if (nrow(subp)==1 | deep_sea) subp %>% select(.data$phase, .data$phaseID, .data$id)
    else {
      subp %>% summarise(phase = first(.data$phase),
                         phaseID = first(.data$phaseID),
                         id = first(.data$id))
    }
  }
  else {
    #here references to sea also stand for avoid areas on non-sea phases
    #does a gt circle from start to end still miss the land?
    test_line <- s2::s2_make_line(c(first(subp$long), last(subp$long)),
                                  c(first(subp$lat), last(subp$lat)))
    sea_only <- ! s2::s2_intersects(test_line, useMap)
    if (sea_only) {
      if (nrow(subp)==1) subp %>% select(.data$phase, .data$phaseID, .data$id)
      else {
        subp %>% summarise(phase = first(.data$phase),
                           phaseID = first(.data$phaseID),
                           id = first(.data$id))
      }
    }
    else {
      #do recursion.
      #split on first step at furthest from Gt circle
      # and after that where closest to land, excluding start and end
      #except if length is 3 when it has to the middle
      split <-  case_when(
        nrow(subp)==3 ~ 2,
        # TRUE ~ which.max(d) *1.0) #pure distance from GC
        # nchar(subp[1,]$phaseID)<5 ~ which.max(d)*1.0,
        TRUE ~ which.min(subp$fromLand_km[2:(nrow(subp)-2)]) + 1)
      #recurse - extending phaseID each time
      bind_rows( findGC(subp %>% slice(1:split) %>%
                          mutate(phaseID=paste0(.data$phaseID,"0")), withMap_s2, avoidMap_s2),
                 findGC(subp %>% slice(split:nrow(subp)) %>%
                          mutate(phaseID=paste0(.data$phaseID,"1")), withMap_s2, avoidMap_s2))
    }
  }
}


#return an empty-ish route
emptyRoute <- function(ac, ap2, fat_map,
                       levels=lattice_phases){

  data.frame(phase = factor(NA, levels = levels),
             phaseID = "0.",
             w_id = 0, from = 0, to = 0,
             time_h = NA_real_,
             from_long = ap2$from_long, from_lat = ap2$from_lat,
             to_long = ap2$to_long, to_lat = ap2$to_lat,

             gcdist_km = geosphere::distGeo(c(ap2$from_long, ap2$from_lat),
                                 c(ap2$to_long, ap2$to_lat))/1000,
             routeID = ap2$AP2,
             acID = ac$id,
             acType = ac$type,
             grid = NA,
             timestamp = format(Sys.time(), "%Y%m%d_%H%M%OS2"), #unique identifier
             speed_kph = NA,
             legs = NA, leg_id = NA,
             fullRouteID = ap2$AP2,
             refuel_ap = NA,
             stringsAsFactors = FALSE) %>%
    mutate(gc = st_sfc(st_linestring(), crs=crs_longlat), #for want of anything else NULL sfc
           crow = st_gcIntermediate(p1=c(ap2$from_long, ap2$from_lat),
                                    p2=c(ap2$to_long, ap2$to_lat),
                                    n = 30, addStartEnd=TRUE,
                                    crs=crs_longlat),
           envelope = st_sfc(st_polygon(), crs=crs_longlat))
}

#' Find best routes between airport-pair & aircraft combinations
#'
#' \code{find_routes} combines an aircraft and airport-pair list and finds the
#' best routes between them, refuelling if necessary
#'
#' This function finds is a wrapper for the single-case function
#' \code{find_route}. It takes (text) lists of aircraft and airport codes,
#' combines them, then finds routes for all of these. A 'route' is made up
#' of one or two 'legs' (airport to airport without intermediate stop).
#'
#' For more details see \code{\link{find_route}}
#'
#'
#' @param ac_ids A vector of aircraft IDs, as in column 'id' from
#'   \code{\link{make_aircraft}}
#' @param ap2_ids A 2-column matrix or dataframe of airport pair text IDs
#' @param aircraft Specification of the aircraft, see
#'   \code{\link{make_aircraft}}
#' @param airports Airport locations as from \code{\link{make_airports}}
#' @param ... Other parameters, passed to \code{\link{find_route}}.
#'
#'
#' @return Dataframe with details of the routes
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' # need to load some of the built-in data
#' aircraft <- make_aircraft(warn = FALSE)
#' airports <- make_airports(crs = crs_Pacific)
#' # get test datasets
#' NZ_buffer30 <- hm_get_test("buffer")
#' NZ_grid <- hm_get_test("grid")
#'
#' options("quiet" = 4) #for heavy reporting
#' # from Auckland to Christchurch
#' ap2 <- make_AP2("NZAA","NZCH",airports)
#' routes <- find_route(aircraft[4,],
#'                     ap2,
#'                     fat_map = NZ_buffer30,
#'                     route_grid = NZ_grid,
#'                     ap_loc = airports)
#'
#' @export
find_routes <- function(ac_ids, ap2_ids, aircraft, airports, ...){
  stopifnot(ncol(ap2_ids)==2)
  if (!is.data.frame(ap2_ids)) ap2_ids <- as.data.frame(ap2_ids, stringsAsFactors = FALSE)

  combos <- tidyr::crossing(ac_ids, ap2_ids)
  names(combos) <- c("ac","ap1","ap2")
  # pb <- progress_estimated(nrow(combos) , min_time = 3)
  pb <- progress::progress_bar$new(total = nrow(combos),
                                   format = "[:bar] :percent :eta")

  routes <- purrr::reduce(lapply(1:nrow(combos),
                                 function(x) {
                                   ac <- aircraft[aircraft$id == combos$ac[x], ]
                                   ap2 <- make_AP2(combos$ap1[x],
                                                   combos$ap2[x],
                                                   airports)
                                   r <- find_route(ac, ap2,
                                                   ap_loc = airports, ...)
                                   pb$tick()
                                   if (getOption("quiet", default=0)>0) message("") # new line
                                   return(r)
                                 }
  ),
  rbind)
  return(routes)
}


#' Find best route between 2 airports
#'
#' \code{find_route} finds the quickest route between two airports, refuelling
#' if necessary
#'
#' This function finds the quickest route between two airports. A 'route' is
#' made up of one or two 'legs' (airport to airport without intermediate stop).
#' \code{find_route} makes one or more calls to \code{find_leg} as required.
#'
#' It assumes that the routing grid, \code{route_grid}, has already been
#' classified as land or sea using the map \code{fat_map}. The map is further
#' used when converting the grid-based route to one of great circles segments.
#'
#'
#' @section Refuelling:
#'
#'   If either necessary, because the great circle distance is greater than the
#'   aircraft range, or because \code{refuel_only_if} is FALSE,
#'   \code{find_route} searches through a list of refuelling airports and
#'   chooses the quickest one (or \code{refuel_topN}).
#'
#'   Circuitous refuelling is avoided, tested against total distance <
#'   \code{max_circuity} * great circle distance. This is separate to the limits
#'   placed on circuity of individual legs in \code{\link{find_leg}}.
#'
#'   If no refuel option is found, a message is displayed. The route with `NA`
#'   for `time_h` is returned.
#'
#'   Each refuelling stop costs \code{refuel_h} in addition to the time to
#'   descend to the airport and then to climb out again.
#'
#' @param ac One aircraft, as from \code{\link{make_aircraft}}
#' @param ap2 One airport pair, as from \code{\link{make_AP2}}
#' @param fat_map \code{sf::MULTIPOLYGON} map of land, including buffer
#' @param avoid \code{sf::MULTIPOLYGON} map of areas not to fly over
#' @param route_grid \code{GridLat} routing grid as from
#'   \code{\link{make_route_grid}}
#' @param cf_subsonic Further aircraft to use as comparator, default NA. (use is
#'   not recommended)
#' @param refuel Airports available for refuelling, dataframe with \code{APICAO,
#'   long, lat}
#' @param refuel_h Duration of refuelling stop, in hours
#' @param refuel_only_if If TRUE (default) only test refuel options if necessary
#'   because the great circle distance is too far for the aircraft range
#' @param refuel_topN Return the best N (default 1) refuelling options
#' @param max_circuity Threshold for excluding refuelling stops (default 2.0)
#' @param ap_loc Airport locations as from \code{\link{make_airports}}
#' @param margin_km Great circle distance between airports must be less than
#'     aircraft range minus this operating margin (default 200km), to give
#'     a margin for arrival and departure.
#' @param ... Other parameters, passed to \code{\link{find_leg}} and thence to
#'   to \code{\link{make_route_envelope}}.
#'
#'
#' @return Dataframe with details of the route
#'
#'
#' @import cppRouting
#' @import sf
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' # need to load some of the built-in data
#' aircraft <- make_aircraft(warn = FALSE)
#' airports <- make_airports(crs = crs_Pacific)
#' # get test datasets
#' NZ_buffer30 <- hm_get_test("buffer")
#' NZ_grid <- hm_get_test("grid")
#'
#' options("quiet" = 4) #for heavy reporting
#' # from Auckland to Christchurch
#' ap2 <- make_AP2("NZAA","NZCH",airports)
#' routes <- find_route(aircraft[4,],
#'                     ap2,
#'                     fat_map = NZ_buffer30,
#'                     route_grid = NZ_grid,
#'                     ap_loc = airports)
#'
#' @export
find_route <- function(ac, ap2, fat_map, avoid=NA, route_grid, cf_subsonic=NA,
                      refuel=NA, refuel_h=1, refuel_only_if=TRUE,
                      refuel_topN=1,
                      max_circuity=2.0,
                      ap_loc,
                      margin_km = 200, ...){
  #cf_subsonic is either NA or a line from the ac dataframe, like ac itself
  #refuel is either NA or a dataframe list APICAO, lat, long at least, with ap_locs

  if (getOption("quiet", default=0)>0) message("Route:-", ap2$AP2,"----") #route header v refuel subroutes

  # # check for unknown airport
  # if (is.na(ap2$gcdist_km)){
  #   miss_ADEP <- ""
  #   miss_ADES <- ""
  #   if (!(ap2$ADEP %in% ap_loc$APICAO)) miss_ADEP <- ap2$ADEP
  #   if (!(ap2$ADES %in% ap_loc$APICAO)) miss_ADES <- ap2$ADES
  #   warning(paste("Unknown airport:",miss_ADEP,miss_ADES))
  #   return(emptyRoute(ac, ap2, fat_map))
  # }

  #note ap2$routeID is in a specific order, but ADEP/ADES might not reflect that
  #switch if necessary
  ap2 <- ap2 %>%
    select(.data$AP2, .data$ADEP, .data$ADES, ends_with("_long"), ends_with("_lat"), .data$gcdist_km) %>%
    mutate(ADEP= stringr::str_split(.data$AP2,"(<>)|(>)|(<)", simplify=TRUE)[1],
           ADES= stringr::str_split(.data$AP2,"(<>)|(>)|(<)", simplify=TRUE)[2])
  sep <- stringr::str_remove_all(ap2$AP2,paste0("(", ap2$ADEP,")|(", ap2$ADES,")")) #get separator "<>",">"
  unidirectional <- (sep==">")

  #can aircraft make it without refuelling?
  ap2range_ok <- ap2$gcdist_km < ac$range_km - margin_km

  #if yes - then get the route
  if (ap2range_ok) routes <- find_leg(ac, ap2, ap_loc = ap_loc,
                                             fat_map=fat_map, route_grid = route_grid, avoid = avoid, ...)
  else {
    if (getOption("quiet", default=0)>1) message(" Too far for one leg.")
    routes <- emptyRoute(ac, ap2, fat_map)
  }

  #do a parallel run for a subsonic aircraft - a true baseline?
  #not advised - just use the M084 estimate
  if (is.data.frame(cf_subsonic)) {
    if (getOption("quiet", default=0)>1) message(" Adding subsonic, without range bounds.")
    r_subsonic <- find_leg(cf_subsonic, ap2, ap_loc = ap_loc,
                                  enforce_range = FALSE,
                                  fat_map=fat_map, route_grid = route_grid, avoid=avoid, ...)
    routes <- rbind(routes, r_subsonic)
  }

  #look for options including refuelling
  if (is.data.frame(refuel) & (!ap2range_ok || !refuel_only_if)){
    #find triples: AREF in range of ADEP and ADES
    r_ap3 <- ap2 %>%
      tidyr::crossing(refuel %>%
                 select(.data$APICAO, .data$long, .data$lat) %>%
                 rename(AREF=.data$APICAO, ref_long=.data$long, ref_lat=.data$lat)) %>%
      rowwise() %>%
      mutate(dep_ref_km = geosphere::distGeo(c(.data$from_long, .data$from_lat),
                                  c(.data$ref_long, .data$ref_lat))/1000,
             ref_des_km = geosphere::distGeo(c(.data$ref_long, .data$ref_lat),
                                  c(.data$to_long, .data$to_lat))/1000) %>%
      ungroup() %>%
      filter(.data$dep_ref_km < ac$range_km - margin_km &
               .data$ref_des_km < ac$range_km - margin_km) %>%
      filter((.data$AREF != .data$ADEP) & (.data$AREF != .data$ADES)) %>% #can't refuel at start or end!
      #if 1 or more under circuity then filter
      mutate(circuity = (.data$dep_ref_km + .data$ref_des_km)/ap2$gcdist_km,
             n_under_circ = sum(.data$circuity < max_circuity)) %>%
      filter((.data$n_under_circ<2)|(.data$circuity < max_circuity))

    if (nrow(r_ap3) < 1) {
      message("No refuel options for ", ap2$AP2)
    } else {
      #simplify to distinct AP2 (there may be some duplicates, eg Heathrow-Gander appearing for -SFO & -LAX)
      r_ap2 <- r_ap3 %>%
        select(.data$ADEP, .data$AREF, .data$from_long, .data$from_lat, .data$ref_long, .data$ref_lat) %>%
        rename(ADES=.data$AREF, to_long=.data$ref_long, to_lat=.data$ref_lat) %>%
        rbind(r_ap3 %>%
                select(.data$AREF, .data$ADES, .data$ref_long, .data$ref_lat, .data$to_long, .data$to_lat) %>%
                rename(ADEP=.data$AREF, from_long=.data$ref_long, from_lat=.data$ref_lat)) %>%
        distinct() %>%
        rowwise() %>%
        mutate(gcdist_km = geosphere::distGeo(c(.data$from_long, .data$from_lat),
                                       c(.data$to_long, .data$to_lat))/1000,
               AP2=ifelse(unidirectional,
                          paste(.data$ADEP, .data$ADES,sep=sep),
                          paste_ADEPADES(.data$ADEP, .data$ADES, FALSE))) #get it in the 'right' order (for the cache)


      # w <- lapply(1:nrow(r_ap2), function(x) find_leg(ac, r_ap2[x, ], fat_map=fat_map, route_grid = route_grid, avoid=avoid,
      #                                                             unidirectional=unidirectional, ...))
      w <- lapply(1:nrow(r_ap2), function(x) find_leg(ac,
                                                     make_AP2(r_ap2[x, ]$ADEP, r_ap2[x, ]$ADES, ap_loc),
                                                     route_grid = route_grid, fat_map=fat_map, avoid=avoid,
                                                     ap_loc = ap_loc, ...))
      refuel_options <- purrr::reduce(w, rbind)

      #extract the best from these options
      #add some variables
      refuel_options <- refuel_options %>%
        mutate(legs = ifelse(.data$routeID==ap2$AP2, 1, 2),
               leg_id = ifelse(grepl(ap2$ADEP,.data$routeID), 1, 2),
               refuel_ap = stringr::str_remove_all(.data$routeID,
                                                   paste0("(", ap2$ADEP,")|(<>)|(>)|(", ap2$ADES,")")),
               routeID = ap2$AP2,
               fullRouteID = paste(ap2$ADEP, .data$refuel_ap, ap2$ADES,
                                   sep=sep))

      best_routes <- refuel_options %>%
        group_by(.data$fullRouteID) %>%
        summarise(time_h = sum(time_h),
                  dist_km = sum(.data$gcdist_km)) %>%
        filter(!is.na(time_h)) %>%  #drop any failed direct route
        top_n(-refuel_topN, time_h) %>%
        pull(.data$fullRouteID)

      #now re-order to get the sequence right (only really imporatnt for speed profile)
      best_refuel_options <-  refuel_options %>%
        filter(.data$fullRouteID %in% best_routes) %>%
        group_by(.data$fullRouteID, .data$leg_id) %>%
        #now check if need to reverse order
        #with refuelling we want the phases to be in the right order and join up in the middle
        mutate(grid_s = utils::head(.data$grid[[1]],1),
               grid_e = utils::tail(.data$grid[[1]],1),
               reverse = (.data$leg_id==1 & .data$grid_s != stringr::str_split(.data$routeID,"(<>)|(>)",simplify=TRUE)[1])|
                 (.data$leg_id==2 & .data$grid_e!= stringr::str_split(.data$routeID,"(<>)|(>)",simplify=TRUE)[2]),
               order = if_else(.data$reverse, -row_number(),row_number()),
               temp = .data$from,
               from = if_else(.data$reverse, .data$to, .data$from),
               to = if_else(.data$reverse, .data$temp, .data$to),
               reverse = .data$reverse & !(row_number()==n()), #don't reverse data in last row, since arr/dep leg is alway from AP
               temp= .data$from_lat,
               from_lat = if_else(.data$reverse, .data$to_lat, .data$from_lat),
               to_lat = if_else(.data$reverse, .data$temp, .data$to_lat),
               temp= .data$from_long,
               from_long = if_else(.data$reverse, .data$to_long, .data$from_long),
               to_long = if_else(.data$reverse, .data$temp, .data$to_long)) %>%
        arrange(.data$fullRouteID, .data$leg_id, order) %>%
        select(-.data$grid_s, -.data$grid_e, -.data$reverse, -.data$temp) #drop order later

      #base the refuel legs on the last row of the first leg - for lat long, etc
      refuel_legs <- best_refuel_options %>%
        group_by(.data$fullRouteID) %>%
        filter(.data$leg_id==1) %>%
        filter(row_number() == n()) %>%
        mutate(phase = factor("refuel", levels = lattice_phases),
               from = .data$to,
               time_h = refuel_h,
               from_long = .data$to_long, from_lat = .data$to_lat,
               gcdist_km = 0,
               speed_kph = 0,
               leg_id = 1.5)

      #need to use rbind not bind_rows because of the sf
      sel_routes <- rbind.data.frame(best_refuel_options, refuel_legs)
      sel_routes <-  sel_routes %>%
        # needed to add ungroup here with dplyr 1.0.0
        ungroup() %>%
        arrange(.data$fullRouteID, .data$leg_id, order) %>%
        group_by(.data$fullRouteID) %>%
        #renumber the phases
        mutate(phaseChange = case_when(
          row_number() == 1 ~ 1L,
          phase != lag(phase) ~ 1L,
          TRUE ~ 0L),
          phaseID = paste0(cumsum(.data$phaseChange), ".", stringr::str_split(.data$phaseID,
                                                                      stringr::coll("."),
                                                                      simplify=TRUE)[,2]),
          timestamp = first(.data$timestamp)) %>%
        select(-.data$phaseChange, -order)

      routes <- rbind.data.frame(routes, sel_routes)
    }

  }
  routes <- st_set_geometry(routes, "gc") #convert to sf
  return(routes)
}


#cached SID-STAR
findToCToD <- function(ap, route_grid, fat_map, ac,
                       ad_dist_m, ad_nearest){
  #the original findToCToD accepted multiline ap & route_grid - for ease of caching no longer true
  stopifnot(nrow(ap)==1 & nrow(ac)==1)

  #can save and load the cache, with loadRDS readRDS
  #use attr "map" of fat_map, and AircraftSet of ac to ensure it's the right cache
  #if cache doesn't exist, create it as a child of Global (so persists outside this function!)
  if ((attr(.hm_cache$star_cache,"map") != route_grid@name) ||
      (attr(.hm_cache$star_cache,"aircraftSet") != attr(ac,"aircraftSet"))) {
    if (getOption("quiet", default=0)>0) message("Map or aircraft have changed, so clearing star cache.")
    hm_clean_cache("star")
    attr(.hm_cache$star_cache,"map") <- route_grid@name
    attr(.hm_cache$star_cache,"aircraftSet") <- attr(ac,"aircraftSet")}

  #cache the SID-STAR with data name which is the ACID, ap, ad_nearest & ad_dist_m.
  cache_as <- paste(ac$id, ap$APICAO, ad_nearest, ad_dist_m, sep="-")

  #if this query has not already been cached, calculate its value
  if (!exists(cache_as, envir=.hm_cache$star_cache, inherits=F)) {
    if (getOption("quiet", default=0)>2) message("  TOC/TOD not cached: calculating...")
    assign(cache_as, findToCToD_really(ap, route_grid, fat_map, ac,
                                       ad_dist_m, ad_nearest), .hm_cache$star_cache)
  }
  #return value
  get(cache_as, .hm_cache$star_cache)
}

#link airport to top of climb and descent
findToCToD_really <- function(ap, route_grid, fat_map, ac,
                              ad_dist_m, ad_nearest){
  #for a each point in ap - which is the airport list with locs
  #find where the airport connects to the 'cruise' grid points
  #not super accurate - because use st_distance rathe than distGeo

  # these should match in the inputs
  use_crs <- st_crs(route_grid@lattice$geometry)
  stopifnot(st_crs(route_grid@points$xy) == use_crs)
  # these are fast, so transform them rather than assume
  if (st_crs(fat_map) != use_crs) st_transform(fat_map, use_crs)
  ap$ap_locs <- st_transform(ap$ap_locs, crs=use_crs, quiet=FALSE)

  y <- as.matrix(st_distance(route_grid@points$xy, ap$ap_locs)) #slow but simple
  #and less slow now that this is route_grid after the route envelope has been applied

  colnames(y)<- ap$APICAO
  w <- data.frame(y) %>%
    gather("AP","dist_m") %>%
    rename(from = .data$AP) %>% #we use the AP ICAO code as the node ID
    group_by(.data$from) %>%
    mutate(to = as.character(route_grid@points$id),
           dist_m = units::drop_units(.data$dist_m),
           near_m = abs(.data$dist_m - ad_dist_m)) %>% #compare to target distance
    arrange(.data$near_m) %>%
    select(-.data$near_m) %>%
    #take the top n, with 8 should be near the points of the compass
    slice(1:ad_nearest) %>%
    #add time (cost) for each aircraft
    ungroup() %>%
    crossing(ac %>% select(.data$id, .data$arrdep_kph)) %>%
    mutate(cost = (.data$dist_m/1000)/.data$arrdep_kph) %>%
    #add the long lats
    left_join(ap %>%
                data.frame() %>%
                select(.data$APICAO, .data$lat, .data$long),
              by=c("from"="APICAO")) %>%
    data.table::as.data.table() %>%
    left_join(route_grid@points %>%
                select(.data$id, .data$long, .data$lat, .data$land) %>%
                mutate(id = as.character(.data$id)),
              by = c("to"="id"), suffix=c("_ap", "_grid")) %>%
    #if no transition leg, then take the acceleration subsonic cruise-supersonic penalty here
    mutate(cost = .data$cost + ifelse(.data$land, 0, ac$trans_h)) %>%
    select(-.data$land) %>%
    as.data.frame()

}


#convert the returned path to a series of great-circle arcs, by phase
pathToGC <- function(path, route_grid,
                     fat_map, avoid,
                     arrDep,
                     ac,
                     byTime,
                     max_gcsteps=40, gckm_perstep=30,
                     shortcuts=TRUE, ...){
  #accepts either a list of one, or an unlisted path
  stopifnot(class(route_grid)=="GridLat")
  stopifnot(length(path)==1)

  if(class(path)=="list") path=unlist(path)
  #need to strip off the airports
  n <- length(path)
  sid <- path[1:2]
  star <- path[(n-1):n]

  #do sid and star first
  #create line for departure, using 0 as id for airport
  dep <- data.frame(phase = factor("arr/dep", levels=levels(route_grid@lattice$phase)),
                    phaseID = "0.",
                    w_id = 0, from=sid[1], to = sid[2],
                    steps = 1, stringsAsFactors = FALSE) %>%
    #add distance and time for the departure
    left_join(arrDep %>% select(-id, -.data$arrdep_kph),
              by=c("from","to") ) %>%
    #flip the types back to match gcid
    mutate(gcdist_km = .data$dist_m/1000,
           from = 0, to = as.numeric(.data$to)) %>%
    rename(from_long = .data$long_ap, from_lat = .data$lat_ap,
           to_long = .data$long_grid, to_lat = .data$lat_grid,
           time_h = .data$cost) %>%
    select(-.data$dist_m)

  #create line for arrival, using 0 as id for airport
  #the phaseID here will need updating if n>3
  arr <- data.frame(phase = factor("arr/dep", levels=levels(route_grid@lattice$phase)),
                    phaseID = "1.",
                    w_id = as.numeric(star[1]), from=star[1], to = star[2],
                    steps = 1, stringsAsFactors = FALSE) %>%
    #add distance and time for the departure
    left_join(arrDep %>% select(-id, -.data$arrdep_kph),
              by=c("from"="to","to"="from")) %>%
    #flip the types back to match gcid
    mutate(gcdist_km = .data$dist_m/1000,
           from = as.numeric(.data$from), to = 0) %>%
    rename(from_long = .data$long_ap, from_lat = .data$lat_ap,
           to_long = .data$long_grid, to_lat = .data$lat_grid,
           time_h = .data$cost) %>%
    select(-.data$dist_m)

  if (n==3) {
    #unusual sid-star case 3 is the minimum possible
    gcid <- dep %>%
      bind_rows(arr)
  } else {
    #the normal case
    path <- path[2:(n-1)]
    n <- n - 2
    #phase is a property of pairs, so need to reconstruct the phase
    #the lattice might be stored in the opposite sense, so check both
    p <- data.frame(from = as.numeric(path[1:n-1]),
                    to = as.numeric(path[2:n])) %>%
      left_join(route_grid@lattice[ , c("from", "to", "phase")] ,
                by=c("from","to")) %>%
      left_join(route_grid@lattice[ , c("from", "to", "phase")],
                by=c("from"="to","to"="from")) %>%
      as.data.frame() %>%
      mutate(phase=coalesce(.data$phase.x, .data$phase.y)) %>%
      select(-.data$phase.x, -.data$phase.y)

    if (ac$over_sea_M == ac$over_land_M | !byTime) {
      #if this is a subsonic aircraft - or distance only, then land/sea/transition phases are the same - land
      #and at this point the path only contains these 3
      p <- p %>%
        mutate(phase = factor("land", levels=levels(route_grid@lattice$phase)),
               phaseID = "1.")
    }
    else {
      #now calculate phase change
      p <- p %>%
        mutate(phaseChange = case_when(
          row_number()==1 ~ 1L,
          phase != lag(phase) ~ 1L,
          TRUE ~ 0L),
          phaseID = paste0(cumsum(.data$phaseChange),".")) %>%
        select(-.data$phaseChange)
    }
    # debug find NA phase
    if (any(is.na(p$phase))){
      message("missing phase")
    }

    if(getOption("quiet", default=0)>1) message(" Calculated phase changes")
    fat_map_s2 <- st_as_s2(fat_map) # do this conversion only once
    if (is.list(avoid)) avoid_s2 <- st_as_s2(avoid)
    else
      avoid_s2 <- avoid
    p <- p %>%
      #now duplicate each row and simplify to a single id instead of from-to
      #deliberately create doubles then drop them
      #tidyr::gather ought to do this, but changes the order in a way I don't want
      rowwise() %>%
      uncount(2) %>%
      group_by(.data$phaseID, .data$from, .data$to) %>%
      mutate(id = c(first(.data$from), first(.data$to))) %>%
      group_by(.data$phaseID) %>%
      select(-.data$from, -.data$to) %>%
      distinct() %>%
      #need the long lat too
      left_join(route_grid@points[ , c("id", "long", "lat")],
                by = c("id")) %>%
      as.data.frame() %>%
      #add in the distances from land - by phase since if <>"sea" then 0
      group_by(.data$phase) %>%
      mutate(fromLand_km = if_else(.data$phase == "sea",
                                   distFromLand(.data$long, .data$lat, fat_map),
                                   distFromLand(.data$long, .data$lat, avoid))) %>%
      ungroup()

    #loop for each phase
    #trim path to list of id pairs, each of which can be joined by a GC
    #reduce collapes a list of data.frames to a data.frame
    if (getOption("quiet", default=0)>2) message("  Ready to recurse")

    gcid <- purrr::reduce(lapply(unique(p$phaseID), function(i, m) findGC(p[p$phaseID==i,],
                                                                          fat_map_s2, avoid_s2)),
                          bind_rows) %>%
      #after a single hop line you can start too late, so correct any gaps
      mutate(from = .data$id,
             to = coalesce(lead(.data$id), last(p$id))) %>%
      rename(w_id = .data$id) %>%
      filter(! .data$from == .data$to) # not sure why it occasionally churns these out


    if (getOption("quiet", default=0)>1) message(" Done recursion")

    #code is split just because there's less to highlight in the debugger
    gcid <- gcid %>%
      data.table::as.data.table() %>%
      #add back the geo data
      left_join(route_grid@points[ , c("id", "long", "lat")],
                by = c("from"="id")) %>%
      rename(from_long = .data$long, from_lat = .data$lat) %>%
      left_join(route_grid@points[ , c("id", "long", "lat")],
                by = c("to"="id")) %>%
      rename(to_long = .data$long, to_lat = .data$lat) %>%
      as.data.frame()

    #re-assert order
    gcid <- p %>%
      select(id) %>% distinct() %>%
      inner_join(gcid, by = c("id"="w_id")) %>%
      rename("w_id" = "id")

    # loop to look for great-circle shortcuts
    if (shortcuts) {
      if (getOption("quiet", default=0)>1) message(" Checking Shortcuts")
      baseID <- 1
      while (baseID < nrow(gcid)-1) {
        #skip this baseID if not 'sea' or 'land'
        if (gcid[baseID, ]$phase %in% c("sea", "land")){
          #check the furthest first
          farID <- nrow(gcid)
          while (farID > baseID + 1) {
            #only check the geometry if all the same phase (all sea, all land)
            phases <- unique(gcid[baseID:farID,"phase"])
            if (nrow(phases) == 1) {
              test_line <- s2::s2_make_line(c(gcid[baseID,]$from_long, gcid[farID,]$to_long),
                                            c(gcid[baseID,]$from_lat, gcid[farID,]$to_lat))
              #just extract the single binary result: All sea?
              if (gcid[baseID, ]$phase == "sea") {
                use_map <- fat_map_s2
              } else {
                # if land, then only worried if we hit the avoid area
                use_map <- avoid_s2
              }
              all_sea <- ! s2::s2_intersects(test_line, use_map)
              if (all_sea) {
                if (getOption("quiet", default=0)>2) message("  Shortcut from ", baseID, " to ", farID)
                #shift the intermediate points onto the new shortcut line
                interm <- (baseID+1):farID
                old_pts <- s2::s2_lnglat(gcid[interm, ]$from_long, gcid[interm, ]$from_lat)
                new_pts <- s2::s2_closest_point(test_line, old_pts)
                gcid[interm, "from_long"] <- s2::s2_x(new_pts)
                gcid[interm, "from_lat"] <- s2::s2_y(new_pts)
                # update the next step info
                gcid[baseID:(farID-1), c("to", "to_long","to_lat")] <- gcid[interm,
                                                                               c("from", "from_long","from_lat")]
                # if you've a shortcut direct to the end then stop, otherwise look for an
                #  efficient place to restart the further-first search
                if (farID == nrow(gcid) | gcid[baseID,"phase"] != gcid[farID + 1,"phase"]) {
                  new_baseID <- farID
                } else {
                  # fast-forward through the intermediate points
                  # with _forward_ search to the _next_ point
                  # rather than furthest first, because the _next_ point must be occluded
                  new_baseID <- baseID
                  hits_land <- TRUE # since furthest first search, this is true for baseID>farID+1
                  while (hits_land) {
                    new_baseID <- new_baseID + 1
                    test_line <- s2::s2_make_line(c(gcid[new_baseID,]$from_long, gcid[farID + 1,]$to_long),
                                                  c(gcid[new_baseID,]$from_lat, gcid[farID + 1,]$to_lat))
                    hits_land <- s2::s2_intersects(test_line, use_map)
                  }
                }
                # drop the intermediate values
                if (new_baseID - baseID > 1) {
                  gcid <- gcid %>% slice(-((baseID + 1):(new_baseID - 1)))
                  new_baseID <- baseID + 1
                  # update to-end of vector
                  gcid[baseID, c("to", "to_long", "to_lat")] <- gcid[new_baseID,
                                                                        c("from", "from_long","from_lat")]
                }
                #on the fly switch to a new search
                baseID <- new_baseID
                farID <- nrow(gcid) + 1
              }
            }
            farID <- farID - 1
          }
        }
        baseID <- baseID + 1
      }
    }

    gcid <- gcid %>%
      mutate(gcdist_km = geosphere::distGeo(matrix(c(.data$from_long, .data$from_lat), ncol=2),
                                 matrix(c(.data$to_long, .data$to_lat), ncol=2))/1000,
             steps = floor(pmin(.data$gcdist_km/gckm_perstep, max_gcsteps))) %>%
      group_by(.data$phase) %>%
      mutate(time_h = time_h(.data$phase, .data$gcdist_km, ac=ac))

    #update the phaseID for the arr phase
    arr <- arr %>%
      mutate(phaseID = paste0(max(as.integer(gcid$phaseID))+1,"."))

    gcid <- dep %>%
      bind_rows(gcid) %>%
      bind_rows(arr)
  }

  gcid <- gcid %>%
    #and finally add the arcs
    #my st_gcIntermediate is not vector-clever so need to go row-wise
    rowwise() %>%
    mutate(gc = st_gcIntermediate(p1=c(.data$from_long, .data$from_lat),
                                  p2=c(.data$to_long, .data$to_lat),
                                  n=.data$steps-1, addStartEnd=TRUE,
                                  crs = crs_longlat)) %>%
    select(-.data$steps) %>%
    ungroup()
}


#' Find best non-stop route between 2 airports
#'
#' \code{find_leg} finds the quickest non-stop route for \code{ac} between
#' two airports \code{ap2}.
#'
#' This function finds the quickest non-stop route between two airports. A
#' 'route' is made up of one or two 'legs' (airport to airport without
#' intermediate stop). \code{\link{find_route}} makes one or more calls to
#' \code{find_leg} as required.
#'
#' It assumes that the routing grid, \code{route_grid}, has already been classified as
#' land or sea using the map \code{fat_map}. The map is further used when
#' converting the grid-based route to one of great-circle segments.
#'
#' In fact \code{find_leg} finds up to 4 versions of the path:
#' \enumerate{
#'     \item A great circle, direct between the airports
#'     \item A grid path, consisting of segments of the routing grid, plus departure
#'     and arrival routes from the airports
#'     \item A simplification of the grid path to great circle segments
#'     \item \code{shortcuts} defaults to TRUE. Without this, you see near-raw
#'     Dijkstra results, which are _not_ shortest great circle.
#' }
#'
#' Legs are automatically saved in \code{route_cache} and retrieved from here if
#' available rather than re-calculated. See
#' \href{../doc/Supersonic_routes_in_depth.html#cache}{vignette on caching} for cache
#' management.
#
#'
#' @param ac,ap2,route_grid,fat_map,ap_loc,avoid See \code{\link{find_route}}
#' @param enforce_range If TRUE (default) then leg is constrained to aircraft range,
#'     otherwise routes of excess range can be found.
#' @param best_by_time If TRUE (default) then the quickest route is found,
#'     else the shortest distance.
#' @param grace_km Default NA. Otherwise, if great circle distance is within
#'   3pct of aircraft range, then add \code{grace_km}km to the range.
#' @param shortcuts If TRUE (default) then path will be checked for great circle shortcuts.
#' @param ad_dist_m The length of arrival/departure links, in m. (Default 100,000=100km)
#' @param ad_nearest The number of arrival/departure links to create (Default 12)
#' @param max_leg_circuity The maximum detour over great circle distance that
#'   can be flown to find a quick over-sea route. Default 1.4.
#' @param ... Other parameters, passed to \code{\link{make_route_envelope}}
#'
#'
#' @return Dataframe with details of the leg
#'
#' @import cppRouting
#' @import sf
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' # need to load some of the built-in data
#' aircraft <- make_aircraft(warn = FALSE)
#' airports <- make_airports(crs = crs_Pacific)
#' # get test datasets
#' NZ_buffer30 <- hm_get_test("buffer")
#' NZ_grid <- hm_get_test("grid")
#'
#' options("quiet" = 4) #for heavy reporting
#' # from Auckland to Christchurch
#' ap2 <- make_AP2("NZAA","NZCH",airports)
#' routes <- find_leg(aircraft[4,],
#'                     ap2,
#'                     fat_map = NZ_buffer30,
#'                     route_grid = NZ_grid,
#'                     ap_loc = airports)
#'
#' @export
find_leg <- function(ac, ap2, route_grid, fat_map, ap_loc,
                    avoid=NA, enforce_range=TRUE,
                    best_by_time=TRUE, grace_km=NA,
                    shortcuts=TRUE,
                    ad_dist_m = 100 * 1000,
                    ad_nearest = 12, max_leg_circuity = 1.4,
                    ...){
  stopifnot(is.data.frame(ac)) #should be a single-row dataframe
  if (any(is.na(ac))) stop("Aircraft invalid (check selected row).") #should without NAs

  unidirectional <- FALSE #not an option in this version

  #can save and load the cache, with loadRDS readRDS
  #if cache doesn't match create it as a child of Global (so persists outside this function!)
  if ((attr(.hm_cache$route_cache,"map") != route_grid@name)) {
    if (getOption("quiet", default=0)>0) message("Map used by grid has changed, so clearing route cache.")
    hm_clean_cache("route") # empty cache
    attr(.hm_cache$route_cache,"map") <- route_grid@name}

  if (unidirectional) {
    #note we use a different separator here, but not < > which file systems might reject
    cache_as <- paste(ac$id, ap2$ADEP, ap2$ADES, attr(avoid,"avoid"),
                      enforce_range, best_by_time, grace_km, shortcuts,
                      ad_dist_m/1000, ad_nearest, sep="_")
  }
  else {
    #cache the route with data name which is the ACID and AP2
    cache_as <- paste(ac$id, min(ap2$ADEP, ap2$ADES), max(ap2$ADEP, ap2$ADES),
                      attr(avoid,"avoid"), enforce_range,
                      best_by_time, grace_km, shortcuts,
                      ad_dist_m/1000, ad_nearest, sep="-")
  }

  #if this query has not already been cached, calculate its value
  if (!exists(cache_as, envir=.hm_cache$route_cache, inherits=F)) {
    if (getOption("quiet", default=0)>2) message("  Not cached: calculating...")
    r <- find_leg_really(ac, ap2, route_grid, fat_map, ap_loc, avoid,
                                enforce_range, best_by_time, grace_km,
                                shortcuts, ad_dist_m, ad_nearest, max_leg_circuity, ...)
    if (is.na(r[1,1]))(return(r)) #quick end without caching if it's an empty route.
    assign(cache_as, r, .hm_cache$route_cache)
  }

  #return value
  get(cache_as, .hm_cache$route_cache)
}


#find a single route
#' @import sf
#' @import dplyr
#'
find_leg_really <- function(ac, ap2, route_grid, fat_map,
                            ap_loc,
                            avoid=NA,
                            enforce_range=TRUE,
                            best_by_time=TRUE,
                            grace_km=NA,
                            shortcuts=TRUE,
                            ad_dist_m, ad_nearest, max_leg_circuity,
                            ...){
  #start with a grid, find the routes for this aircraft
  #ap2 is a row of a dataframe with at least AP2 from_long, from_lat, to_long, to_lat
  # dots are to allow passing the number of points to generate in the envelope
  # avoid is either NA, or a map in fat_map crs of regions to avoid
  #best_by_time means use time as cost function, if FALSE then pure distance
  #arrDep is the airport locations for getting the TOC TOD
  #default algorithm is bidirectional Dijkstra

  tstart <- Sys.time()
  stopifnot(class(route_grid)=="GridLat")

  if (getOption("quiet", default=0)>0) message("Leg: ", ap2$AP2, " Aircraft: ", ac$type)

  #get crow-flies
  crow <- st_gcIntermediate(p1=c(ap2$from_long, ap2$from_lat),
                            p2=c(ap2$to_long, ap2$to_lat),
                            n = 30, addStartEnd=TRUE,
                            crs=crs_longlat)

  #check can actually make it!
  gcdist <- geosphere::distGeo(c(ap2$from_long, ap2$from_lat),
                    c(ap2$to_long, ap2$to_lat))/1000

  if ((gcdist > ac$range_km) & enforce_range) {
    if (getOption("quiet", default=0)>0) message("Distance ", round(gcdist,1), "km exceeds range ",
                                                round(ac$range_km,1), "km.")
    #return something mostly empty
    return(emptyRoute(ac, ap2, fat_map))
  }

  # these should match in the inputs
  use_crs <- st_crs(route_grid@lattice$geometry)
  stopifnot(st_crs(route_grid@points$xy) == use_crs)
  # these are fast, so transform them rather than assume
  if (st_crs(fat_map) != use_crs) st_transform(fat_map, use_crs)
  ap_loc$ap_locs <- st_transform(ap_loc$ap_locs, crs=use_crs, quiet=FALSE)
  if (is.list(avoid)) {
    avoid <- st_transform(avoid, use_crs)
    # merge the list into one, eg if avoid is currently separate countries
    avoid <- st_union(avoid)
  }

  if (getOption("quiet", default=0)>2) message("  Starting envelope: ",round(Sys.time() - tstart,1))
  #make the envelope - so can plot even if don't enforce it
  #we work with the envelope in map CRS, then save at last stage in crs_longlat
  envelope <-  st_sfc(st_polygon(), crs=use_crs) #null by default
  if (gcdist <= ac$range_km) {

    #if necessary add a grace distance, to allow routing to be found if within 2%.
    if (!is.na(grace_km) & (gcdist/ac$range_km > 0.97)) ac$range_km <- ac$range_km + grace_km

    # v.v. if gcdist is small, reduce the range using max_leg_circuity
    ac$range_km <- min(ac$range_km, gcdist * max_leg_circuity)
    if (enforce_range) {
      #find route Envelope
      envelope <-  make_route_envelope(ac, ap2, ...) %>%
        st_transform(use_crs)
      # crop first points then lattice to envelope
      # crop first by lat long then by intersection
      env_rec <- s2::s2_bounds_rect(envelope)
      if (env_rec$lng_lo < env_rec$lng_hi) {
        rgp <- route_grid@points %>%
          filter(.data$long >= env_rec$lng_lo & .data$long <= env_rec$lng_hi)}
      else {
        rgp <- route_grid@points %>%
          filter(.data$long >= env_rec$lng_lo | .data$long <= env_rec$lng_hi)}
      route_grid@points <- rgp %>%
        filter(.data$lat >= env_rec$lat_lo & .data$lat <= env_rec$lat_hi) %>%
        filter(sf::st_intersects(.data$xy, envelope, sparse = FALSE) %>% as.vector()) %>%
        data.table::as.data.table()
      # 'crop' using ids - much faster than geo-intersection
      route_grid@lattice <- route_grid@lattice %>%
        inner_join(route_grid@points %>% select(.data$id), by=c("from"="id")) %>%
        inner_join(route_grid@points %>% select(.data$id), by=c("to"="id")) %>%
        data.table::as.data.table()
      # crop map to envelope, too
      fat_map <- st_intersection(fat_map, envelope)
      if (getOption("quiet", default=0)>1) message(" Cut envelope from lattice: ",round(Sys.time() - tstart,1))
    }
  }

  #get the arrDep routes
  #note here only 1-directional - the makegraph adds the other direction
  adep <- ap_loc[ap_loc$APICAO == ap2$ADEP, ]
  ades <- ap_loc[ap_loc$APICAO == ap2$ADES, ]
  arrDep <-
    bind_rows(findToCToD(adep, route_grid, fat_map,
                         ac, ad_dist_m, ad_nearest),
              findToCToD(ades, route_grid, fat_map,
                         ac, ad_dist_m, ad_nearest))

  #check if need to avoid areas
  if (is.list(avoid)){
    # remove the avoid area from the search grid
    z <- sf::st_intersects(avoid, route_grid@lattice$geometry, sparse=FALSE) %>%
      as.vector()
    if (all(!z)) {
      avoid <- NA # no intersection so treat as no avoid
    } else {
      #remove this from lattice
      route_grid@lattice <- route_grid@lattice %>%
        filter(!z) %>%
        data.table::as.data.table()
      #need an extra map
      #for over-sea flight ensure the avoid areas are included, so they are not allowed
      fat_map <- st_union(fat_map, avoid)
      # check for validity of arr-departure routes now.
      # check in two parts for better error reporting
      ap1_toc <- arrDep %>% filter(.data$from == ap2$ADEP) %>%
        mutate(pt = s2::s2_lnglat(.data$long_grid, .data$lat_grid))
      if (all(s2::s2_contains(avoid, ap1_toc$pt))) {
        warning("Avoid airspace prevents reaching ", ap2$ADEP, call. = FALSE)
        #return something mostly empty
        return(emptyRoute(ac, ap2, fat_map))
      }
      ap2_toc <- arrDep %>% filter(.data$from == ap2$ADES) %>%
        mutate(pt = s2::s2_lnglat(.data$long_grid, .data$lat_grid))
      if (all(s2::s2_contains(avoid, ap2_toc$pt))) {
        warning("Avoid airspace prevents reaching ", ap2$ADES, call. = FALSE)
        #return something mostly empty
        return(emptyRoute(ac, ap2, fat_map))
      }
    }

    if (getOption("quiet", default=0)>2) message("  Adjusted for avoid areas: ", round(Sys.time() - tstart,1))
    #and for non-overseas flight, also need to avoid 'avoid'
  }

  #in debug, quick plot of sea points in grid
  # ggplot(route_grid@points %>% filter(!land), aes(long, lat)) + geom_point(size=0.1)

  #add aircraft specific costs
  costed_lattice <- costLattice(route_grid, ac = ac) %>%
    select(.data$from, .data$to, .data$cost, .data$dist_km) %>%
    #add on the arrival dep - indices are now strings
    mutate_at(vars(.data$from, .data$to), ~as.character(.)) %>%
    data.table::as.data.table() %>% # seem to be limits to complexity of pipe for dtplyr
    bind_rows(arrDep %>%
                mutate(dist_km = .data$dist_m/1000) %>%
                select(.data$from, .data$to, .data$cost, .data$dist_km)) %>%
    data.table::as.data.table()

  if (!best_by_time) {costed_lattice <- costed_lattice %>%
    mutate(cost = .data$dist_km)  %>%
    data.table::as.data.table()#if !bytime, just use distance..
  }
  if (getOption("quiet", default=0)>2) message("  Got costed lattice: ",round(Sys.time() - tstart,1))


  gr <- makegraph(costed_lattice %>% select(.data$from, .data$to, .data$cost),
                  directed = FALSE)
  #with update to cppRouting v2.0 this stopped working
  # gr <- cpp_simplify(gr, iterate=TRUE)$graph #simplify the data

  #then get the grid route
  # path <- get_path_pair(gr, nearest_id(route_grid, c(ap2$from_long, ap2$from_lat)),
  #                       nearest_id(route_grid, c(ap2$to_long, ap2$to_lat)))
  if (getOption("quiet", default = 0) < 2) {
    suppressMessages(path <- get_path_pair(gr, ap2$ADEP, ap2$ADES))
  } else path <- get_path_pair(gr, ap2$ADEP, ap2$ADES)

  if (length(path[[1]])>1) {
    if(getOption("quiet", default=0)>2) message("  Got path: ",round(Sys.time() - tstart,1))
  }
  else if (length(path[[1]])<2 ) {
    if (getOption("quiet", default=0)>0) {message("Failed to find path for ", ap2$AP2,
                                                 " (too far, or higher envelope_points)")}
    #return something mostly empty
    return(emptyRoute(ac, ap2, fat_map))
  }


  gcArcs <- pathToGC(path, route_grid, fat_map, avoid, arrDep, ac, best_by_time,
                     shortcuts=shortcuts) %>%
    #add identifying features
    #add grid, crow and envelope in the first one
    mutate(timestamp = rep(format(Sys.time(), "%Y%m%d_%H%M%OS2"),n()), #unique identifier
           routeID = ap2$AP2,
           acID = ac$id,
           acType = ac$type,
           #this is inefficient, saving multiple copies
           grid=c(path, rep(list(NA),n()-1)), #a list with just the grid in the first one
           crow = c(crow, rep(st_sfc(st_linestring(), crs=crs_longlat),n()-1)),
           envelope = c(prj(envelope, crs=crs_longlat),
                        rep(st_sfc(st_polygon(), crs=crs_longlat),
                            n()-1)),
           fullRouteID = ap2$AP2,
           legs = 1, leg_id = 1,
           refuel_ap = NA)

  #only if there is a sea phase can we smooth the accelerations out.
  if ("sea" %in% gcArcs$phase) smoothSpeed(gcArcs, ac)
  else gcArcs <- gcArcs %>% mutate(speed_kph = .data$gcdist_km/time_h)
}


#' Make range-constrained envelope between 2 airports
#'
#' \code{make_route_envelope} finds the range envelope for a given route
#'
#' The 'route envelope' is the region within which a route from A to B must
#' remain. This is an ellipse.
#'
#' It differs from the pure 'range envelope' which is the points which an
#' aircraft can reach from a given airport.
#'
#'
#' @param ac,ap2 See \code{\link{find_route}}
#' @param envelope_points How many points are used to define the ellipse? Default
#'   200.
#' @param fuzz Add a little margin to the range, to allow the longest range to
#'   be flown, rather than be cut off at the boundary. (Default 0.005)
#'
#' @return \code{sf POLYGON} with ad hoc coordinate reference system.
#'
#' @import sf
#'
#' @examples
#' # Need aircraft and airport datasets
#' ac <- make_aircraft(warn = FALSE)
#' ap <- make_airports()
#' z <- make_route_envelope(ac[1,], make_AP2("EGLL","KJFK",ap))
#'
#' @export
make_route_envelope <- function(ac, ap2,
                          envelope_points=200,
                          fuzz=0.005){
  #add a 0.5% fuzz to allow longest range to be flown
  geo_m <- geosphere::distGeo(c(ap2$from_long, ap2$from_lat),
                              c(ap2$to_long, ap2$to_lat))
  geo_c <- geosphere::midPoint(c(ap2$from_long, ap2$from_lat),
                               c(ap2$to_long, ap2$to_lat))

  r <- (ac[1,]$range_km * 1000) * (1 + fuzz)
  a <- r/2
  b <- sqrt((r/2)^2-(geo_m/2)^2)
  psi <- geosphere::bearing(geo_c, c(ap2$to_long, ap2$to_lat))

  # reverse order for s2 left-hand rule
  theta <- seq(359, 0, length.out = envelope_points)
  tp_rad <- theta*pi/180
  #polar form for radius of an ellipse from the centre with semi-major axis length a
  #and starting with the longest

  dist <- a * b / sqrt(a^2*sin(tp_rad)^2 + b^2*cos(tp_rad)^2)
  geod <- geosphere::geodesic(geo_c, theta + psi, dist)

  pg <- s2::s2_make_polygon(geod[,1], geod[,2]) %>%
    st_as_sfc()

}


#helper for smoothSpeed
smooth1 <- function(sign=1, r, ac){
  #do the smooth loop in 1 direction - at a time
  #a sub-function for smoothSpeed
  stopifnot(sign %in% c(-1, 1))

  #check if need to include arr/dep or not
  #first & last give the arr/dep - if these speeds are low, then accel penalty has been applied
  r_start <- if_else(first(r$speed_kph) < ac$arrdep_kph,
                     1, 2)
  r_end <- if_else(last(r$speed_kph) < ac$arrdep_kph,
                   nrow(r), nrow(r) - 1L)

  if (sign==1) seq <- r_start:(nrow(r) - 1L)
  else seq <- seq.int(r_end,2, by=-1)

  for (i in seq){
    r[i,]$penalty <- ifelse((i-sign)>0 & (i-sign)<=nrow(r),
                            r[i-sign,]$penalty,0) #carry it forward
    if (r[i,]$phase %in% c("arr/dep", "transition") & r[i+sign,]$phase == "sea" ) {
      #first, take the penalty for transition off the time and add it to the running penalty
      r[i,]$time_h <- r[i,]$time_h - ac$trans_h
      r[i,]$penalty <- r[i,]$penalty + ac$trans_h
      #now it's like a sea phase
      #take as much of this penalty as we can, but not slower than over-land
      #typically it'll end up hitting the slowest: over-land
      min_kph <- if_else(r[i,]$phase=="arr/dep",
                         ac$arrdep_kph, ac$over_land_kph)
      max_h <- r[i,]$gcdist_km/min_kph #no slower than over land
      p <- min(r[i,]$penalty, max_h - r[i,]$time_h)
      r[i,]$time_h <- r[i,]$time_h + p
      r[i,]$penalty <- r[i,]$penalty - p
      r[i,]$speed_kph <- r[i,]$gcdist_km/r[i,]$time_h
    }
    if (r[i,]$phase=="sea" & r[i,]$penalty>0) {
      max_h <- r[i,]$gcdist_km/r[i-sign,]$speed_kph #no slower than last step
      p <- min(r[i,]$penalty, max_h - r[i,]$time_h)
      r[i,]$time_h <- r[i,]$time_h + p
      r[i,]$penalty <- r[i,]$penalty - p
      r[i,]$speed_kph <- r[i,]$gcdist_km/r[i,]$time_h
    }
  }

  return(r)
}

#when it's all done, we need to smooth the speed profile
smoothSpeed <- function(r, ac){
  #r is one route, assume first and last row is arr/dep
  #ac is the aircraft dataset
  #hard to see how do this other than row by row

  #logically, if the average end-to-end speed were less than the over-land cruise, then
  #Dijkstra would have taken it overland, so the minimum speed is the overland speed
  #originally ignoring arr/dep - now included since arr/dep-sea links puts accel penalty on arr/dep

  ac <- ac %>% filter(id == first(r$acID))
  r <- r %>%
    mutate(penalty = 0,
           speed_kph= .data$gcdist_km/.data$time_h) #useful initial value
  #spread the acceleration cost forwards
  r <- smooth1(1, r, ac)
  #spread the deceleration cost backwards
  r <- smooth1(-1, r, ac)


  return(r %>% select(-.data$penalty))
}


#' Summarise a set of routes
#'
#' Reduce a set of routes to a one-line per route summary
#'
#' This function takes the output of \code{\link{find_route}} and summarises to
#' one line per (full) route.
#'
#' With refuelling, there can be multiple 'full routes' for each 'route'. The
#' \code{best} column indicates the best route for each \code{routeID}.
#'
#' The results are rounded to a reasonable number of significant figures. After
#' all this is just an approximate model. The \code{arrdep_h} has been checked
#' against actual and is reasonable (observed range roughly 0.3-0.5).
#'
#' @param routes Each segment in each route, as produced by
#'   \code{\link{find_route}} or \code{\link{find_leg}}
#' @param ap_loc List of airport locations, output of
#'   \code{\link{make_airports}}
#' @param arrdep_h Total time for the M084 comparator aircraft to arrive &
#'   depart in hours. Default 0.5.
#'
#' @return Dataframe with summary of the route, sorted in ascending order of \code{advantage_h}
#' so that the best route are plotted on top. The fields are:
#' \itemize{
#'   \item \code{timestamp}: when the leg was originally generated (it may have been cached)
#'   \item \code{fullRouteID}: including the refuel stop if any
#'   \item \code{routeID}: origin and destination airport, in \code{\link{make_AP2}} order
#'   \item \code{refuel_ap}: code for the refuelling airport, or NA
#'   \item \code{acID, acType}: aircraft identifiers taken from the aircraft set
#'   \item \code{M084_h}: flight time for a Mach 0.84 comparator aircraft (including \code{2*arrdep_h})
#'   \item \code{gcdist_km}: great circle distance between the origin and destination airports
#'   \item \code{sea_time_frac}: Fraction of \code{time_h} time spent over sea, hence at supersonic speed,
#'     or accelerating to, or decelerating from supersonic speed
#'   \item \code{sea_dist_frac}: as sea_time_frac, but fraction of \code{dist_km}
#'   \item \code{dist_km}: total length of the route, in km
#'   \item \code{time_h}: total time, in hours
#'   \item \code{n_phases}: number of distinct phases: arr/dep, transition, land, sea, refuel.
#'   \item \code{advantage_h}: \code{M084_h - time_h}
#'
#'   \item \code{circuity}: the route distance extension (1 = perfect) \code{dist_km / gcdist_km}
#'   \item \code{best}: for each \code{routeID}, the \code{fullrouteID} with maximum \code{advantage_h}
#' }
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' # here we use a built-in set of routes
#' # see vignette for more details of how to obtain it
#' airports <- make_airports(crs = crs_Pacific)
#' NZ_routes <- hm_get_test("route")
#' sumy <- summarise_routes(NZ_routes, airports)
#'
#' @export
summarise_routes <- function(routes,
                            ap_loc,
                            arrdep_h = 0.5){
  #include 30 mins for arr/dep - based on 777ER examples
  route_summary <- routes %>%
    group_by(.data$routeID) %>%
    rename(segdist_km = .data$gcdist_km) %>%
    mutate(gcdist_km = make_AP2(substr(first(.data$routeID),1,4),
                                    substr(first(.data$routeID),7,10),
                                    ap_loc)$gcdist_km,
           M084_h = round(.data$gcdist_km/(0.84 * himach::mach_kph),2) + arrdep_h,
           gcdist_km = round(.data$gcdist_km,1)) %>%
    group_by(.data$timestamp, .data$fullRouteID, .data$routeID, .data$refuel_ap,
             .data$acID, .data$acType, .data$M084_h, .data$gcdist_km) %>%
    summarise(sea_time_frac =
                round(sum(if_else(.data$phase=="sea",.data$time_h,0))/sum(.data$time_h), 3),
              sea_dist_frac =
                round(sum(if_else(.data$phase=="sea", .data$segdist_km, 0))/sum(.data$segdist_km), 3),
              dist_km = round(sum(.data$segdist_km), 1),
              fly_time_h = round(sum(if_else(.data$phase=="refuel", 0, .data$time_h)), 2),
              time_h = round(sum(.data$time_h), 2),
              n_phases =
                as.integer(last(.data$phaseID)) - as.integer(first(.data$phaseID)) + 1,
              n_accel = sum(if_else(.data$phase=="sea" & lag(.data$phase) != "sea", 1,0),
                            na.rm = TRUE)) %>%
    mutate(advantage_h = .data$M084_h - .data$time_h,
           advantage_pct = round(.data$advantage_h / .data$M084_h, 3),
           circuity = round(.data$dist_km/.data$gcdist_km, 2) - 1,
           ave_fly_speed_M = round(.data$dist_km / (.data$fly_time_h * himach::mach_kph), 2),) %>%
    # for non-routes, set more values to NA
    ungroup() %>%
    mutate_at(c("dist_km", "n_phases", "n_accel", "circuity"),
              ~ifelse(is.na(.data$time_h), NA, .)) %>%
    arrange(.data$routeID, .data$acType, .data$time_h) %>%
    #add best for routeID, refuelling or not
    group_by(.data$routeID, .data$acID) %>%
    mutate(best = ifelse(
      all(is.na(.data$advantage_h)),
      NA,
      (.data$advantage_h == max(.data$advantage_h, na.rm = TRUE))),
      #fix circuity rounding error
      circuity = pmax(0, .data$circuity)) %>%
    ungroup() %>%
    arrange(.data$advantage_h)

}
