# routes
#
# functions for finding routes on a grid


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
costLattice <- function(pg,ac){
  #given a GridLat and an Aircraft return a dataframe lattice with costs
  stopifnot(class(pg)=="GridLat")

  cl <- pg@lattice %>%
    mutate(cost = case_when(
      phase == "land" ~ dist_km/ac$over_land_kph,
      phase == "sea" ~ dist_km/ac$over_sea_kph,
      TRUE ~ dist_km/ac$trans_kph + ac$trans_h))
}


distFromLand <- function(long, lat, land){
  #return a distance for each point from land, in km
  #long and lat are vectors, land is a map (sfc_MULTIPOLYGON)
  if (is.na(land)) return(0)
  mp <- st_transform(st_cast(st_sfc(
    st_multipoint(matrix(c(long, lat),ncol=2)),crs=4326),'POINT'),
    crs=st_crs(land))
  as.vector(st_distance(mp, land))/1000
}



#this will be called recursively
findGC <- function(subp, withMap, avoidMap){

  if(getOption("quiet",default=0)>2) print(paste(first(subp$phase), first(subp$phaseID), nrow(subp))) #debug

  #if is.na(avoidMap) then a simplified approach for the non-sea phases
  #for sea phases use withMap (avoiding land & avoid areas, already union)
  #for other phases use avoidMap
  if (first(subp$phase) != "sea") useMap <- avoidMap
  else useMap <- withMap

  #land or transition, we assume a single GC
  #sea, but single step, then the same
  if ((first(subp$phase) != "sea" & is.na(useMap)) | length(subp) == 1){
    if (nrow(subp)==1) subp %>% select(phase, phaseID, id)
    else {
      subp %>% summarise(phase = first(phase),
                         phaseID = first(phaseID),
                         id = first(id))
    }
  }
  else {
    #here references to sea also stand for avoid areas on non-sea phases
    #does a gt circle from start to end still miss the land?
    #default is bycol..

    test_line <- st_gcIntermediate(p1=c(first(subp$long), first(subp$lat)),
                                   p2=c(last(subp$long), last(subp$lat)),
                                   n=nrow(subp)-2, addStartEnd=TRUE,
                                   crs=st_crs(useMap))
    #just extract the single binary result
    sea_only <- ! as.logical(st_intersects(test_line, useMap, sparse=FALSE))
    if (sea_only) {
      if (nrow(subp)==1) subp %>% select(phase, phaseID, id)
      else {
        subp %>% summarise(phase = first(phase),
                           phaseID = first(phaseID),
                           id = first(id))
      }
    }
    else {
      #do recursion.
      # d <- dist2gc(p1=c(first(subp$long), first(subp$lat)),
      #              p2=c(last(subp$long), last(subp$lat)),
      #              p3 = rbind(as.matrix(subp %>% select(long, lat)),
      #                         c(last(subp$long), last(subp$lat))))
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
                          mutate(phaseID=paste0(phaseID,"0")), withMap, avoidMap),
                 findGC(subp %>% slice(split:nrow(subp)) %>%
                          mutate(phaseID=paste0(phaseID,"1")), withMap, avoidMap))
    }
  }
}


#return an empty-ish route
emptyRoute <- function(ac, ap2, onMap,
                       levels=lattice_phases){

  data.frame(phase = factor(NA, levels = levels),
             phaseID = "0.",
             w_id = 0, from = 0, to = 0,
             time_h = NA_real_,
             from_long = ap2$from_long, from_lat = ap2$from_lat,
             to_long = ap2$to_long, to_lat = ap2$to_lat,

             gcdist_km = distGeo(c(ap2$from_long,ap2$from_lat),
                                 c(ap2$to_long,ap2$to_lat))/1000,
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
    mutate(gc = st_sfc(st_linestring(),crs=crs_latlong), #for want of anything else NULL sfc
           crow = st_gcIntermediate(p1=c(ap2$from_long,ap2$from_lat),
                                    p2=c(ap2$to_long,ap2$to_lat),
                                    n = 30, addStartEnd=TRUE,
                                    crs=crs_latlong),
           envelope = st_sfc(st_polygon(),crs=crs_latlong))
}


# wrapper for routeAnAircraft, including refuelling
findRoute <- function(ac, ap2, onMap, avoid, pg, cf_sub=NA,
                      refuel=NA, refuel_h=1, refuel_only_if_nec=TRUE,
                      refuel_topN=1,
                      max_circuity=2.0,
                      apLoc, ...){
  #cf_sub is either NA or a line from the ac dataframe, like ac itself
  #refuel is either NA or a dataframe list APICAO, lat, long at least, with ap_locs

  if (getOption("quiet",default=0)>0) print(paste("Route:-",ap2$AP2,"----")) #route header v refuel subroutes

  # #debug  - find good crs
  # useCRS <- "+proj=laea +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # onMap <- st_transform(onMap, crs=useCRS)
  # pg@points$xy <- st_transform(pg@points$xy, crs=useCRS)
  # if (!is.na(avoid)) avoid <- st_transform(avoid, useCRS)
  #
  #note ap2$routeID is in a specific order, but ADEP/ADES might not reflect that
  #switch if necessary
  ap2 <- ap2 %>%
    select(AP2, ADEP, ADES, ends_with("_long"), ends_with("_lat"), gcdistance_km) %>%
    mutate(ADEP=str_split(AP2,"(<>)|(>)|(<)",simplify=TRUE)[1],
           ADES=str_split(AP2,"(<>)|(>)|(<)",simplify=TRUE)[2])
  sep <- str_remove_all(ap2$AP2,paste0("(",ap2$ADEP,")|(",ap2$ADES,")")) #get separator "<>",">"
  unidirectional <- (sep==">")

  #can aircraft make it without refuelling?
  ap2range_ok <- ap2$gcdistance_km < ac$range_km

  #if yes - then get the route
  if (ap2range_ok) routes <- routeAnAircraft(ac, ap2, apLoc = apLoc,
                                             onMap=onMap, pg = pg, avoid = avoid, ...)
  else {
    if (getOption("quiet",default=0)>1) print("Too far for one leg.")
    routes <- emptyRoute(ac, ap2, onMap)
  }

  #do a parallel run for a subsonic aircraft - a true baseline?
  #not advised - just use the M084 estimate
  if (is.data.frame(cf_sub)) {
    if (getOption("quiet",default=0)>1) print("Adding subsonic, without range bounds.")
    r_subsonic <- routeAnAircraft(cf_sub, ap2, apLoc = apLoc,
                                  enforceRange = FALSE,
                                  onMap=onMap, pg = pg, avoid=avoid, ...)
    routes <- rbind(routes, r_subsonic)
  }

  #look for options including refuelling
  if (is.data.frame(refuel) & (!ap2range_ok || !refuel_only_if_nec)){
    #find triples: AREF in range of ADEP and ADES
    r_ap3 <- ap2 %>%
      crossing(refuel %>%
                 select(APICAO, long, lat) %>%
                 rename(AREF=APICAO, ref_long=long, ref_lat=lat)) %>%
      rowwise() %>%
      mutate(dep_ref_km = distGeo(c(from_long,from_lat),
                                  c(ref_long,ref_lat))/1000,
             ref_des_km = distGeo(c(ref_long,ref_lat),
                                  c(to_long,to_lat))/1000) %>%
      ungroup() %>%
      filter(dep_ref_km < ac$range_km & ref_des_km < ac$range_km) %>%
      filter((AREF != ADEP) & (AREF != ADES)) %>% #can't refuel at start or end!
      #if 1 or more under circuity then filter
      mutate(circuity = (dep_ref_km + ref_des_km)/ap2$gcdistance_km,
             n_under_circ = sum(circuity < max_circuity)) %>%
      filter((n_under_circ<2)|(circuity < max_circuity))

    #simplify to distinct AP2 (there may be some duplicates, eg Heathrow-Gander appearing for -SFO & -LAX)
    r_ap2 <- r_ap3 %>%
      select(ADEP, AREF, from_long, from_lat, ref_long, ref_lat) %>%
      rename(ADES=AREF, to_long=ref_long, to_lat=ref_lat) %>%
      rbind(r_ap3 %>%
              select(AREF, ADES, ref_long, ref_lat, to_long, to_lat) %>%
              rename(ADEP=AREF, from_long=ref_long, from_lat=ref_lat)) %>%
      distinct() %>%
      rowwise() %>%
      mutate(gcdistance_km = distGeo(c(from_long, from_lat),
                                     c(to_long, to_lat))/1000,
             AP2=ifelse(unidirectional,
                        paste(ADEP,ADES,sep=sep),
                        concat_ADEPADES(ADEP, ADES, FALSE))) #get it in the 'right' order (for the cache)


    # w <- lapply(1:nrow(r_ap2), function(x) routeAnAircraft(ac, r_ap2[x, ], onMap=onMap, pg = pg, avoid=avoid,
    #                                                             unidirectional=unidirectional, ...))
    w <- lapply(1:nrow(r_ap2), function(x) routeAnAircraft(ac,
                                                           make_AP2(r_ap2[x, ]$ADEP, r_ap2[x, ]$ADES, apLoc),
                                                           pg = pg, onMap=onMap, avoid=avoid,
                                                           apLoc = apLoc, ...))
    refuel_options <- reduce(w, rbind)

    #extract the best from these options
    #add some variables
    refuel_options <- refuel_options %>%
      mutate(legs=ifelse(routeID==ap2$AP2,1,2),
             leg_id = ifelse(grepl(ap2$ADEP,routeID),1,2),
             refuel_ap = str_remove_all(routeID,paste0("(",ap2$ADEP,")|(<>)|(>)|(",ap2$ADES,")")),
             routeID = ap2$AP2,
             fullRouteID=paste(ap2$ADEP,refuel_ap,ap2$ADES,sep=sep))

    best_routes <- refuel_options %>%
      group_by(fullRouteID) %>%
      summarise(time_h = sum(time_h),
                dist_km = sum(gcdist_km)) %>%
      filter(!is.na(time_h)) %>%  #drop any failed direct route
      top_n(-refuel_topN, time_h) %>%
      pull(fullRouteID)

    #now re-order to get the sequence right (only really imporatnt for speed profile)
    best_refuel_options <-  refuel_options %>%
      filter(fullRouteID %in% best_routes) %>%
      group_by(fullRouteID, leg_id) %>%
      #now check if need to reverse order
      #with refuelling we want the phases to be in the right order and join up in the middle
      mutate(grid_s = head(grid[[1]],1),grid_e = tail(grid[[1]],1),
             reverse = (leg_id==1 & grid_s != str_split(routeID,"(<>)|(>)",simplify=TRUE)[1])|
               (leg_id==2 & grid_e!= str_split(routeID,"(<>)|(>)",simplify=TRUE)[2]),
             order = if_else(reverse, -row_number(),row_number()),
             temp = from,
             from = if_else(reverse, to, from),
             to = if_else(reverse, temp, to),
             reverse = reverse & !(row_number()==n()), #don't reverse data in last row, since arr/dep leg is alway from AP
             temp= from_lat,
             from_lat = if_else(reverse, to_lat, from_lat),
             to_lat = if_else(reverse, temp, to_lat),
             temp= from_long,
             from_long = if_else(reverse, to_long, from_long),
             to_long = if_else(reverse, temp, to_long)) %>%
      arrange(fullRouteID, leg_id, order) %>%
      select(-grid_s, -grid_e, -reverse, -temp) #drop order later

    #base the refuel legs on the last row of the first leg - for lat long, etc
    refuel_legs <- best_refuel_options %>%
      group_by(fullRouteID) %>%
      filter(leg_id==1) %>%
      filter(row_number() == n()) %>%
      mutate(phase = factor("refuel", levels = lattice_phases),
             from=to,
             time_h=refuel_h,
             from_long=to_long, from_lat=to_lat,
             gcdist_km=0,
             speed_kph=0,
             leg_id=1.5)

    #need to use rbind not bind_rows because of the sf
    sel_routes <- rbind.data.frame(best_refuel_options, refuel_legs)
    sel_routes <-  sel_routes %>%
      arrange(fullRouteID, leg_id, order) %>%
      group_by(fullRouteID) %>%
      #renumber the phases
      mutate(phaseChange = case_when(
        row_number() == 1 ~ 1L,
        phase != lag(phase) ~ 1L,
        TRUE ~ 0L),
        phaseID = paste0(cumsum(phaseChange),".",str_split(phaseID,coll("."),simplify=TRUE)[,2]),
        timestamp = first(timestamp)) %>%
      select(-phaseChange, -order)

    routes <- rbind.data.frame(routes, sel_routes)

  }
  return(routes)
}


#cached SID-STAR
findToCToD <- function(ap, pg, onMap, ac,
                       ad_dist=ad_m, nearest = num_sid){
  #the original findToCToD accepted multiline ap & pg - for ease of caching no longer true
  stopifnot(nrow(ap)==1 & nrow(ac)==1)

  #can save and load the cache, with loadRDS readRDS
  #use attr "map" of onMap, and AircraftSet of ac to ensure it's the right cache
  #if cache doesn't exist, create it as a child of Global (so persists outside this function!)
  if (!exists("star_cache") || attr(star_cache,"map") != pg@name ||
      attr(star_cache,"aircraftSet") != attr(ac,"aircraftSet")) {
    assign("star_cache", new.env(parent=.GlobalEnv), .GlobalEnv)
    attr(star_cache,"map") <- pg@name
    attr(star_cache,"aircraftSet") <- attr(ac,"aircraftSet")}

  #cache the SID-STAR with data name which is the ACID, ap, nearest & ad_dist.
  cache_as <- paste(ac$id, ap$APICAO, nearest, ad_dist, sep="-")

  #if this query has not already been cached, calculate its value
  if (!exists(cache_as, envir=star_cache, inherits=F)) {
    if (getOption("quiet",default=0)>2) print("TOC/TOD not cached: calculating...")
    assign(cache_as, findToCToD_really(ap, pg, onMap, ac,
                                       ad_dist, nearest), star_cache)
  }
  #return value
  get(cache_as, star_cache)
}

#link airport to top of climb and descent
findToCToD_really <- function(ap, pg, onMap, ac,
                              ad_dist, nearest){
  #for a each point in ap - which is the airport list with locs
  #find where the airport connects to the 'cruise' grid points
  #not super accurate - because use st_distance rathe than distGeo

  y <- as.matrix(st_distance(pg@points$xy,ap$ap_locs)) #slow but simple
  #and less slow now that this is pg after the route envelope has been applied

  colnames(y)<- ap$APICAO
  w <- data.frame(y) %>%
    gather("AP","dist_m") %>%
    rename(from=AP) %>% #we use the AP ICAO code as the node ID
    group_by(from) %>%
    mutate(to = as.character(pg@points$id),
           dist_m = drop_units(dist_m),
           near_m = abs(dist_m - ad_dist)) %>% #compare to target distance
    arrange(near_m) %>%
    select(-near_m) %>%
    #take the top n, with 8 should be near the points of the compass
    slice(1:nearest) %>%
    #add time (cost) for each aircraft
    ungroup() %>%
    crossing(ac %>% select(id, arrdep_kph)) %>%
    mutate(cost = (dist_m/1000)/arrdep_kph) %>%
    #add the long lats
    left_join(ap %>% as.data.frame() %>% select(-ap_locs) %>%
                select(-AP, -APLongName, -TZ, -TZName, -TZ_to_Show), by=c("from"="APICAO")) %>%
    left_join(pg@points %>% select(id, long, lat, land) %>% mutate(id = as.character(id)),
              by = c("to"="id"), suffix=c("_ap","_grid")) %>%
    #if no transition leg, then take the acceleration subsonic cruise-supersonic penalty here
    mutate(cost = cost + if_else(land, 0, ac$trans_h)) %>%
    select(-land)

}


#convert the returned path to a series of great-circle arcs, by phase
pathToGC <- function(path, pg,
                     onMap, avoid,
                     arrDep,
                     ac,
                     byTime,
                     max_gcsteps=40, gckm_perstep=30,
                     checkShortcuts=TRUE, ...){
  #accepts either a list of one, or an unlisted path
  stopifnot(class(pg)=="GridLat")
  stopifnot(length(path)==1)

  if(class(path)=="list") path=unlist(path)
  #need to strip off the airports
  n <- length(path)
  sid <- path[1:2]
  star <- path[(n-1):n]

  #do sid and star first
  #create line for departure, using 0 as id for airport
  dep <- data.frame(phase = factor("arr/dep", levels=levels(pg@lattice$phase)),
                    phaseID = "0.",
                    w_id = 0, from=sid[1], to = sid[2],
                    steps = 1, stringsAsFactors = FALSE) %>%
    #add distance and time for the departure
    left_join(arrDep %>% select(-id, -arrdep_kph),
              by=c("from","to")) %>%
    #flip the types back to match gcid
    mutate(gcdist_km = dist_m/1000,
           from = 0, to = as.numeric(to)) %>%
    rename(from_long = long_ap, from_lat = lat_ap,
           to_long = long_grid, to_lat = lat_grid,
           time_h = cost) %>%
    select(-dist_m)

  #create line for arrival, using 0 as id for airport
  #the phaseID here will need updating if n>3
  arr <- data.frame(phase = factor("arr/dep", levels=levels(pg@lattice$phase)),
                    phaseID = "1.",
                    w_id = as.numeric(star[1]), from=star[1], to = star[2],
                    steps = 1, stringsAsFactors = FALSE) %>%
    #add distance and time for the departure
    left_join(arrDep %>% select(-id, -arrdep_kph),
              by=c("from"="to","to"="from")) %>%
    #flip the types back to match gcid
    mutate(gcdist_km = dist_m/1000,
           from = as.numeric(from), to = 0) %>%
    rename(from_long = long_ap, from_lat = lat_ap,
           to_long = long_grid, to_lat = lat_grid,
           time_h = cost) %>%
    select(-dist_m)

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
      left_join(pg@lattice %>% select(from, to, phase),
                by=c("from","to")) %>%
      left_join(pg@lattice %>% select(from, to, phase),
                by=c("from"="to","to"="from")) %>%
      mutate(phase=coalesce(phase.x, phase.y)) %>%
      select(-phase.x, -phase.y)

    if (ac$over_sea_M == ac$over_land_M | !byTime) {
      #if this is a subsonic aircraft - or distance only, then land/sea/transition phases are the same - land
      #and at this point the path only contains these 3
      p <- p %>%
        mutate(phase = factor("land",levels=levels(pg@lattice$phase)),
               phaseID = "1.")
    }
    else {
      #now calculate phase change
      p <- p %>%
        mutate(phaseChange = case_when(
          row_number()==1 ~ 1L,
          phase != lag(phase) ~ 1L,
          TRUE ~ 0L),
          phaseID = paste0(cumsum(phaseChange),".")) %>%
        select(-phaseChange)
    }
    if(getOption("quiet",default=0)>1) print("Calculated phase changes")
    p <- p %>%
      #now duplicate each row and simplify to a single id instead of from-to
      #deliberately create doubles then drop them
      #tidyr::gather ought to do this, but changes the order in a way I don't want
      rowwise() %>%
      uncount(2) %>%
      group_by(phaseID, from, to) %>%
      mutate(id = c(first(from), first(to))) %>%
      group_by(phaseID) %>%
      select(-from, -to) %>%
      distinct() %>%
      #need the long lat too
      left_join(pg@points %>% select(id, long, lat),
                by = c("id")) %>%
      #add in the distances from land - by phase since if <>"sea" then 0
      group_by(phase) %>%
      mutate(fromLand_km = if_else(phase == "sea",
                                   distFromLand(long, lat, onMap),
                                   distFromLand(long, lat, avoid))) %>%
      ungroup()

    #loop for each phase
    #trim path to list of id pairs, each of which can be joined by a GC
    #reduce collapes a list of data.frames to a data.frame
    if (getOption("quiet",default=0)>2) print("Ready to recurse")

    gcid <- purrr::reduce(lapply(unique(p$phaseID), function(i, m) findGC(p[p$phaseID==i,],
                                                                          onMap, avoid)),
                          bind_rows) %>%
      #after a single hop line you can start too late, so correct any gaps
      mutate(from = id,
             to = coalesce(lead(id),last(p$id))) %>%
      rename(w_id = id)

    if (getOption("quiet",default=0)>1) print("Done recursion")

    #code is split just because there's less to highlight in the debugger
    gcid <- gcid %>%
      #add back the geo data
      left_join(pg@points %>% select(id, long, lat),
                by = c("from"="id")) %>%
      rename(from_long = long, from_lat = lat) %>%
      left_join(pg@points %>% select(id, long, lat),
                by = c("to"="id")) %>%
      rename(to_long = long, to_lat = lat)

    #post-processing
    #loop to look for shortcuts
    if (checkShortcuts) {
      if (getOption("quiet",default=0)>1) print("Checking Shortcuts")
      baseID <- 1
      while (baseID < nrow(gcid)-1) {
        #skip this baseID if not 'sea'
        if (gcid[baseID,"phase"]=="sea"){
          #check the furthest first
          farID <- nrow(gcid)
          while (farID > baseID + 1) {
            #only check the geometry if all a sea phase
            phases <- unique(gcid[baseID:farID,"phase"])
            if (length(phases)==1) {
              # if (length(phases)==1 && phases == "sea") {
              #check from the start (from) of A to the far end (to) of B
              test_line <- st_gcIntermediate(p1=c(gcid[baseID,]$from_long, gcid[baseID,]$from_lat),
                                             p2=c(gcid[farID,]$to_long, gcid[farID,]$to_lat),
                                             n=20, addStartEnd=TRUE, crs=st_crs(onMap))
              #just extract the single binary result: All sea?
              all_sea <- ! as.logical(st_intersects(test_line, onMap, sparse=FALSE))
              if (all_sea) {
                #can drop the intermediate points
                if (getOption("quiet",default=0)>2) print(paste("Shortcut from",baseID,"to",farID))
                #first update the 'next step' info in the baseID
                gcid[baseID,c("to","to_long","to_lat")] <- gcid[farID,c("to","to_long","to_lat")]
                #then skip the intermediate points
                gcid <- gcid %>% slice(-((baseID+1):farID))
                #no need to search further for this baseID - so set farID to a value which stops this loop
                farID <- baseID
              }
            }
            farID <- farID - 1
          }
        }
        baseID <- baseID + 1
      }
    }

    gcid <- gcid %>%
      mutate(gcdist_km = distGeo(matrix(c(from_long, from_lat), ncol=2),
                                 matrix(c(to_long, to_lat), ncol=2))/1000,
             steps = floor(pmin(gcdist_km/gckm_perstep, max_gcsteps))) %>%
      group_by(phase) %>%
      mutate(time_h = time_h(phase, gcdist_km, ac=ac))

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
    mutate(gc = st_gcIntermediate(p1=c(from_long, from_lat),
                                  p2=c(to_long, to_lat),
                                  n=steps-1, addStartEnd=TRUE,
                                  crs = crs_latlong)) %>%
    select(-steps) %>%
    ungroup()
}


#this is the entry point, but it's a cache
routeAnAircraft <- function(ac, ap2, pg, onMap, apLoc,
                            avoid=NA, enforceRange=TRUE,
                            dijkstraByTime=TRUE, grace_km=NA,
                            unidirectional=FALSE,
                            checkShortcuts=TRUE,...){
  #can save and load the cache, with loadRDS readRDS
  #if cache doesn't exist, create it as a child of Global (so persists outside this function!)
  if (!exists("rte_cache") || attr(rte_cache,"map") != pg@name) {
    assign("rte_cache", new.env(parent=.GlobalEnv), .GlobalEnv)
    attr(rte_cache,"map") <- pg@name}

  if (unidirectional) {
    #note we use a different separator here, but not < > which file systems might reject
    cache_as <- paste(ac$id, ap2$ADEP, ap2$ADES, attr(avoid,"avoid"),
                      enforceRange, dijkstraByTime, grace_km, checkShortcuts, sep="_")
  }
  else {
    #cache the route with data name which is the ACID and AP2
    cache_as <- paste(ac$id, min(ap2$ADEP, ap2$ADES), max(ap2$ADEP, ap2$ADES),
                      attr(avoid,"avoid"), enforceRange,
                      dijkstraByTime, grace_km, checkShortcuts, sep="-")
  }

  #if this query has not already been cached, calculate its value
  if (!exists(cache_as, envir=rte_cache, inherits=F)) {
    if (getOption("quiet",default=0)>2) print("Not cached: calculating...")
    r <- routeAnAircraft_really(ac, ap2, pg, onMap, apLoc, avoid,
                                enforceRange, dijkstraByTime, grace_km,
                                checkShortcuts,...)
    if (is.na(r[1,1]))(return(r)) #quick end without caching if it's an empty route.
    assign(cache_as, r, rte_cache)
  }

  #return value
  get(cache_as, rte_cache)
}


#find a single route
routeAnAircraft_really <- function(ac, ap2, pg, onMap,
                                   apLoc,
                                   avoid=NA,
                                   enforceRange=TRUE,
                                   dijkstraByTime=TRUE,
                                   grace_km=NA,
                                   checkShortcuts=TRUE,...){
  #start with a grid, find the routes for this aircraft
  #ap2 is a row of a dataframe with at least AP2 from_long, from_lat, to_long, to_lat
  # dots are to allow passing the number of points to generate in the envelope
  # avoid is either NA, or a map in onMap crs of regions to avoid
  #dijkstraByTime means use time as cost function, if FALSE then pure distance
  #arrDep is the airport locations for getting the TOC TOD
  #default algorithm is bidirectional Dijkstra

  tstart <- Sys.time()
  stopifnot(class(pg)=="GridLat")

  if (getOption("quiet",default=0)>0) print(paste("Leg:",ap2$AP2," Aircraft:",ac$type))

  #get crow-flies
  crow <- st_gcIntermediate(p1=c(ap2$from_long,ap2$from_lat),
                            p2=c(ap2$to_long,ap2$to_lat),
                            n = 30, addStartEnd=TRUE,
                            crs=crs_latlong)

  #check can actually make it!
  gcdist <- distGeo(c(ap2$from_long,ap2$from_lat),
                    c(ap2$to_long,ap2$to_lat))/1000

  if ((gcdist > ac$range_km) & enforceRange) {
    if (getOption("quiet",default=0)>0) print(paste0("Distance ",round(gcdist,1),"km exceeds range ",round(ac$range_km,1),"km."))
    #return something mostly empty
    return(emptyRoute(ac, ap2, onMap))
  }

  if (getOption("quiet",default=0)>2) print(paste("Starting envelope:",round(Sys.time() - tstart,1)))
  #make the envelope - so can plot even if don't enforce it
  #we work with the envelope in map CRS, then save at last stage in crs_latlong
  envelope <-  st_sfc(st_polygon(),crs=st_crs(onMap)) #null by default
  if (gcdist <= ac$range_km) {

    #if necessary add a grace distance, to allow routing to be found if within 2%.
    if (!is.na(grace_km) & (gcdist/ac$range_km > 0.97)) ac$range_km <- ac$range_km + grace_km

    if (enforceRange) {
      #trim the points to the route Envelope
      envelope <-  routeEnvelope(ac, pg, ap2, st_crs(onMap), ...)

      #reduce the lattice - but do it via the points, because we don't want to cut lines up
      pg@points <- pg@points %>%
        filter(st_intersects(xy, envelope, sparse=FALSE))
      pg@lattice <- pg@lattice %>%
        inner_join(pg@points %>% select(id), by=c("from"="id")) %>%
        inner_join(pg@points %>% select(id), by=c("to"="id"))

      if (getOption("quiet",default=0)>1) print(paste("Cut envelope from lattice:",round(Sys.time() - tstart,1)))
    }
  }

  #get the arrDep routes
  #note here only 1-directional - the makegraph adds the other direction
  arrDep <-
    bind_rows(findToCToD(apLoc[apLoc$APICAO==ap2$ADEP,],pg,onMap,ac),
              findToCToD(apLoc[apLoc$APICAO==ap2$ADES,],pg,onMap,ac))

  #check if need to avoid areas
  if (!is.na(avoid)){
    #need an extra map
    #for over-sea flight ensure the avoid areas are included, so they are not allowed
    onMap <- st_union(onMap, avoid)
    if (getOption("quiet",default=0)>2) print(paste("Merged map and avoid areas:",round(Sys.time() - tstart,1)))
    #and for non-seas flight, also need to avoid 'avoid'
  }

  #in debug, quick plot of sea points in grid
  # ggplot(pg@points %>% filter(!land), aes(long, lat)) + geom_point(size=0.1)

  #add aircraft specific costs
  costed_lattice <- costLattice(pg, ac=ac) %>%
    select(from, to, cost, dist_km) %>%
    #add on the arrival dep - indices are now strings
    mutate_at(vars(from, to), ~as.character(.)) %>%
    bind_rows(arrDep %>%
                mutate(dist_km = dist_m/1000) %>%
                select(from, to, cost, dist_km))

  if (!dijkstraByTime) {costed_lattice <- costed_lattice %>%
    mutate(cost = dist_km) #if !bytime, just use distance..
  }
  if (getOption("quiet",default=0)>2) print(paste("Got costed lattice:",round(Sys.time() - tstart,1)))


  gr <- makegraph(costed_lattice %>% select(from, to, cost),
                  directed = FALSE)
  #with update to cppRouting v2.0 this stopped working
  # gr <- cpp_simplify(gr, iterate=TRUE)$graph #simplify the data

  #then get the grid route
  # path <- get_path_pair(gr, nearest_id(pg,c(ap2$from_long,ap2$from_lat)),
  #                       nearest_id(pg,c(ap2$to_long,ap2$to_lat)))
  path <- get_path_pair(gr, ap2$ADEP, ap2$ADES)

  if (length(path[[1]])>1) {
    if(getOption("quiet",default=0)>2) print(paste("Got path:",round(Sys.time() - tstart,1)))
  }
  else if (length(path[[1]])<2 ) {
    if (getOption("quiet",default=0)>0) {print(paste0("Failed to find path for ",ap2$AP2," (too far, or higher envelope_points)"))}
    #return something mostly empty
    return(emptyRoute(ac, ap2, onMap))
  }


  gcArcs <- pathToGC(path, pg, onMap, avoid, arrDep, ac, dijkstraByTime,
                     checkShortcuts=checkShortcuts) %>%
    #add identifying features
    #add grid, crow and envelope in the first one
    mutate(timestamp = rep(format(Sys.time(), "%Y%m%d_%H%M%OS2"),n()), #unique identifier
           routeID = ap2$AP2,
           acID = ac$id,
           acType = ac$type,
           #this is inefficient, saving multiple copies
           grid=c(path, rep(list(NA),n()-1)), #a list with just the grid in the first one
           crow = c(crow, rep(st_sfc(st_linestring(),crs=crs_latlong),n()-1)),
           envelope = c(prj(envelope,crs=crs_latlong),
                        rep(st_sfc(st_polygon(),crs=crs_latlong),
                            n()-1)),
           fullRouteID = ap2$AP2,
           legs = 1, leg_id = 1,
           refuel_ap = NA)

  #only if there is a sea phase can we smooth the accelerations out.
  if ("sea" %in% gcArcs$phase) smoothSpeed(gcArcs, ac)
  else gcArcs <- gcArcs %>% mutate(speed_kph = gcdist_km/time_h)
}



#routes from ap to ap2 must stay within this envelope
routeEnvelope <- function(ac, pg, ap2, crs,
                          envelope_points=200,
                          fuzz=0.005){
  #add a 0.5% fuzz to allow longest range to be flown
  geo_m <- distGeo(c(ap2$from_long,ap2$from_lat),c(ap2$to_long,ap2$to_lat))
  geo_c <- midPoint(c(ap2$from_long,ap2$from_lat),c(ap2$to_long,ap2$to_lat))

  r <- (ac[1,]$range_km * 1000) *(1 + fuzz)
  a <- r/2
  b <- sqrt((r/2)^2-(geo_m/2)^2)
  psi <- bearing(geo_c,c(ap2$to_long, ap2$to_lat))

  theta <- seq(0, 360, length.out = envelope_points)
  tp_rad <- theta*pi/180
  #polar form for radius of an ellipse from the centre with semi-major axis length a
  #and starting with the longest

  dist <- a * b / sqrt(a^2*sin(tp_rad)^2 + b^2*cos(tp_rad)^2)
  geod <- geodesic(geo_c, theta + psi, dist)

  boundary <- st_cast(st_cast(
    st_transform(st_sfc(st_multipoint(geod[,1:2]),crs=4326),crs=crs),
    'LINESTRING'),
    'POLYGON')
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
           speed_kph= gcdist_km/time_h) #useful initial value
  #spread the acceleration cost forwards
  r <- smooth1(1, r, ac)
  #spread the deceleration cost backwards
  r <- smooth1(-1, r, ac)


  return(r %>% select(-penalty))
}
