# utils
# Private helper functions for 2speed package
# that do not fit into the routes, grids or maps collections

# utils::globalVariables(c("mach_kph", "crs_Pacific", "crs_longlat"))

# helper to put European names first - assumes 4-letter ICAO code
isEur <- function(x) substr(x,1,1) %in% c("E","L")

#put ADEP ADES together with European first
paste_ADEPADES <- function(ADEP, ADES, unidirectional=FALSE){
  #vector adep ades is ok
  sep <- if_else(unidirectional, ">", "<>")
  AP2 <- case_when(
    isEur(ADEP) & isEur(ADES) ~ paste(pmin(ADEP, ADES), pmax(ADEP, ADES), sep=sep),
    isEur(ADEP) ~ paste(ADEP, ADES, sep=sep),
    isEur(ADES) ~ paste(ADES, ADEP, sep=sep),
    TRUE ~ paste(pmin(ADEP, ADES), pmax(ADEP, ADES), sep=sep))
}



#copy a list of attributes from one dataset to another
copy_attr <- function(from, to, atts){
  lapply(atts, function(x) {
    if (is.null(attr(from,x))){
      warning("Warning: Attribute ",x," not found to copy.")
    }
    attr(to,x) <- attr(from,x)
    p <- parent.env(environment()) #need to pass the value back to 'to' in the copy_attr environment
    assign("to",to,p)
  })
  return(to)
}

#' Make airport-pair dataset
#'
#' \code{make_AP2} creates an airport-pair set from two sets of airports
#'
#' This function takes two lists of airports (of the same length), specified
#' as 4-letter codes and combines them, adding the fields:
#'
#' \itemize{
#'   \item \code{from_long, from_lat, to_long, to_lat}: the airport lat-longs
#'    with adep first
#'   \item \code{AP2}: a name for the route in a specific order
#'   \item \code{gcdist_km}: the great circle distance in km
#' }
#'
#' In \code{AP2} European airports (crudely, from starting letter = 'E' or 'L')
#' are listed first, otherwise in alphabetical order. If unidirectional is TRUE,
#' then ">" is the separator, otherwise "<>".
#' (Unidirectional not currently supported)
#'
#' For more details see the \href{../doc/Supersonic_Routes.html}{introductory vignette}.
#'
#' @param adep,ades Identical-length lists of airport codes
#' @param ap List of locations of airports, defaults to the output
#'     of \code{\link{make_airports}}.
#'
#' @return Dataframe with additional variables as described above.
#'
#' @examples
#'
#' airports <- make_airports() #get a default set of lat-longs
#' ap2 <- make_AP2("NZAA","NZCH", airports)
#'
#' @importFrom dplyr %>%
#'
#' @export
make_AP2 <- function(adep, ades, ap=make_airports()){
  stopifnot(length(adep)==length(ades))

  missing_AP <- setdiff(union(adep, ades), ap$APICAO)
  if (length(missing_AP) > 0) stop("Airport(s) ",
                                   paste(missing_AP, collapse = " "),
                                   " unknown.")

  #only bidirectional is currently supported
  unidirectional=FALSE

  data.frame(ADEP=adep, ADES=ades, stringsAsFactors = FALSE) %>%
    left_join(ap %>%
                dplyr::select(.data$APICAO, .data$long, .data$lat) %>%
                dplyr::rename(ADEP=.data$APICAO, from_long=.data$long, from_lat=.data$lat),
              by="ADEP") %>%
    left_join(ap %>%
                dplyr::select(.data$APICAO, .data$long, .data$lat) %>%
                dplyr::rename(ADES=.data$APICAO, to_long=.data$long, to_lat=.data$lat),
              by="ADES") %>%
    dplyr::mutate(AP2 = paste_ADEPADES(.data$ADEP, .data$ADES, unidirectional),
                  gcdist_km = geosphere::distGeo(c(.data$from_long, .data$from_lat),
                                                     c(.data$to_long, .data$to_lat))/1000)
}


# projection function - for more readable code in full_map
# in utils because used both in maps and elsewhere
# crs=54030 Robinson works well, but avoid cropping - a classic
# for the Pacific crs=CRS("+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
prj <- function(x, crs) {
  st_transform(x, crs = crs, quiet=FALSE)
}

#' Make aircraft data from minimum dataset
#'
#' \code{make_aircraft} ensures a minimum set of variables describing aircraft
#'
#' This function provides a test set of aircraft if necessary and adds variables
#' to a minimal set of data to give all the information that will be needed.
#'
#' This minimal set needs to have the following fields:
#'
#' \itemize{
#'   \item \code{id, type}: a very short, and longer text identifier for this aircraft
#'   \item \code{over_sea_M, over_land_M}: the eponymous two speeds, given as a Mach number
#'   \item \code{accel_Mpm}: acceleration in Mach per minute between these two
#'   \item \code{arrdep_kph}: the speed on arrival and departure from airports, given in km per hour
#'   \item \code{range_km}: range in km
#' }
#'
#' An attribute is set to help keep track of where the aircraft data came from
#' (and whether a new cache is needed). If the \code{aircraftSet} attribute of
#' the \code{ac} parameter is not set, the set is treated as 'disposable'.
#'
#'
#' For more details see the help vignette:
#' \code{vignette("SupersonicRouting", package = "himach")}
#'
#' @param ac Dataframe containing the minimum fields, or NA (default)
#' @param sound_kph Speed of sound used to convert from Mach to kph, default
#'     \code{mach_kph}=1062 at a suitable altitude.
#' @param warn Warn if no \code{ac} supplied, so default set is used. Default TRUE.
#'
#' @return Dataframe with at least 11 variables describing the performance of one or
#'      more aircraft
#'
#' @examples
#' # do minimal version (we know it will use the default so turn off warning)
#' ac <- make_aircraft(warn = FALSE)
#'
#' # on-the-fly example
#' ac <- data.frame(id = "test", type = "test aircraft",
#'                  over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
#'                  arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
#' ac <- make_aircraft(ac, warn = FALSE)
#'
#' \dontrun{
#' # example for your own data
#' aircraft <- utils::read.csv("data/aircraft.csv", stringsAsFactors = FALSE)
#' aircraft <- make_aircraft(aircraft)
#' # strongly recommended to record the file name for later reference
#' attr(aircraft, "aircraftSet") <- "aircraft.csv"
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
make_aircraft <- function(ac = NA, sound_kph = himach::mach_kph, warn = TRUE){
  if (!is.data.frame(ac)) {
    if (warn) warning("Using default aircraft file.")
    file <- system.file("extdata", "test_aircraft.csv", package = "himach", mustWork = TRUE)
    ac <- utils::read.csv(file, stringsAsFactors = FALSE)
    attr(ac, "aircraftSet") <- "test_aircraft" #keep track
  }

  req_vbls <- c("id", "type", "over_sea_M",  "over_land_M",
                "accel_Mpm", "arrdep_kph", "range_km")
  miss_vbls <- setdiff(req_vbls, names(ac))
  if (length(miss_vbls) > 0) stop("Aircraft definition is missing: ",
                                  paste(miss_vbls, collapse = " "))

  num_vbls <- c("over_sea_M",  "over_land_M",
                "accel_Mpm", "arrdep_kph", "range_km")
  miss_vbls <- setdiff(num_vbls, names(dplyr::select_if(ac, is.numeric)))
  if (length(miss_vbls) > 0) stop("These variables should be numeric: ",
                                  paste(miss_vbls, collapse = " "))

  ac_full <- ac %>%
    #make sure all the same type - double, not integer
    dplyr::mutate_if(is.numeric, as.double) %>%
    dplyr::mutate(over_sea_kph = .data$over_sea_M*sound_kph,
           over_land_kph = .data$over_land_M*sound_kph,
           trans_kph = (.data$over_sea_kph + .data$over_land_kph)/2,
           #transition penalty is time to change from over_sea to over_land speed (or v.v)
           trans_h = (.data$over_sea_M - .data$over_land_M)/(.data$accel_Mpm * 60))

  if (is.null(attr(ac, "aircraftSet"))) {
    attr(ac_full, "aircraftSet") <- "Dummy aircraft"
  } else {
    attr(ac_full, "aircraftSet") <- attr(ac, "aircraftSet")

  }
  return(ac_full)
}


#' Make or load airport data
#'
#' \code{make_airports} ensures a minimum set of variables describing airports
#'
#' This function provides a test set of airports if necessary from
#' \code{airportr::airports} and geocodes the lat-long of this or the dataset
#' provide as \code{ap}.
#'
#' This minimal set needs to have the following fields:
#'
#' \itemize{
#'   \item \code{APICAO}: the 4-letter ICAO code for the airport (though there is no
#'   validity check applied, so 'TEST', or 'ZZZZ' could be used, for example)
#'   \item \code{lat, long}: latitude and longitude in decimal degrees
#' }
#'
#' @param ap Dataframe containing the minimum fields, or NA (default)
#' @param crs Coordinate reference system for the coded lat-longs.
#'     Default 4326.
#' @param warn warn if default set is used (default = TRUE)
#'
#' @return Dataframe with, in addition, a geocoded lat-long.
#'
#' @examples
#' # do minimal version
#' airports <- make_airports()
#'
#' # on-the-fly example
#' airports <- data.frame(APICAO = "TEST", lat = 10, long = 10, stringsAsFactors = FALSE)
#' airports <- make_airports(airports)
#'
#' \dontrun{
#' # example for your own data
#' airports <- utils::read.csv("data/airports.csv", stringsAsFactors = FALSE)
#' airports <- make_airports(airports)
#' }
#'
#' @import sf
#' @importFrom dplyr %>%
#'
#' @export
make_airports <- function(ap = NA, crs = 4326, warn = TRUE){
  if (!is.data.frame(ap)) {
    if (warn) message("Using default airport data: airportr::airport.")
    ap <- airportr::airports %>%
      dplyr::filter(.data$Type == "airport") %>%
      dplyr::select(.data$ICAO, .data$Latitude, .data$Longitude) %>%
      dplyr::rename(APICAO = .data$ICAO, lat = .data$Latitude, long = .data$Longitude)
  }

  req_vbls <- c("APICAO", "long", "lat")
  miss_vbls <- setdiff(req_vbls, names(ap))
  if (length(miss_vbls) > 0) stop("Airport definition is missing: ",
                                  paste(miss_vbls, collapse = " "))

  ap %>%
    #convert to map feature
    # 4326 is a lat-long format, for input, then transform to required crs
    dplyr::mutate(ap_locs = st_transform(
      st_cast(st_sfc(
        st_multipoint(matrix(c(.data$long, .data$lat),ncol=2)), crs = himach::crs_longlat),
        'POINT'), crs = crs))
}

# rename a dataset in an environment,
# replacing *one* find of a substring with replace
ren_subst <- function(ds,
                      find_str, replace_str,
                      in_env){
  # ds is a string
  # copy with new name
  assign(sub(find_str, replace_str, ds),
         get(ds, in_env), in_env)
  #get rid of old one
  rm(list = ds, envir = in_env)
  TRUE
}

#wrapper for geosphere::gcIntermediate that returns an sfc object
st_gcIntermediate <- function(crs, ...){
  #not vector-clever for n, which is (single) integer
  #starts with 4326 - any old lat long and then transform to the required crs
  st_transform(
    st_sfc(st_linestring(geosphere::gcIntermediate(...)), crs=4326),
    crs)
}


# simple wrapper to update a n existing progress bar
# every nstep rows, if passed row_number as n
withProgress <- function(pb, f, ...){
  pb$tick()
  f(...)
}

# normally this behaviour creates warnings so circumvent
# but the warnings are there for a reason
# so beware of the original crs changing!
reassert_crs <- function(x, crs = himach::crs_Pacific){
  suppressWarnings(x <- sf::st_set_crs(x, crs))
  return(x)
}

#' Get test data
#'
#' Access 5 datasets that are used in vignettes and in testing.
#'
#'
#' @param item Any one of \code{"coast", "buffer", "nofly", "grid", "route"}. See details.
#'
#' @details
#' \describe{
#'   \item{\code{"coast"}}{A dataset containing sf::MULTIPOLYGONS for New Zealand. Simplified
#'   version of Stats NZ data, at 1km resolution.}
#'   \item{\code{"buffer"}}{As \code{"coast"} but with an added 30km buffer to keep supersonic
#'   flight away from the coast.}
#'   \item{\code{"nofly"}}{As \code{"buffer"}, but limited to Buller district with a 40km buffer.
#'   To test additional no-fly zones.}
#'   \item{\code{"grid"}}{Latitude-longitude-based routing grid around New Zealand at 30km target
#' distance, as generated by \code{\link{make_route_grid}}, so format is \linkS4class{GridLat}}
#'   \item{\code{"route"}}{Some very unlikely supersonic routes around New Zealand using the test
#' aircraft that was given a very short range and slow subsonic cruise to get
#' the example to 'work'. Includes one refuelling stop (!) in Wellington. [Not for operational use!]
#' Returns a dataframe.}
#' }
#'
#' This is not the normal way to access package test data. But the usual, direct, way fails on
#' some machines that have some older software (a known feature of the `sf` package). This is a
#' least-ugly workaround.
#'
#' @return See list above
#' @source
#'   \url{https://datafinder.stats.govt.nz/layer/104266-territorial-authority-2020-clipped-generalised/}
#'
#' @export
#'
#' @examples
#' NZ_coast <- hm_get_test("coast")
#'
hm_get_test <- function(item = c("coast", "buffer", "nofly", "grid", "route")){
  stopifnot(item %in% c("coast", "buffer", "nofly", "grid", "route"))
  item <- substr(item, 1, 1)
  # these three are already crs_Pacific
  if (item == "c")  z <- reassert_crs(NZ_coast)
  if (item == "b")  z <- reassert_crs(NZ_buffer30)
  if (item == "n")  z <- reassert_crs(NZ_Buller_buffer40)
  if (item == "g") {
    z <- NZ_grid
    z@points$xy <- reassert_crs(z@points$xy)
    z@lattice$geometry <- reassert_crs(z@lattice$geometry)
  }
  if (item == "r"){
    z <- NZ_routes %>%
      mutate(across(c(.data$gc, .data$crow, .data$envelope),
                    reassert_crs, crs_longlat))
  }
  return(z)
}
