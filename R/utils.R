# utils
# Private helper functions for 2speed package
# that do not fit into the routes, grids or maps collections


#helper to put European names first - assumes 4-letter ICAO code
isEur <- function(x) substr(x,1,1) %in% c("E","L")

#put ADEP ADES together with European first
concat_ADEPADES <- function(ADEP, ADES, unidirectional=FALSE){
  #vector adep ades is ok
  sep <- if_else(unidirectional, ">", "<>")
  AP2 <- case_when(
    isEur(ADEP) & isEur(ADES) ~ paste(pmin(ADEP,ADES),pmax(ADEP,ADES),sep=sep),
    isEur(ADEP) ~ paste(ADEP,ADES,sep=sep),
    isEur(ADES) ~ paste(ADES,ADEP,sep=sep),
    TRUE ~ paste(pmin(ADEP,ADES),pmax(ADEP,ADES),sep=sep))
}



#copy a list of attributes from one dataset to another
copy_attr <- function(from, to, atts){
  lapply(atts, function(x) {
    if (is.null(attr(from,x))){
      warning("Warning: Attribute",x,"not found to copy.")
    }
    attr(to,x) <- attr(from,x)
    p <- parent.env(environment()) #need to pass the value back to 'to' in the copy_attr environment
    assign("to",to,p)
  })
  return(to)
}

#for vectors adep, ades, make AP2 list (ie including long lat)
make_AP2 <- function(adep, ades, ap, ...){
  stopifnot(length(adep)==length(ades))

  data.frame(ADEP=adep, ADES=ades, stringsAsFactors = FALSE) %>%
    left_join(ap %>% dplyr::select(APICAO, long, lat) %>% dplyr::rename(ADEP=APICAO, from_long=long, from_lat=lat),
              by="ADEP") %>%
    left_join(ap %>% dplyr::select(APICAO, long, lat) %>% dplyr::rename(ADES=APICAO, to_long=long, to_lat=lat),
              by="ADES") %>%
    dplyr::mutate(AP2 = concat_ADEPADES(ADEP, ADES, ...),
           gcdistance_km = distGeo(c(from_long, from_lat),
                                   c(to_long, to_lat))/1000)
}


# projection function - for more readable code in full_map
# in utils because used both in maps and elsewhere
# crs=54030 Robinson works well, but avoid cropping - a classic
# for the Pacific crs=CRS("+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
prj <- function(x,crs=crs_map) {
  st_transform(x, crs=crs, quiet=FALSE)
}

#' Expand minimum set of aircraft data
#'
#' \code{expand_aircraft} ensures a minimum set of variables describing aircraft
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
#' @param ac Dataframe containing the minimum fields, or NA (default)
#' @param sound_kph Speed of sound used to convert from Mach to kph, default 1062
#'
#' @return Dataframe with at least 11 variables describing the performance of one or
#'      more aircraft
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
#' \dontrun{
#' # example for your own data
#' aircraft <- read.csv("data/aircraft.csv", stringsAsFactors = FALSE)
#' aircraft <- expand_aircraft(aircraft)
#' # strongly recommended to record the file name for later reference
#' attr(aircraft, "aircraftSet") <- "aircraft.csv"
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
expand_aircraft <- function(ac = NA, sound_kph = 1062){
  if (is.na(ac[1])) {
    warning("Using default aircraft file.")
    file <- system.file("extdata", "test_aircraft.csv", package = "twospeed", mustWork = TRUE)
    ac <- read.csv(file, stringsAsFactors = FALSE)
    attr(ac, "aircraftSet") <- "test_aircraft" #keep track
  }

  req_vbls <- c("id", "type", "over_sea_M",  "over_land_M",
                "accel_Mpm", "arrdep_kph", "range_km")
  miss_vbls <- setdiff(req_vbls, names(ac))
  if (length(miss_vbls) > 0) stop("Aircraft definition is missing: ",
                                  paste(miss_vbls, collapse = " "))

  ac_full <- ac %>%
    dplyr::mutate(over_sea_kph = over_sea_M*sound_kph,
           over_land_kph = over_land_M*sound_kph,
           trans_kph = (over_sea_kph + over_land_kph)/2,
           #transition penalty is time to change from over_sea to over_land speed (or v.v)
           trans_h = (over_sea_M - over_land_M)/(accel_Mpm * 60))

  attr(ac_full, "aircraftSet") <- attr(ac, "aircraftSet")

  return(ac_full)
}


#' Expand minimum set of airport data
#'
#' \code{expand_airports} ensures a minimum set of variables describing airports
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
#'
#' @return Dataframe with, in addition, a geocoded lat-long.
#'
#' @examples
#' # do minimal version
#' airports <- expand_airports()
#'
#' # on-the-fly example
#' airports <- data.frame(APICAO = "TEST", lat = 10, long = 10, stringsAsFactors = FALSE)
#' airports <- expand_airports(airports)
#'
#' \dontrun{
#' # example for your own data
#' airports <- read.csv("data/airports.csv", stringsAsFactors = FALSE)
#' airports <- expand_airports(airports)
#' }
#'
#' @import sf
#' @importFrom magrittr %>%
#'
#' @export
expand_airports <- function(ap = NA){
  if (is.na(ap[1])) {
    warning("Using default airport file.")
    ap <- airportr::airports %>%
      dplyr::filter(Type == "airport") %>%
      dplyr::select(ICAO, Latitude, Longitude) %>%
      dplyr::rename(APICAO = ICAO, lat = Latitude, long = Longitude)
  }

  req_vbls <- c("APICAO", "long", "lat")
  miss_vbls <- setdiff(req_vbls, names(ap))
  if (length(miss_vbls) > 0) stop("Airport definition is missing: ",
                                  paste(miss_vbls, collapse = " "))

  ap <- ap %>%
    #convert to map feature
    # 4326 is a lat-long format
    dplyr::mutate(ap_locs = st_cast(st_sfc(
      st_multipoint(matrix(c(long, lat),ncol=2)),crs=4326),'POINT'))
}

#rename a dataset in an environment, replacing *one* find with replace
#returns ds not found error - but does the job!
ren_subst <- function(ds,find_str,replace_str,in_env){
  # ds is a string
  #copy with new name
  assign(sub(find_str,replace_str,ds),
         get(ds, in_env), in_env)
  #get rid of old one
  rm(ds, envir=in_env)
  TRUE
}


#wrapper for geosphere::gcIntermediate that returns an sfc object
st_gcIntermediate <- function(crs, ...){
  #not vector-clever for n, which is (single) integer
  #starts with 4326 - any old lat long and then transform to the required crs
  st_transform(
    st_sfc(st_linestring(gcIntermediate(...)),crs=4326),
    crs)
}


