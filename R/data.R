
#' Lat-long coordinate reference system
#'
#' Coordinate reference system (CRS) for creating maps from
#' longitude-latitude coordinates. Used in analysis, but
#' not recommended for plots.
#'
#' \code{crs_longlat} is EPSG4326
#'
#' @seealso \code{\link{crs_Atlantic}}, \code{\link{crs_Pacific}},
#'  \code{\link{crs_S}}, \code{\link{crs_N}}
#'
#' @format CRS
#'
"crs_longlat"

#' Atlantic-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps.
#' Atlantic-centred. Works for most analysis, but not recommended for N-region
#' (eg New Zealand and Fiji), instead use \code{\link{crs_Pacific}}.
#'
#' crs_Atlantic is "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84
#' +units=m +no_defs"
#'
#' @seealso \code{\link{crs_Pacific}}, \code{\link{crs_120E}},
#'   \code{\link{crs_N}}, \code{\link{crs_S}}
#'
#' @format CRS
#'
"crs_Atlantic"

#' Pacific-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps.
#' Pacific-centred.
#'
#' "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m
#' +no_defs"
#'
#' @format CRS
#'
#' @seealso \code{\link{crs_Atlantic}}, \code{\link{crs_120E}},
#'   \code{\link{crs_N}}, \code{\link{crs_S}}
#'
"crs_Pacific"

#' Asia-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps. Centred on
#' East Asia (120E).
#'
#' "+proj=robin +lon_0=120 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m
#' +no_defs"
#'
#' @format CRS
#'
#' @seealso \code{\link{crs_Atlantic}}, \code{\link{crs_Pacific}},
#'   \code{\link{crs_N}}, \code{\link{crs_S}}
#'
"crs_120E"

#' Arctic-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps.
#' WGS 84 / Arctic Polar Stereographic. Used in analysis, but
#' not recommended for plots.
#'
#' crs_N is EPSG3995
#'
#' @seealso \code{\link{crs_Atlantic}}, \code{\link{crs_Pacific}},
#'  \code{\link{crs_120E}}, \code{\link{crs_longlat}}, \code{\link{crs_S}}
#'
#' @format CRS
#'
"crs_N"

#' Antarctic-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps.
#' WGS 84 / Antarctic Polar Stereographic. Used in analysis, but
#' not recommended for plots.
#'
#' crs_N is EPSG 3031
#'
#' @seealso \code{\link{crs_Atlantic}}, \code{\link{crs_Pacific}},
#'  \code{\link{crs_120E}}, \code{\link{crs_longlat}}, \code{\link{crs_N}}
#'
#' @format CRS
#'
"crs_S"


#' Speed of sound, for Mach to km conversion
#'
#' 1 Mach is approximately 1062kph in standard met conditions at the altitude
#' for supersonic flight (approx 50,000 feet).
#'
#' @format double
#'
"mach_kph"

