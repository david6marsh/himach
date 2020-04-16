#' Simplified polygons of New Zealand.
#'
#' A dataset containing sf::MULTIPOLYGONS for New Zealand. Simplified version of Stats NZ data,
#' at 1km resolution.
#'
#' @format MULTIPOLYGON in original coordinates EPSG 2193
#'
#' @seealso \code{\link{NZ_b}}
#'
#' @source \url{https://datafinder.stats.govt.nz/layer/104266-territorial-authority-2020-clipped-generalised//}
"NZ_u"

#' New Zealand polygons with 30km land buffer
#'
#' A dataset containing sf::MULTIPOLYGONS for New Zealand. Uses simplified version of Stats NZ data,
#' at 1km resolution \code{NZ_s}, with added 30km buffer to all coasts.
#'
#' @format MULTIPOLYGON in original coordinates EPSG 2193
#'
#' @source \url{https://datafinder.stats.govt.nz/layer/104266-territorial-authority-2020-clipped-generalised//}
"NZ_b"

#' Atlantic-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps.
#' Atlantic-centred. Works for most analysis, but not recommended for
#' N-region (eg New Zealand and Fiji), instead use \code{\link{crs_Pacific}}.
#'
#' crs_Atlantic <- 54030
#'
#' @seealso \code{\link{crs_Pacific}}
#'
#' @format CRS
#'
"crs_Atlantic"


#' Pacific-centred coordinate reference system
#'
#' Coordinate reference system (CRS) for plotting and analysing maps.
#' Pacific-centred.
#'
#' "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'
#' @format CRS
#'
#' @seealso \code{\link{crs_Atlantic}}
#'
"crs_Pacific"

#' Speed of sound, for Mach to km conversion
#'
#' 1 Mach is approximately 1062kph in standard met conditions at
#' the altitude for supersonic flight (approx 50,000 feet).
#'
#' @format double
#'
"mach_kph"
