#' himach: A package for computing supersonic aircraft routes
#'
#' The himach (high Mach) package finds the quickest route between airports, for
#' supersonic aircraft that fly subsonic over land.
#'
#'
#' It allows for a coastal buffer and potentially closed regions of airspace. It
#' uses a minimal model of aircraft performance: focus is on time saved versus
#' subsonic flight, rather than a detailed vertical flight profile. Subsonic
#' aircraft can be routed too, for comparison.
#'
#' Mach cut-off flying is also possible by creating aircraft with supersonic
#' cruise speed over land.
#'
#' The package essentially combines the functionality of \code{cppRouting} for
#' finding routes and \code{sf} for handling map 'simple features', with a lot
#' of help from the \code{tidyverse}, of course. In the latest version it uses
#' direct spherical geometry, either directly through package \code{s2} or
#' indirectly through support to \code{s2} from package \code{sf}.
#'
#' @import cppRouting
#' @import sf
#' @import dplyr
#'
#' @docType package
#' @name himach
NULL
