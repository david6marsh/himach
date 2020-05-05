#' Mach2: A package for computing supersonic aircraft routes
#'
#' The Mach2 package finds the quickest route between airports, for
#' supersonic aircraft that fly subsonic over land.
#'
#'
#' It allows for a coastal buffer and potentially closed regions of airspace. It
#' uses a minimal model of aircraft performance: focus is on time saved versus
#' subsonic flight, rather than a detailed vertical flight profile. Subsonic
#' aircraft can be routed too, for comparison.
#'
#' And Mach cut-off flying is also possible.
#'
#' The package essentially combines the functionality of \code{cppRouting} for
#' finding routes and \code{sf} for handling map 'simple features', with a lot
#' of help from the \code{tidyverse}, of course.
#'
#' @import cppRouting
#' @import sf
#' @import dplyr
#'
#' @docType package
#' @name Mach2
NULL
