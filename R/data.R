#' Simplified polygons of New Zealand.
#'
#' A dataset containing sf::MULTIPOLYGONS for New Zealand. Simplified version of Stats NZ data,
#' at 1km resolution.
#'
#' @format MULTIPOLYGON in original coordinates EPSG 2193
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
