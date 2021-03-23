# cache management functions


#' Clean the route and SID-STAR caches.
#'
#' Empties the caches.
#'
#' @seealso For more details see the cache section in the vignette:
#'   \code{vignette("AdvancedRouting", package = "Mach2")}. or
#'   \href{../doc/AdvancedRouting.html#cache}{Vignette on caching}
#'
#' @param cache Which caches to clear. Default is both \code{c("route",
#'   "star")}.
#'
#' @return TRUE silently
#' @export
#'
#' @examples
#' m2_clean_cache("route")
#'
#' m2_clean_cache()
#'
m2_clean_cache <- function(cache = c("route", "star")){
  stopifnot(length(intersect(c("route", "star"), cache))>0)
  if ("route" %in% cache) rm(list = ls(.m2_cache$route_cache), pos=.m2_cache$route_cache)
  if ("star" %in% cache) rm(list = ls(.m2_cache$star_cache), pos=.m2_cache$star_cache)
  invisible(TRUE)
}

#' Save route and SID/STAR caches to file
#'
#' Filename is \code{"route_star_cache_id_XXX.rda"} where "id" is the id
#' parameter and XXX is made up from the name of the grid (which identifies the
#' map used) and the 'aircraftSet' attribute of the aircraft dataset (which
#' identifies the source). This is because the cache should be for a unique
#' combination of these (and you must have these available, because they were
#' needed to generate the routes).
#'
#'
#' @param id Identifying text, see above. Redcommended to use a version number
#'   or date.
#' @param grid Your route grid dataset. The \code{grid@name} will be added to
#'   the filename.
#' @param aircraft Your aircraft dataset. The \code{attr(aircraft,
#'   "aircraftSet")} will be added to the filename.
#' @param path By default \code{"data/"}, where the file will be saved.
#'
#' @seealso #' For more details see the cache section in the vignette:
#'   \code{vignette("AdvancedRouting", package = "Mach2")}
#'
#' @return Invisible true
#' @export
#'
#' @examples
#' # not run
#' # m2_save_caches("v2", grid, ac) #save here
#'
#'
m2_save_caches <- function(id, grid, aircraft, path = "data/"){
  filename <- paste0("route_star_cache_",
                     id,
                     "_",
                     grid@name,
                     "_",
                     attr(aircraft, "aircraftSet"),
                     ".rda")
  save("route_cache", "star_cache",
       envir = .m2_cache,
       file = paste0(path, stringr::str_replace_all(filename, "\\s", "_")))
  invisible(TRUE)
}

#' Load route and SID/STAR caches
#'
#' This silently overwrites any existing values in the cache.
#'
#' @param file Including the path.
#'
#' @return Invisible true
#' @export
#'
#' @examples
#' # not run
#' # m2_load_caches(file="") #load from this file
#'
m2_load_caches <- function(file){
  load(file, envir = .m2_cache)
}
