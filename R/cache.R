# cache management functions


#' Clean the route and SID-STAR cache.
#'
#' Empties the cache.
#'
#' @seealso For more details see the cache section in the vignette:
#'   \code{vignette("Supersonic_Routes_in_depth", package = "himach")}. or
#'   \href{../doc/Supersonic_Routes_in_depth.html#cache}{Vignette on caching}
#'
#' @param cache Which caches to clear. Default is both \code{c("route",
#'   "star")}.
#'
#' @return TRUE silently
#' @export
#'
#' @examples
#' hm_clean_cache("route")
#'
#' hm_clean_cache()
#'
hm_clean_cache <- function(cache = c("route", "star")){
  stopifnot(length(intersect(c("route", "star"), cache))>0)
  if ("route" %in% cache) rm(list = ls(.hm_cache$route_cache), pos=.hm_cache$route_cache)
  if ("star" %in% cache) rm(list = ls(.hm_cache$star_cache), pos=.hm_cache$star_cache)
  invisible(TRUE)
}

#' Save route and SID/STAR cache to file
#'
#' Filename is \code{"route_star_cache_id_XXX.rda"} where "id" is the id
#' parameter and XXX is made up from the name of the grid (which identifies the
#' map used) and the 'aircraftSet' attribute of the aircraft dataset (which
#' identifies the source). This is because the cache should be for a unique
#' combination of these (and you must have these available, because they were
#' needed to generate the routes).
#'
#'
#' @param id Identifying text, see above. Recommended to use a version number
#'   or date.
#' @param grid Your route grid dataset. The \code{grid@name} will be added to
#'   the filename.
#' @param aircraft Your aircraft dataset. The \code{attr(aircraft,
#'   "aircraftSet")} will be added to the filename.
#' @param path By default \code{"data/"}, where the file will be saved.
#'
#' @seealso For more details see the cache section in the vignette:
#'   \code{vignette("Supersonic_Routes_in_depth", package = "himach")}. or
#'   \href{../doc/Supersonic_Routes_in_depth.html#cache}{Vignette on caching}
#'
#'
#' @return Invisible true
#' @export
#'
#' @examples
#' # not run
#' # hm_save_cache("v2", grid, ac) #save here
#'
#'
hm_save_cache <- function(id, grid, aircraft, path = "data/"){
  filename <- paste0("route_star_cache_",
                     id,
                     "_",
                     grid@name,
                     "_",
                     attr(aircraft, "aircraftSet"),
                     ".rda")
  full_filename <- paste0(path, stringr::str_replace_all(filename, "\\s", "_"))
  save("route_cache", "star_cache",
       envir = .hm_cache,
       file = full_filename)
  invisible(full_filename)
}

#' Load route and SID/STAR cache
#'
#' This silently overwrites any existing values in the cache.
#'
#' @param file Including the path.
#'
#' @seealso For more details see the cache section in the vignette:
#'   \code{vignette("Supersonic_Routes_in_depth", package = "himach")}. or
#'   \href{../doc/Supersonic_Routes_in_depth.html#cache}{Vignette on caching}
#'
#' @return Invisible true
#' @export
#'
#' @examples
#' # not run
#' # hm_load_cache(file="") #load from this file
#'
hm_load_cache <- function(file){
  load(file, envir = .hm_cache)
}
