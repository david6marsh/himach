.hm_cache <- new.env(FALSE, parent=globalenv())

.onLoad <- function(libname, pkgname) {

  assign("route_cache", new.env(parent=parent.env(environment())), .hm_cache)
  attr(.hm_cache$route_cache,"map") <- "" # no map name yet

  assign("star_cache", new.env(parent=parent.env(environment())), .hm_cache)
  attr(.hm_cache$star_cache,"map") <- "" # no map name yet

   requireNamespace("s2", quietly = TRUE)
  sf::sf_use_s2(TRUE) # is this rude - changing a state silently?

}


.onUnload = function(libname, pkgname) {
  # nothing here yet
}
