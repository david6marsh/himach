.onLoad <- function(libname, pkgname) {
  # must have sf_use_s2()==TRUE
  # if (!sf::sf_use_s2()){
  #   packageStartupMessage("Mach2 requires that package sf use s2.")
  #   packageStartupMessage("Setting sf_use_s2(TRUE)")
  #   sf::sf_use_s2(TRUE)
  # }

  requireNamespace("s2", quietly = TRUE)
  sf::sf_use_s2(TRUE) # is this rude - changing a state silently?

  assign("rte_cache", new.env(parent=parent.env(environment())), parent.env(environment()))
  attr(rte_cache,"map") <- "" # no map name yet
  assign("star_cache", new.env(parent=parent.env(environment())), parent.env(environment()))
  attr(star_cache,"map") <- "" # no map name yet
}


.onUnload = function(libname, pkgname) {
  # nothing here yet
}
