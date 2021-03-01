.onLoad <- function(libname, pkgname) {
  # must have sf_use_s2()==TRUE
  if (!sf::sf_use_s2()){
    packageStartupMessage("Mach2 requires that package sf use s2.")
    packageStartupMessage("Setting sf_use_s2(TRUE)")
    sf::sf_use_s2(TRUE)
  }
}


.onUnload = function(libname, pkgname) {
  # nothing here yet
}
