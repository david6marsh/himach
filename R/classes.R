# classes
#
# S4 classes used in 2speed

#' A grid and lattice combination
#'
#' Keeps together a grid of points and a
#' lattice of links between those points.
#'
#' @exportClass GridLat
setClass("GridLat",
         slots = c(
           name = "character",
           points = "data.frame",
           lattice = "data.frame"),
         prototype = list(
           name = NA_character_,
           points = as.data.frame(NA),
           lattice = as.data.frame(NA))
)
