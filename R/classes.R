# classes
#
# S4 classes used in 2speed

#' A grid and lattice combination
#'
#' @description
#'
#' A \code{GridLat} keeps together a grid of points and a
#' lattice of links between those points.
#'
#' It has 3 components:
#'
#' * A character name, which isn't used much in anger but might help you remember what's gone into it.
#' * A dataframe containing the points of the lattice (the vertices), which each have an ID, a longitude and latitude.
#' * A dataframe containing the edges of the lattice, joining the points.
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
