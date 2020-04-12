# classes
#
# S4 classes used in 2speed

#a grid and lattice set
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
