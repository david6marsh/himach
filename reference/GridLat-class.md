# A grid and lattice combination

A `GridLat` keeps together a grid of points and a lattice of links
between those points.

It has 3 components:

\* A character name, which isn't used much in anger but might help you
remember what's gone into it. \* A dataframe containing the points of
the lattice (the vertices), which each have an ID, a longitude and
latitude. \* A dataframe containing the edges of the lattice, joining
the points.
