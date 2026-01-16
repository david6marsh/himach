# Make lat-long grid for route finding

`make_route_grid` creates, and optionally classifies, a lat-long route
grid

## Usage

``` r
make_route_grid(
  fat_map,
  name,
  target_km = 800,
  lat_min = -60,
  lat_max = 86,
  long_min = -180,
  long_max = 179.95,
  classify = FALSE
)
```

## Arguments

- fat_map:

  MULTIPOLYGON map defining land regions

- name:

  String assigned to the name slot of the result

- target_km:

  Target length. Default 800km only to avoid accidentally starting heavy
  compute. 30-50km would be more useful.

- lat_min, lat_max:

  Latitude extent of grid

- long_min, long_max:

  Longitude extend of grid. Two allow small grids crossing the 180
  boundary, the function accepts values outside \[-180,180), then rounds
  to within this range.

- classify:

  Whether to classify each link. Defaults to FALSE only to avoid
  accidentally starting heavy compute.

## Value

`gridLat` object containing points and lattice.

## Details

This function creates a
[GridLat](https://david6marsh.github.io/himach/reference/GridLat-class.md)
object that contains a set of point on a lat long grid (ie all the
points are on lines of latitude). It also joins these points into a
lattice. Optionally, but required later, it classifies each link as
land, sea, or transition, with reference to a given map (typically
including a coastal buffer).

The definitions are

- land: both ends of the link are on land

- sea: both ends are on sea, and the link does not intersect the land

- transition: otherwise

The length of the links will be around `target_km` or 50pct longer for
the diagonal links.

For more details see the help vignette:
`vignette("Supersonic Routing", package = "himach")`

## Examples

``` r
NZ_buffer <- hm_get_test("buffer")
system.time(
  p_grid <- make_route_grid(NZ_buffer,"NZ lat-long at 300km",
                           target_km = 300, classify = TRUE,
                           lat_min = -49, lat_max = -32,
                           long_min = 162, long_max = 182)
)
#> Made the grid:0
#> Making the basic lattice:
#> 
#> Adding geo & distance to the lattice...
#> 
#> Added geo & distance to the lattice:0.1
#> Classifying points in the lattice as land.
#> Classified as land:0.1
#> Classifying lines in the lattice as land.
#> Classified as land:0.1
#> Calculated all phases:0.1
#> Converting points and lattice to data table.
#>    user  system elapsed 
#>   0.127   0.004   0.130 
```
