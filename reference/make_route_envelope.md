# Make range-constrained envelope between 2 airports

`make_route_envelope` finds the range envelope for a given route

## Usage

``` r
make_route_envelope(ac, ap2, envelope_points = 200, fuzz = 0.005)
```

## Arguments

- ac, ap2:

  See
  [`find_route`](https://david6marsh.github.io/himach/reference/find_route.md)

- envelope_points:

  How many points are used to define the ellipse? Default 200.

- fuzz:

  Add a little margin to the range, to allow the longest range to be
  flown, rather than be cut off at the boundary. (Default 0.005)

## Value

`sf POLYGON` with ad hoc coordinate reference system.

## Details

The 'route envelope' is the region within which a route from A to B must
remain. This is an ellipse.

It differs from the pure 'range envelope' which is the points which an
aircraft can reach from a given airport.

## Examples

``` r
# Need aircraft and airport datasets
ac <- make_aircraft(warn = FALSE)
ap <- make_airports()
#> Using default airport data: airportr::airport.
z <- make_route_envelope(ac[1,], make_AP2("EGLL","KJFK",ap))
```
