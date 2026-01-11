# Summarise a set of routes

Reduce a set of routes to a one-line per route summary

## Usage

``` r
summarise_routes(routes, ap_loc, arrdep_h = 0.5)
```

## Arguments

- routes:

  Each segment in each route, as produced by
  [`find_route`](https://david6marsh.github.io/himach/reference/find_route.md)
  or
  [`find_leg`](https://david6marsh.github.io/himach/reference/find_leg.md)

- ap_loc:

  List of airport locations, output of
  [`make_airports`](https://david6marsh.github.io/himach/reference/make_airports.md)

- arrdep_h:

  Total time for the M084 comparator aircraft to arrive & depart in
  hours. Default 0.5.

## Value

Dataframe with summary of the route, sorted in ascending order of
`advantage_h` so that the best route are plotted on top. The fields are:

- `timestamp`: when the leg was originally generated (it may have been
  cached)

- `fullRouteID`: including the refuel stop if any

- `routeID`: origin and destination airport, in
  [`make_AP2`](https://david6marsh.github.io/himach/reference/make_AP2.md)
  order

- `refuel_ap`: code for the refuelling airport, or NA

- `acID, acType`: aircraft identifiers taken from the aircraft set

- `M084_h`: flight time for a Mach 0.84 comparator aircraft (including
  `2*arrdep_h`)

- `gcdist_km`: great circle distance between the origin and destination
  airports

- `sea_time_frac`: Fraction of `time_h` time spent over sea, hence at
  supersonic speed, or accelerating to, or decelerating from supersonic
  speed

- `sea_dist_frac`: as sea_time_frac, but fraction of `dist_km`

- `dist_km`: total length of the route, in km

- `time_h`: total time, in hours

- `n_phases`: number of distinct phases: arr/dep, transition, land, sea,
  refuel.

- `advantage_h`: `M084_h - time_h`

- `circuity`: the route distance extension (1 = perfect)
  `dist_km / gcdist_km`

- `best`: for each `routeID`, the `fullrouteID` with maximum
  `advantage_h`

## Details

This function takes the output of
[`find_route`](https://david6marsh.github.io/himach/reference/find_route.md)
and summarises to one line per (full) route.

With refuelling, there can be multiple 'full routes' for each 'route'.
The `best` column indicates the best route for each `routeID`.

The results are rounded to a reasonable number of significant figures.
After all this is just an approximate model. The `arrdep_h` has been
checked against actual and is reasonable (observed range roughly
0.3-0.5).

## Examples

``` r
# here we use a built-in set of routes
# see vignette for more details of how to obtain it
airports <- make_airports(crs = crs_Pacific)
#> Using default airport data: airportr::airport.
NZ_routes <- hm_get_test("route")
sumy <- summarise_routes(NZ_routes, airports)
```
