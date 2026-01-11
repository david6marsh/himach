# Find best route between 2 airports

`find_route` finds the quickest route between two airports, refuelling
if necessary

## Usage

``` r
find_route(
  ac,
  ap2,
  fat_map,
  avoid = NA,
  route_grid,
  cf_subsonic = NA,
  refuel = NA,
  refuel_h = 1,
  refuel_only_if = TRUE,
  refuel_topN = 1,
  max_circuity = 2,
  ap_loc,
  margin_km = 200,
  ...
)
```

## Arguments

- ac:

  One aircraft, as from
  [`make_aircraft`](https://david6marsh.github.io/himach/reference/make_aircraft.md)

- ap2:

  One airport pair, as from
  [`make_AP2`](https://david6marsh.github.io/himach/reference/make_AP2.md)

- fat_map:

  `sf::MULTIPOLYGON` map of land, including buffer

- avoid:

  `sf::MULTIPOLYGON` map of areas not to fly over

- route_grid:

  `GridLat` routing grid as from
  [`make_route_grid`](https://david6marsh.github.io/himach/reference/make_route_grid.md)

- cf_subsonic:

  Further aircraft to use as comparator, default NA. (use is not
  recommended)

- refuel:

  Airports available for refuelling, dataframe with `APICAO, long, lat`

- refuel_h:

  Duration of refuelling stop, in hours

- refuel_only_if:

  If TRUE (default) only test refuel options if necessary because the
  great circle distance is too far for the aircraft range

- refuel_topN:

  Return the best N (default 1) refuelling options

- max_circuity:

  Threshold for excluding refuelling stops (default 2.0)

- ap_loc:

  Airport locations as from
  [`make_airports`](https://david6marsh.github.io/himach/reference/make_airports.md)

- margin_km:

  Great circle distance between airports must be less than aircraft
  range minus this operating margin (default 200km), to give a margin
  for arrival and departure.

- ...:

  Other parameters, passed to
  [`find_leg`](https://david6marsh.github.io/himach/reference/find_leg.md)
  and thence to to
  [`make_route_envelope`](https://david6marsh.github.io/himach/reference/make_route_envelope.md).

## Value

Dataframe with details of the route

## Details

This function finds the quickest route between two airports. A 'route'
is made up of one or two 'legs' (airport to airport without intermediate
stop). `find_route` makes one or more calls to `find_leg` as required.

It assumes that the routing grid, `route_grid`, has already been
classified as land or sea using the map `fat_map`. The map is further
used when converting the grid-based route to one of great circles
segments.

## Refuelling

If either necessary, because the great circle distance is greater than
the aircraft range, or because `refuel_only_if` is FALSE, `find_route`
searches through a list of refuelling airports and chooses the quickest
one (or `refuel_topN`).

Circuitous refuelling is avoided, tested against total distance \<
`max_circuity` \* great circle distance. This is separate to the limits
placed on circuity of individual legs in
[`find_leg`](https://david6marsh.github.io/himach/reference/find_leg.md).

If no refuel option is found, a message is displayed. The route with
\`NA\` for \`time_h\` is returned.

Each refuelling stop costs `refuel_h` in addition to the time to descend
to the airport and then to climb out again.

## Examples

``` r
# need to load some of the built-in data
aircraft <- make_aircraft(warn = FALSE)
# get test datasets
NZ_buffer30 <- hm_get_test("buffer")
NZ_grid <- hm_get_test("grid")
airports <- make_airports(crs = sf::st_crs(NZ_buffer30))
#> Using default airport data: airportr::airport.

options("himach.verbosity" = 4) #for heavy reporting
# from Auckland to Christchurch
ap2 <- make_AP2("NZAA","NZCH",airports)
# on some CRAN machines even this takes too long, so not run
if (FALSE) { # \dontrun{
routes <- find_route(aircraft[4,],
                    ap2,
                    fat_map = NZ_buffer30,
                    route_grid = NZ_grid,
                    ap_loc = airports)
} # }
```
