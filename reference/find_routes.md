# Find best routes between airport-pair & aircraft combinations

`find_routes` combines an aircraft and airport-pair list and finds the
best routes between them, refuelling if necessary

## Usage

``` r
find_routes(ac_ids, ap2_ids, aircraft, airports, ...)
```

## Arguments

- ac_ids:

  A vector of aircraft IDs, as in column 'id' from
  [`make_aircraft`](https://david6marsh.github.io/himach/reference/make_aircraft.md)

- ap2_ids:

  A 2-column matrix or dataframe of airport pair text IDs

- aircraft:

  Specification of the aircraft, see
  [`make_aircraft`](https://david6marsh.github.io/himach/reference/make_aircraft.md)

- airports:

  Airport locations as from
  [`make_airports`](https://david6marsh.github.io/himach/reference/make_airports.md)

- ...:

  Other parameters, passed to
  [`find_route`](https://david6marsh.github.io/himach/reference/find_route.md).

## Value

Dataframe with details of the routes

## Details

This function finds is a wrapper for the single-case function
`find_route`. It takes (text) lists of aircraft and airport codes,
combines them, then finds routes for all of these. A 'route' is made up
of one or two 'legs' (airport to airport without intermediate stop).

For more details see
[`find_route`](https://david6marsh.github.io/himach/reference/find_route.md)

## Examples

``` r
# need to load some of the built-in data
aircraft <- make_aircraft(warn = FALSE)
airports <- make_airports(crs = crs_Pacific)
#> Using default airport data: airportr::airport.
# get test datasets
NZ_buffer30 <- hm_get_test("buffer")
NZ_grid <- hm_get_test("grid")

options("himach.verbosity" = 4) #for heavy reporting
# from Auckland to Christchurch
ap2 <- make_AP2("NZAA","NZCH",airports)
if (FALSE) { # \dontrun{
routes <- find_route(aircraft[4,],
                    ap2,
                    fat_map = NZ_buffer30,
                    route_grid = NZ_grid,
                    ap_loc = airports)
} # }
```
