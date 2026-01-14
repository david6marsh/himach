# Find best non-stop route between 2 airports

`find_leg` finds the quickest non-stop route for `ac` between two
airports `ap2`.

## Usage

``` r
find_leg(
  ac,
  ap2,
  route_grid,
  fat_map,
  ap_loc,
  avoid = NA,
  enforce_range = TRUE,
  best_by_time = TRUE,
  grace_km = NA,
  shortcuts = TRUE,
  ad_dist_m = 100 * 1000,
  ad_nearest = 12,
  max_leg_circuity = 1.4,
  ...
)
```

## Arguments

- ac, ap2, route_grid, fat_map, ap_loc, avoid:

  See
  [`find_route`](https://david6marsh.github.io/himach/reference/find_route.md)

- enforce_range:

  If TRUE (default) then leg is constrained to aircraft range, otherwise
  routes of excess range can be found.

- best_by_time:

  If TRUE (default) then the quickest route is found, else the shortest
  distance.

- grace_km:

  Default NA. Otherwise, if great circle distance is within 3pct of
  aircraft range, then add `grace_km`km to the range.

- shortcuts:

  If TRUE (default) then path will be checked for great circle
  shortcuts.

- ad_dist_m:

  The length of arrival/departure links, in m. (Default 100,000=100km)

- ad_nearest:

  The number of arrival/departure links to create (Default 12)

- max_leg_circuity:

  The maximum detour over great circle distance that can be flown to
  find a quick over-sea route. Default 1.4.

- ...:

  Other parameters, passed to
  [`make_route_envelope`](https://david6marsh.github.io/himach/reference/make_route_envelope.md)

## Value

Dataframe with details of the leg

## Details

This function finds the quickest non-stop route between two airports. A
'route' is made up of one or two 'legs' (airport to airport without
intermediate stop).
[`find_route`](https://david6marsh.github.io/himach/reference/find_route.md)
makes one or more calls to `find_leg` as required.

It assumes that the routing grid, `route_grid`, has already been
classified as land or sea using the map `fat_map`. The map is further
used when converting the grid-based route to one of great-circle
segments.

In fact `find_leg` finds up to 4 versions of the path:

1.  A great circle, direct between the airports

2.  A grid path, consisting of segments of the routing grid, plus
    departure and arrival routes from the airports

3.  A simplification of the grid path to great circle segments

4.  `shortcuts` defaults to TRUE. Without this, you see near-raw
    Dijkstra results, which are \_not\_ shortest great circle.

Legs are automatically saved in `route_cache` and retrieved from here if
available rather than re-calculated. See
\`vignette("Supersonic_Routes_in_depth")\` for cache anagement.

## Examples

``` r
# need to load some of the built-in data (not run)
if (FALSE) { # \dontrun{
aircraft <- make_aircraft(warn = FALSE)
airports <- make_airports(crs = crs_Pacific)
# get test datasets
NZ_buffer30 <- hm_get_test("buffer")
NZ_grid <- hm_get_test("grid")

options("himach.verbosity" = 4) #for heavy reporting
# from Auckland to Christchurch
ap2 <- make_AP2("NZAA","NZCH",airports)
routes <- find_leg(aircraft[4,],
                    ap2,
                    fat_map = NZ_buffer30,
                    route_grid = NZ_grid,
                    ap_loc = airports)
} # }
```
