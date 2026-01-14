# Save route and SID/STAR cache to file

Filename is `"route_star_cache_id_XXX.rda"` where "id" is the id
parameter and XXX is made up from the name of the grid (which identifies
the map used) and the 'aircraftSet' attribute of the aircraft dataset
(which identifies the source). This is because the cache should be for a
unique combination of these (and you must have these available, because
they were needed to generate the routes).

## Usage

``` r
hm_save_cache(id, grid, aircraft, path = "data/")
```

## Arguments

- id:

  Identifying text, see above. Recommended to use a version number or
  date.

- grid:

  Your route grid dataset. The `grid@name` will be added to the
  filename.

- aircraft:

  Your aircraft dataset. The `attr(aircraft, "aircraftSet")` will be
  added to the filename.

- path:

  By default `"data/"`, where the file will be saved.

## Value

Invisible true

## See also

For more details see the cache section in
\`vignette("Supersonic_Routes_in_depth")\`.

## Examples

``` r
# not run
# hm_save_cache("v2", grid, ac) #save here

```
