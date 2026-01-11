# Clean the route and SID-STAR cache.

Empties the cache.

## Usage

``` r
hm_clean_cache(cache = c("route", "star"))
```

## Arguments

- cache:

  Which caches to clear. Default is both `c("route", "star")`.

## Value

TRUE silently

## See also

For more details see the cache section in the vignette:
[`vignette("Supersonic_Routes_in_depth", package = "himach")`](https://david6marsh.github.io/himach/articles/Supersonic_Routes_in_depth.md).
or [Vignette on
caching](https://david6marsh.github.io/himach/doc/Supersonic_Routes_in_depth.html#cache)

## Examples

``` r
hm_clean_cache("route")

hm_clean_cache()
```
