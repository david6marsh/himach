# Make airport-pair dataset

`make_AP2` creates an airport-pair set from two sets of airports

## Usage

``` r
make_AP2(adep, ades, ap = make_airports())
```

## Arguments

- adep, ades:

  Identical-length lists of airport codes

- ap:

  List of locations of airports, defaults to the output of
  [`make_airports`](https://david6marsh.github.io/himach/reference/make_airports.md).

## Value

Dataframe with additional variables as described above.

## Details

This function takes two lists of airports (of the same length),
specified as 4-letter codes and combines them, adding the fields:

- `from_long, from_lat, to_long, to_lat`: the airport lat-longs with
  adep first

- `AP2`: a name for the route in a specific order

- `gcdist_km`: the great circle distance in km

In `AP2` European airports (crudely, from starting letter = 'E' or 'L')
are listed first, otherwise in alphabetical order. If unidirectional is
TRUE, then "\>" is the separator, otherwise "\<\>". (Unidirectional not
currently supported)

For more details see the [introductory
vignette](https://david6marsh.github.io/himach/doc/Supersonic_Routes.md).

## Examples

``` r
airports <- make_airports() #get a default set of lat-longs
#> Using default airport data: airportr::airport.
ap2 <- make_AP2("NZAA","NZCH", airports)
```
