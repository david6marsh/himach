# Make or load airport data

`make_airports` ensures a minimum set of variables describing airports

## Usage

``` r
make_airports(ap = NA, crs = crs_longlat, warn = TRUE)
```

## Arguments

- ap:

  Dataframe containing the minimum fields, or NA (default)

- crs:

  Coordinate reference system for the coded lat-longs. Default 4326.

- warn:

  warn if default set is used (default = TRUE)

## Value

Dataframe with, in addition, a geocoded lat-long.

## Details

This function provides a test set of airports if necessary from
[`airportr::airports`](https://rdrr.io/pkg/airportr/man/airports.html)
and geocodes the lat-long of this or the dataset provide as `ap`.

This minimal set needs to have the following fields:

- `APICAO`: the 4-letter ICAO code for the airport (though there is no
  validity check applied, so 'TEST', or 'ZZZZ' could be used, for
  example)

- `lat, long`: latitude and longitude in decimal degrees

## Examples

``` r
# do minimal version
airports <- make_airports()
#> Using default airport data: airportr::airport.

# on-the-fly example
airports <- data.frame(APICAO = "TEST", lat = 10, long = 10, stringsAsFactors = FALSE)
airports <- make_airports(airports)

if (FALSE) { # \dontrun{
# example for your own data
airports <- utils::read.csv("data/airports.csv", stringsAsFactors = FALSE)
airports <- make_airports(airports)
} # }
```
