# himach

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/himach)](https://CRAN.R-project.org/package=himach)
[![Codecov test coverage](https://codecov.io/gh/david6marsh/himach/branch/main/graph/badge.svg)](https://codecov.io/gh/david6marsh/himach?branch=main)
<!-- badges: end -->

The goal of `himach` ("high Mach") is to support modelling and analysis of the market for supersonic aircraft by generating good routes for aircraft which can fly supersonic over the ocean, but only subsonic over land. 

In this version of `himach`, 'good' usually means the fastest, so the key indicator is time advantage over flying in a subsonic aircraft. 

![Three example routes, including a refuel stop in Anchorage. (Original map: www.naturalearthdata.com)](vignettes/three_routes.png)

It is not an operational tool. Please don't fly these routes, which do not allow for wind or other atmospheric conditions, and are based on a very simple model of aircraft performance.

## Installation

You can install the current version of `himach` from github with:

``` r
install.packages("devtools")
devtools::install_github("david6marsh/github")
```

For the vignette and testing, this package uses a map of New Zealand map based on Stats NZ's data which are licensed by Stats NZ for re-use under the Creative Commons Attribution 4.0 International licence. Map shown above uses data from www.naturalearthdata.com, from through the `rnaturalearthdata`, and `rnaturalearthhires` packages. 

## Example

See the vignettes for a worked example.

You need maps and airport data, both available from other R packages, as the vignette shows. This package provides a short list of aircraft for illustration purposes only, the aircraft data were taken from Wikipedia and most-likely out of date. Add your own aircraft performance data: the vignette shows how.

