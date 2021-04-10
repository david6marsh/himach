# himach

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test coverage](https://codecov.io/gh/david6marsh/himach/branch/main/graph/badge.svg)](https://codecov.io/gh/david6marsh/himach?branch=main)
[![Travis build status](https://travis-ci.com/david6marsh/himach.svg?branch=main)](https://travis-ci.com/david6marsh/himach)
<!-- badges: end -->

The goal of himach ("high Mach") is to support modelling and analysis of the market for supersonic aircraft by generating good routes for aircraft which can fly supersonic over the ocean, but only subsonic over land. 

'Good' usually means the fastest, and the key indicator is time advantage over flying in a subsonic aircraft.

It is not an operational tool. Please don't fly these routes, which do not allow for wind or other atmospheric conditions, and are based on a very simple model of aircraft performance.

## Installation

You can install the current version of `himach` from github with:

``` r
install.packages("devtools")
devtools::install_github("david6marsh/github")
```

For the vignette and testing, this package uses a map of New Zealand map based on Stats NZ's data which are licensed by Stats NZ for re-use under the Creative Commons Attribution 4.0 International licence.

## Example

See the vignettes for a worked example.

