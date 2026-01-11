# Changelog

## himach 1.0.1

## himach 1.0.1

- Patch of missing global declaration, previously hidden by use of
  dplyr.
- Fix typo in reference to vignette.

## himach 1.0.0

CRAN release: 2025-07-13

- Upgrade to major release, because *himach* has been mature and stable
  for quite a while.
- Dependency upgrade: replace use of ‘quiet’ option.
- Bug fixes: out-dated link in vignette, dependency on R for pipe.

## himach 0.3.2

CRAN release: 2023-09-20

- New graphics: Route density charts, for routes combined with
  forecasts. See the Advanced Vignette.
- Bug fixes: package dependencies, package documentation.

## himach 0.3.1

CRAN release: 2022-12-05

- Bug fixes: Prettier cutting of background map at dateline.
- Dependency upgrades
- using edition 3 of testthat
- removing deprecated syntax in tidyverse

## himach 0.3.0

CRAN release: 2022-06-09

- New graphics options: in `map_routes` colour lines by a flight
  frequency variable or by number of accelerations to supersonic; now
  can also plot a simple speed/distance/time/altitude profile with
  `profile_routes`.
- Added a section on cache housekeeping to the advanced vignette.
- Bug fixes: Solved map ‘leakage’ at dateline, and `make_AP2` copes with
  vectors of airport pairs correctly now.

## himach 0.2.3

CRAN release: 2021-12-01

- Fixes for solaris running old GDAL: switch to storing crs_Atlantic etc
  as strings.

## himach 0.2.2

CRAN release: 2021-11-21

- Fixed crash when routing over very short hops (say for re-positioning
  flights).
- (development) Bug fixes: map leakage solved, summarise_routes now
  works with 3-letter airport codes as well as 4 (though default is
  still ICAO codes, not IATA codes for airports).
- Removed use of Travis for code coverage.

## himach 0.2.1

CRAN release: 2021-06-17

- Patch so that tests work ok with `sf` v1.0+.

## himach 0.2.0

- More precise estimate of time penalty for acceleration from subsonic
  to supersonic cruise (or v.v.). Perhaps 1% difference in timings as a
  result.
- Faster map plots, since maps are simplified (a little) now, by
  default.

## himach 0.1.2

CRAN release: 2021-04-30

- Tweak test for CRAN.
- Fixed issue when using package functions without loading package.

## himach 0.1.1

CRAN release: 2021-04-23

- Fixing check errors on other platforms
  - Errors involving “NULL: PROJ available?” appear linked [to this
    ‘older GDAL’ issue](https://github.com/r-spatial/sf/issues/1419) in
    `sf`. So implemented a work-around in various examples and
    vignettes.
  - Use `make_aircraft(warn = FALSE)` in examples & vignettes just to
    eliminate warnings. Still think this merits a warning in normal use,
    because the default aircraft are unlikely to be fit for purposes
    beyond testing.
  - bug fix: Mach 0.84 comparison from `summarise_routes` now uses 0.84
    instead of 0.85.

## himach 0.1.0

CRAN release: 2021-04-15

- First version targeted for CRAN release.
- Fixes a number of bugs in the avoid-area functionality.

## himach 0.0.2

- Major overhaul of the approach to smoothing from a Dijkstra-style
  route to a great-circle route, to handle the 1% of cases where the old
  approach didn’t work well.
- Further speed improvements.

## himach 0.0.1

- First version using the `s2` capabilities of the `sf` package. This
  gives more robust plotting, and a number of internal speed
  improvements.
