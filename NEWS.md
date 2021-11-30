# himach 0.2.3

# himach 0.2.2

* Fixed crash when routing over very short hops (say for re-positioning flights).
* (development) Bug fixes: map leakage solved, summarise_routes now works with 3-letter airport codes as well as 4 (though default is still ICAO codes, not IATA codes for airports).
* Removed use of Travis for code coverage.

# himach 0.2.1

* Patch so that tests work ok with `sf` v1.0+.

# himach 0.2.0

* More precise estimate of time penalty for acceleration from subsonic to supersonic cruise (or v.v.). Perhaps 1% difference in timings as a result.
* Faster map plots, since maps are simplified (a little) now, by default.

# himach 0.1.2

* Tweak test for CRAN.
* Fixed issue when using package functions without loading package.

# himach 0.1.1

* Fixing check errors on other platforms
  + Errors involving "NULL: PROJ available?" appear linked [to this 'older GDAL' issue](https://github.com/r-spatial/sf/issues/1419) in `sf`. So implemented a work-around in various examples and vignettes.
  + Use `make_aircraft(warn = FALSE)` in examples & vignettes just to eliminate warnings. Still think this merits a warning in normal use, because the default aircraft are unlikely to be fit for purposes beyond testing.
  + bug fix: Mach 0.84 comparison from `summarise_routes` now uses 0.84 instead of 0.85.

# himach 0.1.0

* First version targeted for CRAN release. 
* Fixes a number of bugs in the avoid-area functionality. 

# himach 0.0.2

* Major overhaul of the approach to smoothing from a Dijkstra-style route to a great-circle route, to handle the 1% of cases where the old approach didn't work well. 
* Further speed improvements.

# himach 0.0.1

* First version using the `s2` capabilities of the `sf` package. This gives more robust plotting, and a number of internal speed improvements.


