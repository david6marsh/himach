## Re-submission
Re-submission comments:
* Note: New submission
ok
* Note: Found the following (possibly) invalid URLs: link to: ['maturing'] badge. 
 Solution: I have removed the badge. The revised link was helpful in clarifying.
* Check: data for non-ASCII characters, Result: WARNING
   Error loading dataset 'crs_Atlantic':
    Error in .requirePackage(package) : unable to find required package 'sp'
   Error loading dataset 'crs_Pacific':
    Error in .requirePackage(package) : unable to find required package 'sp'
   The dataset(s) may use package(s) not declared in Depends/Imports.
 Solution: `sp` was only in Suggested, moved to Depends.
* Check: for detritus in the temp directory, Result: NOTE
 Found the following files/directories:
   'RtmpC0BgCdroute_star_cache_test_NZ_lat-long_at_30km_test_aircraft.rda'
   'RtmpC8Err6route_star_cache_test_NZ_lat-long_at_30km_test_aircraft.rda'
 Solution: These sound like the output of test_routes:85 which tests saving the cache to a `tempdir`. I have added an `unlink` to remove the temporary directory explicitly. Plus added Debian Linux, R-devel, GCC to test environments.

## Test environments
* local OS X install, R 4.0.2  (Repeated 12 Apr)
* win-builder (release & devel)   (Repeated 12 Apr)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC through rhub  (Repeated 12 Apr)
* Debian Linux, R-devel, GCC, through rhub (Added 12 April)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE both on Windows & Linux:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘David Marsh <david.marsh@eurocontrol.int>’
  
  New submission
That is unavoidably true.


