## Re-submission
Re-submission comments:

* Note: New submission
   + ok
* Flavor: r-devel-linux-x86_64-debian-gcc
Check: for detritus in the temp directory, Result: NOTE
 Found the following files/directories:
   'RtmpIryMewroute_star_cache_test_NZ_lat-long_at_30km_test_aircraft.rda'
   'Rtmppsd4Throute_star_cache_test_NZ_lat-long_at_30km_test_aircraft.rda'
   + Solution: These are cache data saved to the session temp directory. I have added an `unlink` to remove the temporary files explicitly. Plus added Debian Linux, R-devel, GCC to test environments (though yesterday that didn't produce this note).

## Test environments

* local OS X install, R 4.0.2  (Repeated 13 Apr)
* win-builder (release & devel)   (Repeated 13 Apr)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC through rhub  (Repeated 12 Apr)
* Debian Linux, R-devel, GCC, through rhub (Repeated 13 April)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE both on Windows & Linux:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘David Marsh <david.marsh@eurocontrol.int>’
  
  New submission
That is unavoidably true.


