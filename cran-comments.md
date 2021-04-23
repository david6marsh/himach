## Re-submission
Patch v0.1.1 addresses a number of CMD Check errors in v0.1.0, largely on older machines (as of 20/4 on oldrel-macos & patched-solaris). All appear to be linked to a [known issue with `sf`](https://github.com/r-spatial/sf/issues/1419) linked to having older versions of GDAL. Have implemented the work-around suggested there when accessing in-built (test) datasets. This removed those CMD Check errors as far as I could tell, using rhub-check on old releases.

## Test environments

* local Mac OS X install, R 4.0.2 (23 April)
* win-builder (old_release, devel)   (23 April)
* rhub windows (old_release, devel)   (23 April)
* Ubuntu Linux 30.04.1 LTS, R-release, GCC (23 April)
* Oracle Solaris 10, x86, 32 bit, R-release (solaris-x86-patched), through rhub (20 April)

## R CMD check results
I get ERRORs on Solaris, because the package 'lwgeom' is unavailable (and is a dependency for 'sf' which is a dependency for this package 'himach'). So this is a system, not package error, in my view.

On other platforms there were no ERRORs, WARNINGs or NOTEs.


