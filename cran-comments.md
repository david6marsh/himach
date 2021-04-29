## Re-submission
Patch v0.1.2 addresses two CMD Check errors in v0.1.1. It should stop the Macos error, but not the solaris error, which is due to a missing, required package 'lwgeom'.

One other code change addresses an issue when using the functions without loading the package.

## Test environments

* local Mac OS X install, R 4.0.2 (29 April)
* win-builder (devel)   (29 April)
* WIndows R-oldrel (rhub - 29 April)
* Ubuntu Linux 30.04.1 LTS, R-release, GCC (29 April)
* Oracle Solaris 10, x86, 32 bit, R-release (solaris-x86-patched), through rhub (29 April)

## R CMD check results
I get ERRORs on Solaris, because the package 'lwgeom' is unavailable (and is a dependency for 'sf' which is a dependency for this package 'himach'). So this is a system, not package error, in my view.

On other platforms there were no ERRORs, WARNINGs or NOTEs.


