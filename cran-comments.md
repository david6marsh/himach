## Re-submission
Patch v0.1.2 addresses CMD Check errors in v0.1.1 on two platforms. As far as I can test, I've found substantive tests which also work on solaris, finally.

One other code change addresses an issue when using the functions without loading the package.

## Test environments

* local Mac OS X install, R 4.0.2 (29 April)
* win-builder (devel)   (29 April)
* Windows R-oldrel (rhub - 29 April)
* Ubuntu Linux 30.04.1 LTS, R-release, GCC (29 April)
* Oracle Solaris 10, x86, 32 bit, R-release (solaris-x86-patched), through rhub (29 April)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Reverse dependencies

None.


