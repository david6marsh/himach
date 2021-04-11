## Test environments
* local OS X install, R 4.0.2
* win-builder (release & devel)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC through rhub

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE both on Windows & Linux:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘David Marsh <david.marsh@eurocontrol.int>’
  
  New submission
That is unavoidably true.

There was 1 NOTE just on Linux:

* checking for future file timestamps ... NOTE
  unable to verify current time
This doesn't sound like an issue with `himach`.

