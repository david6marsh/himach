## Test environments
* local OS X install, R 4.0.2
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
New submission.

(which is unavoidably true)


## Comments

Tried testing on travis-ci and ubuntu, but `himach` needs a very recent version of `sf` that is not available as a binary on travis-ci. Installing `sf` and `units` through travis-ci caused problems unrelated to `himach`, so I've backed out of that rabbit hole for now.

