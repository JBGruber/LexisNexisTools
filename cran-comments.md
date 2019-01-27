## Update Summary
Even though the last version is only a week old, this release is neccessary as changes in `quanteda` broke one of the functions [see](https://github.com/quanteda/quanteda/issues/1572). The release solves this problem and uses `quanteda`'s functions a little more consistently.

## Test environments
* win-builder.r-project.org, R-release, R-oldrelease
* local Windows 10 Home 64bit, R version 3.5.2
* local Kubuntu 18.04, R version 3.5.2
* macOS Sierra 10.12.6 (on travis-ci), R: release
* Ubuntu 14.04.5 (on travis-ci), R: release, R: oldrel, R: devel

## R CMD check results
There were no ERRORs or WARNINGs.
There was one NOTE: "Days since last update: 6" (see above for explanation).
There were two NOTEs on win-builder.r-project.org R-oldrelease, both stating that running examples for lnt_diff() took more than 10s to run (18s on 'i386' and 19s on 'x64').
On the other test environments this was not the case.
