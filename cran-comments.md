## Update Summary
I decided to submit this update as some tests were failing on (https://cran.r-project.org/web/checks/check_results_LexisNexisTools.html) due to an update to package `diffobj`.

## Test environments
* win-builder.r-project.org, R-release, R-oldrelease
* local Windows 10 Home 64bit, R version 3.5.2
* local Kubuntu 18.04, R version 3.5.2
* macOS Sierra 10.12.6 (on travis-ci), R: release
* Ubuntu 14.04.5 (on travis-ci), R: release, R: oldrel, R: devel

## R CMD check results
There were no ERRORs or WARNINGs.
There were two NOTEs on win-builder.r-project.org R-oldrelease, both stating that running examples for lnt_diff() took more than 10s to run (12.32s on 'i386' and 13.68 on 'x64').
On the other test environments this was not the case.
