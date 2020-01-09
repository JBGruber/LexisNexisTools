## Test environments
* local Kubuntu 18.04, R version 3.6.2
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* R-hub via rhub::check_for_cran()
* Ubuntu Xenial 16.04 (on travis-ci), R: release, R: oldrel, R: devel

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTEs

* The NOTE appears only on R-hub builder where the example for lnt_diff runs for more than 7 seconds. This is not a problem for any of the other test environments.
