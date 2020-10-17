## Purpose
This submission is necessary as some tests fail due to the recent update of dplyr.

## Test environments
* local Kubuntu 18.04, R version 4.0.3
* local Windows 10, R version 4.0.3
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* Ubuntu Xenial 16.04 (on travis-ci), R: release, R: oldrel, R: devel
* rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTEs

Only on rhub: 
"Examples with CPU (user + system) or elapsed time > 5s
lnt_diff 6.28    0.8    7.14
         user system elapsed"

Other test environments do not note this.

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().