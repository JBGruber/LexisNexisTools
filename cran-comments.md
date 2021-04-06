## Purpose
A small maintenance update.

## Test environments
* local Kubuntu 20.04, R version 4.0.3
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* GitHub actions (windows-latest, macOS-latest, ubuntu-20.04), r: 'release' and 'devel'
* rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().