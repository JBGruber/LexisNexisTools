## Purpose
This submission is necessary as since upcoming changes in quanteda would otherwise break the package (see https://github.com/JBGruber/LexisNexisTools/pull/16).

## Test environments
* local Kubuntu 20.04, R version 4.0.3
* local Windows 10, R version 4.0.3
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* Ubuntu Xenial 16.04 (on travis-ci), R: release, R: oldrel, R: devel
* rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().