## Purpose
This is a resubmission. One of the URLs in the file README.md produced a 301 status message in the automatic tests. I corrected the URL and another issue regarding the date in the description.

## Test environments
* local Kubuntu 20.04, R version 4.1.2
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* GitHub actions (windows-latest, macOS-latest, ubuntu-20.04), r: 'release' and 'devel'
* rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTEs

I moved to a different organisation and have added my new contact email to the description.
