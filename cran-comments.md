## Resubmission
In the original submission, R Under development (unstable) (2019-07-07 r76800) reported a NOTE:

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ‘sample.TXT’
  
The file ‘sample.TXT’ is produced by /tests/testthat/test-lnt_rename.R and should be removed after the test via command testthat::teardown() (which works fine under current versions of R). Since this works locally and on other versions, I assume the behaviour is caused by upcoming changes in R. I'm skipping the test via testthat::skip_on_cran() for now. I checked on win-builder and can cofirm that this resolves the NOTE.

Other changes: I added a test for lnt_convert() to a quanteda corpus for quanteda version 1.5.0, which was released since the original submission.

## Test environments
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* local Windows 10 Home 64bit, R version 3.6.0
* local Kubuntu 18.04, R version 3.6.1
* macOS Sierra 10.12.6 (on travis-ci), R: release
* Ubuntu 14.04.5 (on travis-ci), R: release, R: oldrel, R: devel

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.
