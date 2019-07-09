## Re-submission
In the original submission, R Under development (unstable) (2019-07-07 r76800) reported a NOTE:

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ‘sample.TXT’
  
The file ‘sample.TXT’ was produced by multiple tests (but also removed after completion) and the example for lnt_sample(). This was never a problem before and still isn't on other instances. I changed the respective code and tested if checks complete without a NOTE via rhub::check_for_cran().

Other changes: I added a test for lnt_convert() to a quanteda corpus for quanteda version 1.5.0, which was released since the original submission.

## Test environments
* local Kubuntu 18.04, R version 3.6.1
* win-builder.r-project.org, R-release, R-oldrelease, R-devel
* R-hub via rhub::check_for_cran()
* macOS Sierra 10.12.6 (on travis-ci), R: release
* Ubuntu 14.04.5 (on travis-ci), R: release, R: oldrel, R: devel

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.
