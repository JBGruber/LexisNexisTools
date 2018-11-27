## Resubmission
This is a resubmission. Despite local testing, the package did not pass incoming checks on the first submission. In this version I have:

* Changed links previously not in canonical form which were in the README.md. I use https now.
* Removed calling DBI via '::' in one of the tests. Instead I use `RSQLite` now which is listed under Suggests. 
* Fixed ERROR concerning a testthat test (conversion from a package specific format to SQL).
* Removed one of the testthat tests which caused ERROR seemingly due to encoding issues with a show method. I could not replicate this issue on Windows or Linux and the test is not essential. From the log it seems like win-builder shows the output but, maybe due to languague settings, not all characters are displayed correctly. 

## Test environments
* win-builder.r-project.org, R-release
* local Windows 10 Home 64bit, R version 3.5.1
* local Kubuntu 18.04, R version 3.5.1
* macOS Sierra 10.12.6 (on travis-ci), R version 3.5.1
* Ubuntu 14.04.5 (on travis-ci), R version 3.5.1

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 