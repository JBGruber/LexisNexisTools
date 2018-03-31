## Resubmission
This is a resubmission. In this version I have:

* Omited the redundant part "An R package for" in the title..

* Wrote package names and software names in single quotes (e.g. 'LexisNexis') in title and description. 

* Wrote the title in title case: 

* Consistently and wrote TXT, never txt

* Added '()' behind all function names in description.

* Removed all mentions of old function names. 

* Removed markups in description. 

* Added url to one of LexisNexis websites (files for this package can be retrieved from several urls depending on the country you are trying to reach the archive from and if you are downlaoding files via e.g., a library account).

* Comment from first submission: "Why don't you search for end_keyword and tolower(end_keyword)?"
* It seems the description was not very clear about this point. tolower(end_keyword) would achieve the opposite of the intended behaviour. The keyword should only be found if all caps. The description describes the rare case of false positives when all caps form of the keyword is used inside a newspaper article. I used the code to convert 1.6 million articles and this happened twice so far. I updated the description to be more specific.

* Removed \dontrun{} from examples and added small sample.TXT.




## Test environments
* local Windows 10 Home 64bit, R version 3.4.3
* local Kubuntu 17.10, R version 3.4.4
* macOS Sierra 10.12.6 (on travis-ci), R version 3.4.4
* Ubuntu 14.04.5 (on travis-ci), R version 3.3.3

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

