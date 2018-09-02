# LexisNexisTools 0.2.0

* Rewrote lnt_read() to be more stable (and faster) which rendered lnt_checkFiles() unnecessary (now deprecated).
* Added lnt_convert() to transform objects created by lnt_read() to formats used in popular text-as-data analysis packages.
* Enhanced lnt_similarity() which tended to crash when comparing longer texts due to memory limitations.
* Added lnt_diff() to display results from lnt_similarity() in a diff-like viewer.
* Added lnt_lookup() which can be used to check if the nexis keyword search worked properly or to apply simple dictionaries to subset the data.
* Added several methods to work with the S4 class `LNToutput`.
* Added vignette with basic usage.
* Enhanced documentation of of all functions.
* Started using testthat tests
