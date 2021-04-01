# A few commands I run after updating the package 
## set for spelling package
Sys.setenv(NOT_CRAN = TRUE)

setwd(here::here())

## Update date and version
update_description <- function() {
  desc <- readLines("DESCRIPTION")
  date <- desc[grepl("^Date:", desc)]
  date2 <- gsub("[^[:digit:]-]", "", date)
  desc[grepl("^Date:", desc)] <- gsub(date2, Sys.Date(), desc[grepl("^Date:", desc)])
  vers <- desc[grepl("^Version:", desc)]
  vers2 <- gsub("[^[:digit:].]", "", vers)
  vers3 <- readline(prompt = paste("New Version? Old:", vers2))
  if (vers3 == "") {
    vers3 <- vers2
  }
  desc[grepl("^Version:", desc)] <- gsub(vers2, vers3, desc[grepl("^Version:", desc)])
  writeLines(desc, "DESCRIPTION")
}
update_description()

## Update roxygen and check
roxygen2::roxygenise(clean = TRUE)
devtools::check()

## Check code quality
lintr::lint_package()
goodpractice::gp()

## Check spelling
spelling::spell_check_package()
spelling::update_wordlist()
spelling::spell_check_files("README.Rmd", ignore = readLines("./inst/WORDLIST"), lang = "en-GB")

## Update Citation
update_citation <- function() {
  cit <- readLines("./inst/CITATION")
  version <- grep("note = ", cit)
  year <- grep("year = ", cit)
  
  desc <- readLines("DESCRIPTION")
  vers <- gsub("[^[:digit:].]", "", grep("^Version:", desc, value = TRUE))
  vers <- gsub(".9000$", "", vers)
  cit[version] <- gsub("R package version (.*)", paste0("R package version ", vers, "\""), cit[version])
  
  y <- gsub(".*(\\d{4}).*", "\\1", grep("^Date:", desc, value = TRUE))
  cit[year] <- paste0("         year = ", y, ",")
  
  writeLines(cit, "./inst/CITATION")
}
update_citation()

## create the package in wd
path <- devtools::build(vignettes = TRUE, manual = TRUE)
devtools::check_built(path = path)

## build manual
unlink("LexisNexisTools.pdf")
system("R CMD Rd2pdf ../LexisNexisTools")

## build vignette
knitr::knit("README.Rmd")

## test covr
devtools::test_coverage()


# For release on CRAN
## test on winbuilder
devtools::check_win_devel()
devtools::check_win_oldrelease()
devtools::check_win_release()

## check r_hub
rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

## release
revdepcheck::revdep_check()
devtools::release()
