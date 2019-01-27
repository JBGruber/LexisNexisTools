# A few commands I run after updating the package 
## set for spelling package
Sys.setenv(NOT_CRAN = TRUE)

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

## Update Citation
update_citation <- function() {
  cit <- readLines("./inst/CITATION")
  note <- grep("note =", cit)
  desc <- readLines("DESCRIPTION")
  date <- desc[grepl("^Date:", desc)]
  date2 <- gsub("[^[:digit:]-]", "", date)
  desc[grepl("^Date:", desc)] <- gsub(date2, Sys.Date(), desc[grepl("^Date:", desc)])
  vers <- desc[grepl("^Version:", desc)]
  vers2 <- gsub("[^[:digit:].]", "", vers)
  vers3 <- gsub("\\d+{3}", "", vers2)
  vers3 <- gsub("[[:punct:]]$", "", vers3)
  cit[note] <- gsub("\\d+.\\d+.\\d+", vers3, cit[note]) 
  writeLines(cit, "./inst/CITATION")
}
update_citation()

## create the package in wd
path <- devtools::build(vignettes = TRUE, manual = TRUE)
devtools::check_built(path = path)

## build manual
unlink("../LexisNexisTools.pdf")
system("R CMD Rd2pdf ../LexisNexisTools")

## build vignette
knitr::knit("README.Rmd")

# For release on CRAN
## test on winbuilder
devtools::check_win_devel()
devtools::check_win_oldrelease()
devtools::check_win_release()

## release
devtools::release()
