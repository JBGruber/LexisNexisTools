#' Provides a small sample TXT file
#'
#' Copies a small TXT sample file to the current working directory and returns
#' the location of this newly created file. The content of the file is made up
#' or copied from Wikipedia since real articles from LexisNexis fall under
#' copyright laws and can not be shared.
#'
#' A small sample database to test the functions of LexisNexisTools
#'
#' @param overwrite Should sample.TXT be overwritten if found in the current
#'   working directory?
#' @param verbose Display warning message if file exists in current wd.
#'
#' @examples
#' lnt_sample()
#' @author Johannes Gruber
#' @export
lnt_sample <- function(overwrite = FALSE,
                       verbose = TRUE) {
  if (all(file.exists(paste0(getwd(), "/sample.TXT")), !overwrite)) {
    if (verbose){
      warning(
        "Sample file exists in wd. Use overwrite = TRUE to create fresh sample file."
      )
    }
  } else {
    file.copy(from = system.file("extdata", "sample.TXT", package = "LexisNexisTools"),
              to = paste0(getwd(), "/sample.TXT"),
              overwrite = TRUE)
  }
  return(paste0(getwd(), "/sample.TXT"))
}
