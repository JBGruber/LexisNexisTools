#' @importFrom utils packageVersion
.onAttach <- function(...) {
  packageStartupMessage("LexisNexisTools Version ", packageVersion("LexisNexisTools"))
}
