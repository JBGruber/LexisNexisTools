#' Convert LNToutput to other formats
#'
#' Takes output from \link{lnt_read} and converts it to other formats.
#'
#' @param x An object of class LNToutput
#' @param to Which format to convert into.
#' @param what Either "Articles" or "Paragraph" to use articles or paragraphs as
#'   text in the output object.
#'
#' @export
#'
#' @examples
#' LNToutput <- lnt_read(lnt_sample())
#' docs <- lnt_convert(LNToutput)
lnt_convert <- function(x, to = "rDNA", what = "Articles") {
  if (to == "rDNA") {
    return(lnt2rDNA(x, what = what))
  }
}

#' @rdname lnt_convert
#' @export
lnt2rDNA <- function(x, what) {
  if (what == "Articles") {
    text <- x@articles$Article
  } else if (what == "Paragraph") {
    text <- x@paragraphs$Paragraph
  }
  dta <- data.frame(id = x@meta$ID, 
                    title = x@meta$Headline, 
                    text = text, 
                    coder = 1, 
                    author = x@meta$Author, 
                    source = x@meta$Newspaper, 
                    section = x@meta$Section, 
                    notes = "", 
                    type = "newspaper", 
                    date = x@meta$Date,
                    stringsAsFactors = FALSE)
  if (any(grepl("Date", class(dta$date)))) {
    dta$date <- as.POSIXct.Date(dta$date)
  }
  if (any(is.na(dta$date), !grepl("POSIXct", class(dta$date)))) {
    warning(paste0("One or more (or all) dates could not be converted to POSIXct.",
            "Na entries in 'date' were filled with the system's time and date instead."))
    dta$date <- tryCatch(as.POSIXct(dta$date),
                         error = function(e) NA)
    dta$date[is.na(dta$date)] <- Sys.time()
    if (class(dta$date) == "numeric") {
      dta$date <- as.POSIXct.numeric(dta$date, origin = "1970-01-01")
    }
  }
  return(dta)
}
