#' Convert Strings to dates
#'
#' Converts dates from string formats common in LexisNexis to date
#'
#' @param x A character object to be converted.
#' @param format Either "auto" to guess the format based on common order of day,
#'   month and year or provide (see \link[stringi]{stri_datetime_format} for
#'   format options).
#' @param locale A ISO 639-1 locale code (see
#'   \url{https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes}).
#'
#' @return This function returns an object of class \link{date}.
#' @export
#'
#' @examples
#' LNToutput <- lnt_read(lnt_sample(), convert_date = FALSE)
#' d <- lnt_asDate(LNToutput@meta$Date)
#' d
#' @importFrom stringi stri_replace_all_fixed stri_replace_all_regex
#'   stri_datetime_parse stri_opts_fixed stri_datetime_symbols
#'   stri_datetime_format
#' @importFrom utils head
lnt_asDate <- function(x,
                       format = "auto",
                       locale = "auto") {
  formats <- c(English = "MMMM d,yyyy",
               German = "d MMMM yyyy",
               Spanish = "d MMMM yyyy",
               Dutch = "d MMMM yyyy",
               French = "d MMMM yyyy",
               Portuguese = "d MMMM yyyy",
               Russian = "d MMMM yyyy")
  locales <- c(English = "en",
               German = "de",
               Spanish = "es",
               Dutch = "nl",
               French = "fr",
               Portuguese = "pt",
               Russian = "ru")

  for (loc in locales) {
    x <- stri_replace_all_fixed(str = x,
                                pattern = c(stri_datetime_symbols(locale = loc)$Weekday,
                                            "PM", "AM"),
                                replacement = "",
                                vectorize_all = FALSE,
                                opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
  }
  x <- stri_replace_all_regex(str = x,
                              pattern = c("[A-Z]{3}$",
                                          "((?:(?:[0-1][0-9])|(?:[2][0-3])|(?:[0-9])):(?:[0-5][0-9])(?::[0-5][0-9])?(?:\\s?(?:am|AM|pm|PM))?)"),
                              replacement = "",
                              vectorize_all = FALSE)

  if (any(format == "auto",
          locale == "auto")) {
    correct <- mapply(function(format, locale) {
      out <- stringi::stri_datetime_parse(str = x,
                                          format = format,
                                          locale = locale)
      out <- 1 - sum(is.na(out)) / length(x)
      out * 100
    }, formats, locales)
    most <- head(sort(correct[correct > 0.01], decreasing = TRUE), n = 3)
    if (most[1] < 100) {
      if (length(most) > 1) {
        if (length(most) == 2) {
          input <- readline(prompt = paste0("Most likely languages for dates: ", names(most)[1],
                                            " (", most[1], "%", "), ", names(most)[2],
                                            " (", most[2], "%", "). Select language",
                                            " for date conversion (1/2) or 'abort':"))
        } else if (length(most) == 3) {
          input <- readline(prompt = paste0("Most likely languages for dates: ", names(most)[1],
                                            " (", most[1], "%", "), ", names(most)[2],
                                            " (", most[2], "%", "), ", names(most)[3],
                                            " (", most[3], "%", "). Select language",
                                            " for date conversion (1/2/3) or 'abort':"))
        }
        if (grepl("1|2|3", input)) {
          input <- as.numeric(input)
        } else {
          input <- FALSE
        }
      } else {
        input <- readline(prompt = paste0("Most likely language for dates: ", names(most),
                                          " (", most, "%", "). Proceed date ",
                                          "conversion with this language? (y/n)"))
        input <- grepl("y|yes", input, ignore.case = TRUE)
      }
    } else {
      input <- 1
    }
    format <- formats[names(formats) == names(most)[input]]
    locale <- locales[names(locales) == names(most)[input]]
  }
  if (!format[1] %in% formats) {
    message("A non-standard format was provided. Conversion is tried but might fail.\n")
  }
  x <- stringi::stri_datetime_parse(str = x,
                                    format = format,
                                    locale = locale)
  x <- as.Date(x)
  return(x)
}
