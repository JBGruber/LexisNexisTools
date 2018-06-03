

#' make S4 object
#' @noRd
#' @importFrom methods new
setClass("LNToutput",
         representation(meta = "data.frame",
                        articles = "data.frame",
                        paragraphs = "data.frame"))


setMethod("show",
          signature = "LNToutput",
          definition = function(object) {
            cat("Object of class 'LNToutput':\n")
            cat(nrow(object@meta), "Articles\n")
            cat(nrow(object@paragraphs), "Paragraphs\n")


            meta <- head(object@meta, n = 6)
            articles <- head(object@articles, n = 6)
            paragraphs <- head(object@paragraphs, n = 6)
            trim <- function(object, n, e = "...") {
              ifelse(nchar(object) > n,
                     paste0(gsub("\\s+$", "",
                                 strtrim(object, width = n)),
                            e),
                     object)
            }
            for (cols in colnames(meta)) {
              meta[, cols] <- trim(meta[, cols], 8)
            }
            for (cols in colnames(articles)) {
              articles[, cols] <- trim(articles[, cols], 8)
            }
            for (cols in colnames(paragraphs)) {
              paragraphs[, cols] <- trim(paragraphs[, cols], 8)
            }
            cat("\n\nMeta (6 of ", nrow(object@meta), "):\n", sep = "")
            print(meta)
            cat("\n\nArticles (6 of ", nrow(object@articles), "):\n", sep = "")
            print(articles)
            cat("\n\nParagraphs (6 of ", nrow(object@paragraphs), "):\n",
                sep = "")
            print(paragraphs)
          }
)


#' Read in a LexisNexis TXT file
#'
#' Read a LexisNexis TXT file and convert it to a data frame.
#' @param x Name or names of LexisNexis TXT file to be converted.
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8
#'   (the LexisNexis standard value).
#' @param extract_paragraphs A logical flag indicating if the returned object
#'   will include a third data frame with paragraphs.
#' @param convert_date A logical flag indicating if it should be tried to
#'   convert the date of each article into Date format. Fails for non standard
#'   dates provided by LexisNexis so it might be safer to convert date
#'   afterwards.
#' @param start_keyword Is used to indicate the beginning of an article. All
#'   articles should have the same number of Beginnings, ends and lengths (which
#'   indicate the the last line of meta-data). Use regex expression such as
#'   "\\d+ of \\d+ DOCUMENTS$" (which would catch e.g., the format "2 of 100
#'   DOCUMENTS") or "auto" to try all common keywords. Keyword search is case
#'   sensitive.
#' @param end_keyword Is used to indicate the end of an article. Works the same
#'   way as start_keyword. A common regex would be "^LANGUAGE: " which catches
#'   language in all caps at the beginning of the line (usually the last line of
#'   an article).
#' @param length_keyword Is used to indicate the end of the meta-data. Works the same
#'   way as start_keyword and end_keyword. A common regex would be "^LENGTH: " which catches
#'   length in all caps at the beginning of the line (usually the last line of
#'   the metadata).
#' @param exclude_lines Lines in which these keywords are found are excluded.
#' @param recursive A logical flag indicating whether subdirectories are
#'   searched for more txt files.
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @param ... Additional arguments passed on to \link{lnt_asDate}.
#' @return A LNToutput S4 object consisting of 3 data.frames for meta-data,
#'   articles and paragraphs.
#' @details The function can produce a LNToutput S4 object with two data.frame:
#'   meta, containing all meta information such as date, author and headline and
#'   articles, containing just the article ID and the text of the articles. When
#'   extract_paragraphs is set to TRUE, the output contains a third data.frame,
#'   similar to articles but with articles split into paragraphs.
#'
#'   Note: All files need to have same number of Beginnings, ends and lengths
#'   (which indicate the the last line of meta-data). If this is true can be
#'   tested with \link{lnt_checkFiles}. In some cases it makes sense to change
#'   the keywords for these three important indicators e.g. to "^LANGUAGE:
#'   ENGLISH" to narrow down the search for the ends of an article.
#' @author Johannes B. Gruber
#' @export
#' @examples
#' LNToutput <- lnt_read(lnt_sample())
#' meta.df <- LNToutput@meta
#' articles.df <- LNToutput@articles
#' paragraphs.df <- LNToutput@paragraphs
#' @importFrom stringi stri_read_lines stri_extract_last_regex stri_join stri_isempty stri_split_fixed stri_replace_all_regex
#' @importFrom utils tail
lnt_read <- function(x,
                     encoding = "UTF-8",
                     extract_paragraphs = TRUE,
                     convert_date = TRUE,
                     start_keyword = "auto",
                     end_keyword = "auto",
                     length_keyword = "^LENGTH: |^L\u00c4NGE: |^LONGUEUR: ",
                     exclude_lines = "^LOAD-DATE: |^UPDATE: |^GRAFIK: |^GRAPHIC: |^DATELINE: ",
                     recursive = FALSE,
                     verbose = TRUE,
                     ...){
  if (missing(x)) {
    if (readline(prompt = "No path was given. Should files in working direcotry be read? [y/n]")
        %in% c("y", "yes", "Y", "Yes")) {
      x <- paste0(getwd(), "/")
    } else {
      stop("Aborted by user")
    }
  }
  if (all(grepl(".txt$", x, ignore.case = TRUE))) {
    files <- x
  } else if (any(grepl(".txt$", x, ignore.case = TRUE))) {
    message("Not all provided files were TXT files. Other formats are ignored.")
    files <- grep(".txt$", x, ignore.case = TRUE, value = TRUE)
  } else if (any(grepl("\\\\|/", x))) {
    if (length(x) > 1) {
      files <- unlist(sapply(x, function(f) {
        list.files(path = f,
                   pattern = ".txt$",
                   ignore.case = TRUE,
                   full.names = TRUE,
                   recursive = recursive)
      }, USE.NAMES = FALSE))
    } else {
      files <- list.files(path = x,
                          pattern = ".txt$",
                          ignore.case = TRUE,
                          full.names = TRUE,
                          recursive = recursive)
    }
  } else {
    stop("Provide either file name(s) ending on '.txt' or folder name(s) to x or leave black to search wd.")
  }
  if (start_keyword == "auto") {
    start_keyword <- "\\d+ of \\d+ DOCUMENTS$| Dokument \\d+ von \\d+$| Document \\d+ de \\d+$"
  }
  if (end_keyword == "auto") {
    end_keyword <- "^LANGUAGE: |^SPRACHE: |^LANGUE: "
  }
  if (length_keyword == "auto") {
    length_keyword <- "^LENGTH: |^L\u00c4NGE:  |^LONGUEUR: "
  }

  # Track the time
  if (verbose) start.time <- Sys.time(); cat("Creating LNToutput from a connection input...\n")

  ### read in file
  if (length(files) > 1){
    articles.v <- unlist(lapply(files, function(f) {
      out <- stringi::stri_read_lines(f, encoding = encoding)
      names(out) <- rep(f, times = length(out))
      out
    }))
  } else {
    articles.v <- stringi::stri_read_lines(files, encoding = encoding)
    names(articles.v) <- rep(files, times = length(articles.v))
  }
  if (verbose) cat("\t...files loaded [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")

  #exclude some lines
  if (length(exclude_lines) > 0) {
    articles.v[grep("^LOAD-DATE: |^UPDATE: |^GRAFIK: |^GRAPHIC: |^DATELINE: ", articles.v)] <- ""
  }

  beginnings <- grep(start_keyword, articles.v)
  #beginnings <- stringi::stri_detect_regex(pattern = start_keyword, str = articles.v)

  articles.l <- lapply(seq_len(length(beginnings)), function(n) {
    if (n < length(beginnings)) {
      articles.v[beginnings[n]:beginnings[n + 1]]
    } else {
      articles.v[beginnings[n]:length(articles.v)]
    }
  })
  rm(articles.v)
  df.l <- lapply(articles.l, function(a) {
    len <- grep(length_keyword, a)[1]
    if (!is.na(len)) {
      source <- names(a)[1]
      meta <- unname(a[2:len])
      article <- unname(a[(len + 1):(length(a) - 1)])
      list(source = source,
           meta = meta,
           article = article,
           graphic = FALSE)
    } else {
      list(source = names(a)[1],
           meta = NULL,
           article = a,
           graphic = TRUE)
    }
  })
  if(verbose){cat("\t...articles split [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  # make data.frame
  ### length
  . <- sapply(df.l, function(i) {
    grep(pattern = length_keyword, x = i$meta, value = TRUE)[1]
  })
  length.v <- gsub(length_keyword, "", .)
  if(verbose){cat("\t...lengths extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  ### Newspaper (first non-emtpy line)
  newspaper.v <- sapply(df.l, function(i) {
    grep(pattern = "^$",
         x = i$meta,
         value = TRUE,
         fixed = FALSE,
         invert = TRUE)[1]
  })
  # remove if newspaper.v contains Date or Beginning
  newspaper.v[grep("January|February|March|April|May|June|July|August|September|October|November|December", newspaper.v)] <- ""
  if(verbose) {cat("\t...newspapers extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  ### Date
  date.v <- sapply(df.l, function(i) {
    . <- stringi::stri_extract_last_regex(str = i$meta[seq_len(10)],
                                          pattern = "\\w+ \\d+, \\d+|\\d+ \\w+ \\d+")
    na.omit(.)[1]
  })
  if(verbose){cat("\t...dates extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  ### Author (where available)
  author.v <- sapply(df.l, function(i) {
    a <- head(grep(pattern = "AUTOR: |VON |BYLINE: ", x = i$meta),
              n = 1)
    if (length(a) > 0) {
      if (!i$meta[a + 1] == "") {
        a <- c(a:(a + 1))
      }
      stringi::stri_join(i$meta[a], collapse = " ")
    } else {
      NA
    }
  })

  if(verbose){cat("\t...authors extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}


  ### section (where available)
  section.v <- sapply(df.l, function(i) {
    grep(pattern = "SECTION: |RUBRIK: ", x = i$meta, value = TRUE)[1]
  })
  if(verbose){cat("\t...sections extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}


  ### edition (where available)
  edition.v <- sapply(seq_len(length(df.l)), function(i) {
    date <- grep(date.v[i], x = df.l[[i]]$meta, fixed = TRUE)
    if (length(date) == 1) {
      d1 <- df.l[[i]]$meta[(date + 1):(date + 2)]
      if (!d1[1] == "") {
        edition.v <- d1[1]
        if (!d1[2] == "") edition.v <- paste(edition.v, d1[2], collapse = "; ")
        edition.v
      } else {
        # alternativly, the edition is sometimes the first non-empty line in the article
        edition.v <- grep("edition",
                          df.l[[i]]$article[!stringi::stri_isempty(str = df.l[[i]]$article)][1],
                          value = TRUE,
                          ignore.case = TRUE)
        ifelse(length(edition.v) == 0,
               "",
               edition.v)
      }
    } else {
      ""
    }
  })
  if(verbose){cat("\t...editions extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  ### Headline
  headline.v <- sapply(seq_len(length(df.l)), function(i) {
    headline <- df.l[[i]]$meta
    headline[c(grep(length.v[i], headline, fixed = TRUE),
               grep(date.v[i], headline, fixed = TRUE),
               grep(newspaper.v[i], headline, fixed = TRUE),
               grep(author.v[i], headline, fixed = TRUE),
               grep(section.v[i], headline, fixed = TRUE))] <- ""
    stringi::stri_join(headline, collapse = " ")
  })
  if(verbose){cat("\t...headlines extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  if(convert_date){
    date.v <- lnt_asDate(date.v, ...)
    if(verbose){cat("\t...dates converted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  }

  # Clean the clutter from objects
  author.v <- gsub(x = author.v,
                   pattern = "AUTOR: |VON |BYLINE: ",
                   replacement = "")
  section.v <- gsub(x = section.v,
                    pattern = "SECTION: |RUBRIK: ",
                    replacement = "")

  ### make data.frame
  meta.df <- data.frame(ID = seq_len(length(df.l)),
                        Source_File = unlist(sapply(df.l, function(i) i[["source"]])),
                        Newspaper = trimws(newspaper.v, which = "both"),
                        Date = date.v,
                        Length = trimws(length.v, which = "both"),
                        Section = trimws(section.v, which = "both"),
                        Author = trimws(author.v, which = "both"),
                        Edition = trimws(edition.v, which = "both"),
                        Headline = trimws(headline.v, which = "both"),
                        Graphic = unlist(sapply(df.l, function(i) i[["graphic"]])),
                        stringsAsFactors = FALSE)
  if(verbose){cat("\t...metadata extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}


  # Cut of after ends in article
  df.l <- lapply(df.l, function(i) {
    end <- tail(grep(end_keyword, i$article), n = 1)
    if (length(end) > 0) {
      i$article <- i$article[1:end - 1]
    }
    i$article
  })
  articles.df <- data.frame(ID = seq_len(length(df.l)),
                            Article = sapply(df.l, function(i) {
                              stringi::stri_join(i, collapse = "\n")
                            }),
                            stringsAsFactors = FALSE)

  if(verbose){cat("\t...article texts extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}

  if(extract_paragraphs){
    # split paragraphs
    . <- stringi::stri_split_fixed(str = articles.df$Article,
                                   pattern = "\n\n",
                                   n = -1L,
                                   omit_empty = TRUE,
                                   simplify = FALSE)
    paragraphs.df <- data.table::rbindlist(lapply(seq_len(length(.)), function(i) {
      data.frame(Art_ID = i,
                 Paragraph = .[[i]][!.[[i]] == "\n"],
                 stringsAsFactors = FALSE)
    }))
    paragraphs.df$Par_ID <- seq_len(nrow(paragraphs.df))
    paragraphs.df <- paragraphs.df[, c("Art_ID", "Par_ID", "Paragraph")]
    if(verbose){cat("\t...paragraphs extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  }else{
    paragraphs.df <- data.frame(Art_ID = NA,
                                Par_ID = NA,
                                Paragraph = NA,
                                stringsAsFactors = FALSE)
  }

  # remove unneccesary whitespace (removes \n as well)
  articles.df$Article <- stringi::stri_replace_all_regex(str = articles.df$Article,
                                                         pattern = c("\\s+", "^\\s|\\s$"),
                                                         replacement = c(" ", ""),
                                                         vectorize_all = FALSE)
  paragraphs.df$Paragraph <- stringi::stri_replace_all_regex(str = paragraphs.df$Paragraph,
                                                             pattern = c("\\s+", "^\\s|\\s$"),
                                                             replacement = c(" ", ""),
                                                             vectorize_all = FALSE)

  if(verbose) cat("Elapsed time: ", format((Sys.time() - start.time), digits = 2, nsmall = 2),"\n", sep = "")
  return(new("LNToutput", meta = meta.df, articles = articles.df, paragraphs = paragraphs.df))
}


#' Check LexisNexis TXT files (deprecated)
#'
#' @param ... No functionality as this was deprecated.
#' @export
lnt_checkFiles <- function(...){
  .Deprecated(msg = "lnt_checkFiles() has been deprecated as it is no longer necessary to check files. Simply run lnt_read().")
}
