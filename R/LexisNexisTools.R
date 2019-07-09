# Startup ----------------------------------------------------------------------

#' @importFrom utils packageVersion
.onAttach <- function(...) {
  packageStartupMessage(
    "LexisNexisTools Version ",
    packageVersion("LexisNexisTools")
  )
}


# Class and Methods ------------------------------------------------------------

#' An S4 class to store the three data.frames created with \link{lnt_read}
#'
#' This S4 class stores the output from \link{lnt_read}. Just like a spreadsheet
#' with multiple worksheets, an LNToutput object consist of three data.frames
#' which you can select using \code{@}. This object class is intended to be an
#' intermediate container. As it stores articles and paragraphs in two separate
#' data.frames, nested in an S4 object, the relevant text data is stored twice
#' in almost the same format. This has the advantage, that there is no need to
#' use special characters, such as "\\n" to indicate a new paragraph. However,
#' it makes the files rather big when you save them directly. They should thus
#' usually be subsetted using \code{@} or converted to a different format using
#' \link{lnt_convert}.
#'
#' @slot meta The metadata of the articles read in.
#' @slot articles The article texts and respective IDs.
#' @slot paragraphs The paragraphs (if the data.frame exists) and respective
#'   article and paragraph IDs.
#' @name LNToutput
#' @importFrom methods new
setClass(
  "LNToutput",
  representation(
    meta = "data.frame",
    articles = "data.frame",
    paragraphs = "data.frame"
  )
)


#' Methods for LNToutput output objects
#'
#' @param x,object An LNToutput object.
#' @param i Rows of the meta data.frame (default) or values of j.
#' @param j The column you want to use to subset the LNToutput object. Takes
#'   character strings.
#' @param invert Invert the selection of i.
#' @param e1,e2 LNToutput objects which will be combined.
#' @name LNToutput_methods
#' @importFrom tibble tibble
NULL


#' @rdname LNToutput_methods
setMethod("dim",
          signature = "LNToutput",
          definition = function(x) {
            c(
              Articles = length(x@meta[[1]]),
              Meta_variable = length(x@meta[1, ]),
              data.frames = ifelse(
                all(is.na(x@paragraphs)),
                2,
                3
              )
            )
          }
)


#' @rdname LNToutput_methods
setMethod("show",
  signature = "LNToutput",
  definition = function(object) {
    cat("Object of class 'LNToutput':\n")
    cat(nrow(object@meta), "Articles\n")
    cat(nrow(object@paragraphs), "Paragraphs\n")
    print(object@meta, n = 6)
    print(object@articles, n = 6)
    print(object@paragraphs, n = 6)
  }
)


#' @rdname LNToutput_methods
setMethod("[",
  signature = "LNToutput",
  definition = function(x, i, j, invert = FALSE) {
    if (missing(j)) {
      x@meta <- x@meta[i, ]
      x@articles <- x@articles[i, ]
      x@paragraphs <- x@paragraphs[x@paragraphs$Art_ID %in% x@meta$ID, ]
    } else {
      if (j %in% colnames(x@meta)) {
        select <- x@meta$ID[x@meta[[j]] %in% i]
      } else if (j %in% colnames(x@articles)) {
        select <- x@articles$ID[x@articles[[j]] %in% i]
      } else if (j %in% colnames(x@paragraphs)) {
        select <- x@paragraphs$Art_ID[x@paragraphs[[j]] %in% i]
      } else {
        stop("'j' was not found to be a valid column name.")
      }
      if (invert) {
        select <- x@meta$ID[!x@meta$ID %in% select]
      }
      x@meta <- x@meta[x@meta$ID %in% select, ]
      x@articles <- x@articles[x@articles$ID %in% select, ]
      x@paragraphs <- x@paragraphs[x@paragraphs$Art_ID %in% select, ]
    }
    return(x)
  }
)


#' @rdname LNToutput_methods
setMethod("+",
  signature = c("LNToutput", "LNToutput"),
  definition = function(e1, e2) {
    IDs <- c(e1@meta$ID, (e2@meta$ID + max(e1@meta$ID)))
    Par_IDs <- c(
      e1@paragraphs$Par_ID,
      (e2@paragraphs$Par_ID + max(e1@paragraphs$Par_ID))
    )
    Art_IDs <- c(
      e1@paragraphs$Art_ID,
      (e2@paragraphs$Art_ID + max(e1@paragraphs$Art_ID))
    )
    e1@meta <- rbind(e1@meta, e2@meta)
    e1@articles <- rbind(e1@articles, e2@articles)
    e1@paragraphs <- rbind(e1@paragraphs, e2@paragraphs)
    if (any(duplicated(e1@meta$ID)) |
      any(duplicated(e1@paragraphs$Par_ID))) {
      warning("After objects were merged, there were duplicated IDs. This was fixed.")
      e1@meta$ID <- IDs
      e1@articles$ID <- IDs
      e1@paragraphs$Art_ID <- Art_IDs
      e1@paragraphs$Par_ID <- Par_IDs
    }
    return(e1)
  }
)


# Main Functions ------------------------------------------------------------


#' Read in a LexisNexis file
#'
#' Read a file from LexisNexis in a supported format and convert it to an object
#' of class \link{LNToutput}. Supported formats are TXT, DOC, RTF and PDF files.
#'
#' @param x Name(s) of file(s) or one or multiple directories containing files
#'   from LexisNexis to be converted.
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8
#'   (the LexisNexis standard value).
#' @param extract_paragraphs A logical flag indicating if the returned object
#'   will include a third data frame with paragraphs.
#' @param convert_date A logical flag indicating if it should be tried to
#'   convert the date of each article into Date format. For non-standard dates
#'   provided by LexisNexis it might be safer to convert dates afterwards (see
#'   \link{lnt_asDate}).
#' @param start_keyword Is used to indicate the beginning of an article. All
#'   articles should have the same number of Beginnings, ends and lengths (which
#'   indicate the last line of metadata). Use regex expression such as "\\d+ of
#'   \\d+ DOCUMENTS$" (which would catch e.g., the format "2 of 100 DOCUMENTS")
#'   or "auto" to try all common keywords. Keyword search is case sensitive.
#' @param end_keyword Is used to indicate the end of an article. Works the same
#'   way as start_keyword. A common regex would be "^LANGUAGE: " which catches
#'   language in all caps at the beginning of the line (usually the last line of
#'   an article).
#' @param length_keyword Is used to indicate the end of the metadata. Works the
#'   same way as start_keyword and end_keyword. A common regex would be
#'   "^LENGTH: " which catches length in all caps at the beginning of the line
#'   (usually the last line of the metadata).
#' @param author_keyword A keyword to identify the author(s) in the metadata.
#' @param exclude_lines Lines in which these keywords are found are excluded.
#'   Set to \code{character()} if you want to turn off this feature.
#' @param recursive A logical flag indicating whether subdirectories are
#'   searched for more files.
#' @param file_pattern Pattern of file types to be included in search for files.
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @param ... Additional arguments passed on to \link{lnt_asDate}.
#' @return An LNToutput S4 object consisting of 3 data.frames for metadata,
#'   articles and paragraphs.
#' @details The function can produce an \link{LNToutput} S4 object with two or
#'   three data.frame: meta, containing all meta information such as date,
#'   author and headline and articles, containing just the article ID and the
#'   text of the articles. When extract_paragraphs is set to TRUE, the output
#'   contains a third data.frame, similar to articles but with articles split
#'   into paragraphs.
#'
#'   When left to 'auto', the keywords will use the following defaults, which
#'   should be the standard keywords in all languages used by 'LexisNexis':
#'
#'   * \code{start_keyword = "\\d+ of \\d+ DOCUMENTS$| Dokument \\d+ von \\d+$|
#'   Document \\d+ de \\d+$"}.
#'
#'   * \code{end_keyword = "^LANGUAGE: |^SPRACHE: |^LANGUE: "}.
#'
#' @author Johannes B. Gruber
#' @export
#' @examples
#' LNToutput <- lnt_read(lnt_sample(copy = FALSE))
#' meta.df <- LNToutput@meta
#' articles.df <- LNToutput@articles
#' paragraphs.df <- LNToutput@paragraphs
#' @import stringi
#' @importFrom utils tail
#' @importFrom tibble tibble as_tibble
lnt_read <- function(x,
                     encoding = "UTF-8",
                     extract_paragraphs = TRUE,
                     convert_date = TRUE,
                     start_keyword = "auto",
                     end_keyword = "auto",
                     length_keyword = "auto",
                     author_keyword = "AUTOR: |VON |BYLINE: ",
                     exclude_lines = "^LOAD-DATE: |^UPDATE: |^GRAFIK: |^GRAPHIC: |^DATELINE: ",
                     recursive = FALSE,
                     file_pattern = ".txt$|.rtf$|.doc$|.pdf$",
                     verbose = TRUE,
                     ...) {

  files <- get_files(x, recursive = recursive, pattern = file_pattern)

  if (start_keyword == "auto") {
    start_keyword <- "\\d+ of \\d+ DOCUMENTS$|\\d+ of \\d+ DOCUMENT$|Dokument \\d+ von \\d+$| Document \\d+ de \\d+$"
  }
  if (end_keyword == "auto") {
    end_keyword <- "^LANGUAGE: |^SPRACHE: |^LANGUE: "
  }
  if (length_keyword == "auto") {
    length_keyword <- "^LENGTH: |^L\u00c4NGE: |^LONGUEUR: "
  }

  # Track the time
  if (verbose) {
    start_time <- Sys.time()
    message(
      "Creating LNToutput from ", length(files),
      ifelse(length(files) > 1,
        " files...",
        " file..."
      )
    )
  }

  lines <- lnt_read_lines(files, encoding)

  if (verbose) {
    message("\t...files loaded [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }

  # exclude some lines
  if (length(exclude_lines) > 0) {
    lines[grep("^LOAD-DATE: |^UPDATE: |^GRAFIK: |^GRAPHIC: |^DATELINE: ", lines)] <- ""
  }

  articles.l <- split(
    lines, cumsum(stringi::stri_detect_regex(lines, start_keyword))
  )
  articles.l[["0"]] <- NULL
  names(articles.l) <- NULL
  rm(lines)

  if (length(articles.l) == 0) {
    stop("No articles found in provided file(s)")
  }

  df.l <- lapply(articles.l, function(a) {
    len <- grep(length_keyword, a)[1]
    if (!is.na(len)) {
      list(
        source = names(a)[1],
        meta = unname(a[2:len]),
        article = unname(a[(len + 1):(length(a) - 1)]),
        graphic = FALSE
      )
    } else {
      list(
        source = names(a)[1],
        meta = NULL,
        article = a,
        graphic = TRUE
      )
    }
  })
  if (verbose) {
    message(
      "\t...articles split [",
      format(
        (Sys.time() - start_time),
        digits = 2,
        nsmall = 2
      ), "]"
    )
  }

  # make data.frame
  ### length
  . <- vapply(df.l, FUN.VALUE = character(1), function(i) {
    grep(pattern = length_keyword, x = i$meta, value = TRUE)[1]
  })
  length.v <- stri_replace_all_regex(., length_keyword, "")
  if (verbose) {
    message("\t...lengths extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }

  ### Newspaper. First non emtpy line
  newspaper.v <- vapply(df.l, FUN.VALUE = character(1), function(i) {
    grep(
      pattern = "^$",
      x = i$meta,
      value = TRUE,
      fixed = FALSE,
      invert = TRUE
    )[1]
  })
  # remove if newspaper.v contains Date or Beginning
  newspaper.v[grep(
    "January|February|March|April|May|June|July|August|September|October|November|December",
    newspaper.v
  )] <- ""
  if (verbose) {
    message(
      "\t...newspapers extracted [",
      format(
        (Sys.time() - start_time),
        digits = 2,
        nsmall = 2
      ),
      "]"
    )
  }

  ### Date
  date.v <- vapply(df.l, FUN.VALUE = character(1), function(i) {
    . <- stringi::stri_extract_last_regex(
      str = i$meta[seq_len(10)],
      pattern = "\\w+ \\d+, \\d+|\\d+ \\w+ \\d+|\\d+. \\w+ \\d+"
    )
    na.omit(.)[1]
  })
  if (verbose) {
    message("\t...dates extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }

  ### Author (where available)
  author.v <- vapply(df.l, FUN.VALUE = character(1), function(i) {
    a <- head(
      which(stri_detect_regex(i$meta, pattern = author_keyword)),
      n = 1
    )
    if (length(a) > 0) {
      if (!i$meta[a + 1] == "") {
        a <- c(a:(a + 1))
      }
      stringi::stri_join(i$meta[a], collapse = " ")
    } else {
      ""
    }
  })
  author.v[author.v == ""] <- NA

  if (verbose) {
    message("\t...authors extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }


  ### section (where available)
  section.v <- vapply(df.l, FUN.VALUE = character(1), function(i) {
    grep(pattern = "SECTION: |RUBRIK: ", x = i$meta, value = TRUE)[1]
  })
  if (verbose) {
    message("\t...sections extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }


  ### edition (where available)
  edition.v <- lapply(seq_along(df.l), function(i) {
    date <- grep(date.v[i], x = df.l[[i]]$meta, fixed = TRUE)
    if (length(date) == 1) {
      d1 <- df.l[[i]]$meta[(date + 1):(date + 2)]
      if (!d1[1] == "") {
        edition.v <- d1[1]
        if (!d1[2] == "") {
          edition.v <- c(edition.v, d1[2])
        }
        edition.v
      } else {
        # Alternatively, the edition is sometimes the first non-empty line in the article
        edition.v <- grep("edition",
          df.l[[i]]$article[!stringi::stri_isempty(str = df.l[[i]]$article)][1],
          value = TRUE,
          ignore.case = TRUE
        )
        ifelse(length(edition.v) == 0,
               NA,
               edition.v
        )
      }
    } else {
      NA
    }
  })

  if (verbose) {
    message("\t...editions extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }

  ### Headline
  headline.v <- vapply(seq_along(df.l), FUN.VALUE = character(1), function(i) {
    if (!df.l[[i]]$graphic) {
      headline <- df.l[[i]]$meta
      pattern <- na.omit(c(
        length.v[i],
        date.v[i],
        newspaper.v[i],
        author.v[i],
        section.v[i],
        edition.v[[i]]
      ))

      remove.m <- vapply(pattern, FUN.VALUE = matrix(nrow = length(headline)), function(p) {
        out <- stringi::stri_detect_fixed(headline, p[1])
        if (length(p) > 1) {
          out + stringi::stri_detect_fixed(headline, p[2])
        } else {
          out
        }
      })
      headline[as.logical(rowSums(remove.m, na.rm = TRUE))] <- ""
      headline <- stringi::stri_join(headline, collapse = " ")
      stri_replace_all_regex(headline, "\\s+", " ")
    } else {
      ""
    }
  })
  if (verbose) {
    message("\t...headlines extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }

  if (convert_date) {
    date.v <- lnt_asDate(date.v, ...)
    if (verbose) {
      message("\t...dates converted [", format(
        (Sys.time() - start_time),
        digits = 2, nsmall = 2
      ), "]")
    }
  }

  # Clean the clutter from objects
  author.v <- stri_replace_all_regex(
    str = author.v,
    pattern = author_keyword,
    replacement = ""
  )
  section.v <- stri_replace_all_regex(
    str = section.v,
    pattern = "SECTION: |RUBRIK: ",
    replacement = ""
  )
  edition.v <- vapply(edition.v, FUN.VALUE = character(1), function(e) {
    out <- stri_paste(e, collapse = " ")
    out <- stri_replace_all_regex(out, "\\s+", " ")
    out[out == ""] <- NA
    stri_trim_both(out)
  })


  ### make data.frame
  meta.df <- tibble(
    ID = seq_along(df.l),
    Source_File = unlist(lapply(df.l, function(i) i[["source"]])),
    Newspaper = trimws(newspaper.v, which = "both"),
    Date = date.v,
    Length = trimws(length.v, which = "both"),
    Section = trimws(section.v, which = "both"),
    Author = trimws(author.v, which = "both"),
    Edition = edition.v,
    Headline = trimws(headline.v, which = "both"),
    Graphic = unlist(lapply(df.l, function(i) i[["graphic"]]))
  )
  if (verbose) {
    message("\t...metadata extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }


  # Cut of after ends in article
  df.l <- lapply(df.l, function(i) {
    end <- tail(grep(end_keyword, i$article), n = 1)
    if (length(end) > 0) {
      i$article <- i$article[1:end - 1]
    }
    i$article
  })
  articles.df <- tibble(
    ID = seq_along(df.l),
    Article = vapply(df.l, FUN.VALUE = character(1), function(i) {
      stringi::stri_join(i, collapse = "\n")
    })
  )

  if (verbose) {
    message("\t...article texts extracted [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }

  if (extract_paragraphs) {
    # split paragraphs
    . <- stringi::stri_split_fixed(
      str = articles.df$Article,
      pattern = "\n\n",
      n = -1L,
      omit_empty = TRUE,
      simplify = FALSE
    )
    paragraphs.df <- data.table::rbindlist(lapply(seq_along(.), function(i) {
      if (length(.[[i]][!.[[i]] == "\n"]) > 0) {
        tibble(
          Art_ID = i,
          Paragraph = .[[i]][!.[[i]] == "\n"]
        )
      } else {
        tibble(
          Art_ID = i,
          Paragraph = NA
        )
      }
    }))
    paragraphs.df$Par_ID <- seq_len(nrow(paragraphs.df))
    paragraphs.df <- paragraphs.df[, c("Art_ID", "Par_ID", "Paragraph")]
    if (verbose) {
      message("\t...paragraphs extracted [", format(
        (Sys.time() - start_time),
        digits = 2, nsmall = 2
      ), "]")
    }
  } else {
    paragraphs.df <- tibble(
      Art_ID = NA,
      Par_ID = NA,
      Paragraph = NA
    )
  }

  # remove unneccesary whitespace (removes \n as well)
  articles.df$Article <- stringi::stri_replace_all_regex(
    str = articles.df$Article,
    pattern = c("\\s+", "^\\s|\\s$"),
    replacement = c(" ", ""),
    vectorize_all = FALSE
  )
  if (verbose) {
    message("\t...superfluous whitespace removed from articles [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
  }
  paragraphs.df$Paragraph <- stringi::stri_replace_all_regex(
    str = paragraphs.df$Paragraph,
    pattern = c("\\s+", "^\\s|\\s$"),
    replacement = c(" ", ""),
    vectorize_all = FALSE
  )
  if (verbose) {
    message("\t...superfluous whitespace removed from paragraphs [", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ), "]")
    message("Elapsed time: ", format(
      (Sys.time() - start_time),
      digits = 2, nsmall = 2
    ))
  }
  out <- new(
    "LNToutput",
    meta = meta.df,
    articles = articles.df,
    paragraphs = tibble::as_tibble(paragraphs.df)
  )
  attributes(out)$created <- list(
    time = Sys.time(),
    Version = packageVersion("LexisNexisTools")
  )
  return(out)
}


#' Assign proper names to LexisNexis files
#'
#' Give proper names to files downloaded from 'LexisNexis' based on search
#' term and period retrieved from each file cover page. This information is not
#' always delivered by LexisNexis though. If the information is not present in
#' the file, new file names will be empty.
#'
#' Warning: This will rename all supported files in a give folder.
#'
#' @param x Can be either a character vector of LexisNexis file name(s),
#'   folder name(s) or can be left blank (see example).
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8
#'   (the LexisNexis standard value).
#' @param recursive A logical flag indicating whether subdirectories are
#'   searched for more files.
#' @param report A logical flag indicating whether the function will return a
#'   report which files were renamed.
#' @param simulate Should the renaming be simulated instead of actually done?
#'   This can help prevent accidental renaming of unrelated files which
#'   happen to be in the same directory as the files from 'LexisNexis'.
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @keywords LexisNexis
#' @author Johannes B. Gruber
#' @export
#' @importFrom stats na.omit
#' @importFrom stringi stri_extract_all_regex stri_join
#' @examples
#'
#' # Copy sample file to current wd
#' lnt_sample()
#'
#' # Rename files in current wd and report back if successful
#' \dontrun{
#' report.df <- lnt_rename(
#'   recursive = FALSE,
#'   report = TRUE
#' )
#' }
#'
#' # Or provide file name(s)
#' my_files <- list.files(
#'   pattern = ".txt", full.names = TRUE,
#'   recursive = TRUE, ignore.case = TRUE
#' )
#' report.df <- lnt_rename(
#'   x = my_files,
#'   recursive = FALSE,
#'   report = TRUE
#' )
#'
#' # Or provide folder name(s)
#' report.df <- lnt_rename(x = getwd())
#'
#' report.df
lnt_rename <- function(x,
                       encoding = "UTF-8",
                       recursive = FALSE,
                       report = TRUE,
                       simulate = TRUE,
                       verbose = FALSE) {
  files <- get_files(x)
  # Track the time
  start_time <- Sys.time()
  if (verbose) message("Checking LN files...")
  files <- unique(files)
  if (verbose) message(length(files), " files found to process...")
  renamed <- data.frame(
    name_orig = files,
    name_new = character(length = length(files)),
    status = character(length = length(files)),
    stringsAsFactors = FALSE
  )
  # start renaming files
  for (i in seq_along(files)) {
    # read in the articles
    content.v <- readLines(files[i], encoding = encoding, n = 50)
    # look for the range of articles
    range.v <- content.v[grep("^Download Request:|^Ausgabeauftrag: Dokument", content.v)]
    # extract the actual range infromation from line
    range.v <- stringi::stri_extract_all_regex(range.v, pattern = "[[:digit:]]|-", simplify = TRUE)
    range.v <- stringi::stri_join(range.v, sep = "", collapse = "")

    # look for search term
    term.v <- content.v[grep("^Terms: |^Begriffe: ", content.v)]
    # erase everything in the line exept the actual range
    term.v <- gsub("^Terms: |^Begriffe: ", "", term.v)
    # split term into elemets seprated by and or OR
    term.v <- unlist(strsplit(term.v, split = " AND | and | OR ", fixed = FALSE))

    date.v <- term.v[grepl("\\d+-\\d+-\\d+", term.v)]
    if (length(date.v) > 1) {
      date.v <- paste0(
        gsub(
          "[^[:digit:]]",
          "",
          term.v[1]
        ),
        "-",
        gsub(
          "[^[:digit:]]",
          "",
          term.v[2]
        )
      )
      term.v <- gsub("[^[:alpha:]]", "", term.v[3])
    } else if (length(date.v) > 0) {
      date.v <- gsub(
        "[^[:digit:]]",
        "",
        term.v
      )[1]
      term.v <- gsub("[^[:alpha:]]", "", term.v[2])
    } else {
      date.v <- "NA"
      term.v <- gsub("[^[:alpha:]]", "", term.v)
    }
    file.name <- sub("[^/]+$", "", files[i]) # take old filepath
    file.name <- paste0(file.name, term.v, "_", date.v, "_", range.v, ".txt")
    # rename file

    if (file.exists(file.name)) {
      renamed$name_new[i] <- renamed$name_orig[i]
      renamed$status[i] <- "not renamed (file exists)"
    } else {
      if (file.name == "__.txt") {
        renamed$name_new[i] <- file.name
        renamed$status[i] <- "not renamed (file is empty)"
      } else {
        renamed$name_new[i] <- file.name
        renamed$status[i] <- "renamed"
        if (!simulate) {
          file.rename(files[i], file.name)
        }
      }
    }
    if (verbose) {
      message("\r\t...renaming files ", format(
        (100 * (i / length(files))),
        digits = 2, nsmall = 2
      ), "%")
    }
  }
  if (verbose) {
    message(sum(grepl("^renamed$", renamed$status)),
            " files renamed, ",
            appendLF = FALSE
    )

    if (sum(grepl("exists", renamed$status, fixed = TRUE)) > 0) {
      message(sum(grepl("exists", renamed$status, fixed = TRUE)),
              " not renamed (file already exists), ",
              appendLF = FALSE
      )
    }
    if (sum(grepl("empty", renamed$status, fixed = TRUE)) > 0) {
      message(sum(grepl("empty", renamed$status, fixed = TRUE)),
              " not renamed (no search term or time range found), ",
              appendLF = FALSE
      )
    }
  }
  renamed$status <- as.factor(renamed$status)
  elapsed <- Sys.time() - start_time
  if (verbose) {
    message(
      " in ", format(elapsed, digits = 2, nsmall = 2), appendLF = FALSE
    )
  }
  if (simulate) message(" [changes were only simulated]")
  if (report) return(renamed)
}


#' Check for highly similar articles.
#'
#' Check for highly similar articles by comparing all articles published on the
#' same date. This function implements two measures to test if articles are
#' almost identical. The function \link[quanteda]{textstat_simil}, which
#' compares the word similarity of two given texts; and a relative modification
#' of the generalized Levenshtein (edit) distance implementation in
#' \link[stringdist]{stringdist}. The relative distance is calculated by
#' dividing the string distance by the number of characters in the longer
#' article (resulting in a minimum of 0 if articles are exactly alike and 1 if
#' strings are completely different). Using both methods cancels out the
#' disadvantages of each method: the similarity measure is fast but does not
#' take the word order into account. Two widely different texts could,
#' therefore, be identified as the same, if they employ the exact same
#' vocabulary for some reason. The generalized Levenshtein distance is more
#' accurate but is very computationally demanding, especially if more than two
#' texts are compared at once.
#'
#' @param texts Provide texts to check for similarity.
#' @param dates Provide corresponding dates, same length as \code{text}.
#' @param LNToutput Alternatively to providing texts and dates individually, you
#'   can provide an LNToutput object.
#' @param IDs IDs of articles.
#' @param threshold At which threshold of similarity is an article considered a
#'   duplicate. Note that lower threshold values will increase the time to
#'   calculate the relative difference (as more articles are considered).
#' @param rel_dist Calculate the relative Levenshtein distance between two
#'   articles if set to TRUE (can take very long). The main difference between
#'   the similarity and distance value is that the distance takes word order
#'   into account while similarity employs the bag of words approach.
#' @param length_diff Before calculating the relative distance between articles,
#'   the length of the articles in characters is calculated. If the difference
#'   surpasses this value, calculation is omitted and the distance will set to
#'   NA.
#' @param nthread Maximum number of threads to use (see
#'   \link[stringdist]{stringdist-parallelization}).
#' @param max_length If the article is too long, calculation of the relative
#'   distance can cause R to crash (see
#'   \url{https://github.com/markvanderloo/stringdist/issues/59}). To prevent
#'   this you can set a maximum length (longer articles will not be evaluated).
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @keywords similarity
#' @return A data.table consisting of information about duplicated
#'   articles. Articles with a lower similarity than the threshold will be
#'   removed, while all relative distances are still in the returned object.
#'   Before you use the duplicated information to subset your dataset, you
#'   should, therefore, filter out results with a high relative distance (e.g.
#'   larger than 0.2).
#' @author Johannes B. Gruber
#' @export
#' @importFrom stringdist stringdist
#' @importFrom quanteda dfm docnames textstat_simil
#' @importFrom utils combn
#' @examples
#' # Copy sample file to current wd
#' lnt_sample()
#'
#' # Convert raw file to LNToutput object
#' LNToutput <- lnt_read(lnt_sample())
#'
#' # Test similarity of articles
#' duplicates.df <- lnt_similarity(
#'   texts = LNToutput@articles$Article,
#'   dates = LNToutput@meta$Date,
#'   IDs = LNToutput@articles$ID
#' )
#'
#' # Remove instances with a high relative distance
#' duplicates.df <- duplicates.df[duplicates.df$rel_dist < 0.2]
#'
#' # Create three separate data.frames from cleaned LNToutput object
#' LNToutput <- LNToutput[!LNToutput@meta$ID %in%
#'   duplicates.df$ID_duplicate]
#' meta.df <- LNToutput@meta
#' articles.df <- LNToutput@articles
#' paragraphs.df <- LNToutput@paragraphs
lnt_similarity <- function(texts,
                           dates,
                           LNToutput,
                           IDs = NULL,
                           threshold = 0.99,
                           rel_dist = TRUE,
                           length_diff = Inf,
                           nthread = getOption("sd_num_thread"),
                           max_length = Inf,
                           verbose = TRUE) {
  call <- match.call(expand.dots = TRUE)
  start_time <- Sys.time()
  if (missing(LNToutput)) {
    if (any(missing(texts), missing(dates))) {
      stop("Supply either 'LNToutput' or 'texts' and 'dates'.")
    }
    if (is.null(IDs)) IDs <- seq_along(texts)
  } else if (!missing(LNToutput)) {
    if (missing(texts)) texts <- LNToutput@articles$Article
    if (missing(dates)) dates <- LNToutput@meta$Date
    if (is.null(IDs)) IDs <- LNToutput@articles$ID
  }
  if (!length(texts) == length(dates) | !length(dates) == length(IDs)) {
    stop("'texts', 'dates' and 'IDs' need to have the same length.")
  }
  dates.d <- unique(dates)
  dates.d <- dates.d[order(dates.d)]
  if (any(is.na(dates.d))) {
    warning("You supplied NA values to 'dates'. Those will be ignored.")
    dates.d <- dates.d[!is.na(dates.d)]
  }
  lenghts <- vapply(texts, nchar, FUN.VALUE = 1, USE.NAMES = FALSE)
  if (any(lenghts < 1)) {
    warning(
      "\nAt least one of the supplied texts had length 0. These articles with the following IDs will be ignored: ",
      paste(IDs[lenghts == 0], collapse = ", ")
    )
    texts <- texts[lenghts > 0]
    dates <- dates[lenghts > 0]
    IDs <- IDs[lenghts > 0]
  }
  if (exists("LNToutput")) rm(LNToutput)
  if (verbose) {
    message(
      "Checking similiarity for ", length(dates),
      " articles over ", length(dates.d), " dates..."
    )
  }
  text_dfm <- quanteda::dfm(texts,
    tolower = TRUE,
    remove = "[^[:alnum:]]",
    valuetype = "regex",
    verbose = FALSE
  )
  if (verbose) {
    message("\t...quanteda dfm construced for similarity comparison [",
      format(
        (Sys.time() - start_time),
        digits = 2, nsmall = 2
      ), "].",
      appendLF = TRUE
    )
  }
  quanteda::docnames(text_dfm) <- as.character(IDs)
  duplicates.df <- lapply(dates.d, function(x) {
    if (sum(x == na.omit(dates)) > 1) {
      text_dfm_day <- quanteda::dfm_subset(text_dfm, subset = (dates == x))
      sim <- stats::as.dist(quanteda::textstat_simil(
        text_dfm_day,
        selection = NULL,
        method = "cosine",
        margin = "documents"
      ))
      . <- t(combn(as.numeric(quanteda::docnames(text_dfm_day)), 2))
      colnames(.) <- c("ID_original", "ID_duplicate")
      duplicates.df <- data.frame(
        .,
        Similarity = as.numeric(sim),
        stringsAsFactors = FALSE
      )
      duplicates.df <- duplicates.df[duplicates.df$Similarity > threshold, ]
      if (nrow(duplicates.df) > 0) {
        duplicates.df$text_original <- texts[match(duplicates.df$ID_original, IDs)]
        duplicates.df$text_duplicate <- texts[match(duplicates.df$ID_duplicate, IDs)]
        duplicates.df$Date <- dates[match(duplicates.df$ID_duplicate, IDs)]
        duplicates.df <- duplicates.df[, c(
          "Date",
          "ID_original",
          "text_original",
          "ID_duplicate",
          "text_duplicate",
          "Similarity"
        )]

        if (rel_dist) {
          duplicates.df$rel_dist <- vapply(seq_len(nrow(duplicates.df)), FUN.VALUE = 1, function(i) {
            # length of longer string
            mxln <- max(c(nchar(duplicates.df$text_original[i]), nchar(duplicates.df$text_duplicate[i])))
            if (isTRUE(
              abs(nchar(duplicates.df$text_original[i]) - nchar(duplicates.df$text_duplicate[i])) /
                mxln <
                length_diff &
                max_length > mxln
            )) {
              stringdist::stringdist(
                a = duplicates.df$text_original[i],
                b = duplicates.df$text_duplicate[i],
                method = "lv",
                useBytes = FALSE,
                nthread = nthread
              ) / # string distance
                mxln # by length of string
            } else {
              NA
            }
          })
        }
        message(
          "\r\t...processing date ",
          as.character(x),
          ": ",
          length(unique(duplicates.df$ID_duplicate)),
          " duplicates found [",
          format(
            (Sys.time() - start_time), digits = 2, nsmall = 2
          ), "]. \t\t",
          appendLF = FALSE
        )
        return(duplicates.df)
      } else {
        message("\r\t...processing date ", as.character(x), ": 0 duplicates found [",
          format(
            (Sys.time() - start_time), digits = 2, nsmall = 2
          ), "]. \t\t",
          appendLF = FALSE
        )
      }
    } else {
      message("\r\t...processing date ", as.character(x), ": 0 duplicates found [",
        format(
          (Sys.time() - start_time), digits = 2, nsmall = 2
        ), "]. \t\t",
        appendLF = FALSE
      )
    }
  })
  duplicates.df <- data.table::rbindlist(duplicates.df)
  class(duplicates.df) <- c(class(duplicates.df), "lnt_sim")
  time.elapsed <- Sys.time() - start_time
  message(
    "\r\nThreshold = ", threshold, "; ",
    length(dates.d), " days processed; ",
    length(unique(duplicates.df$ID_duplicate)), " duplicates found;",
    " in ", format(time.elapsed, digits = 2, nsmall = 2)
  )
  attributes(duplicates.df)$call <- call
  return(duplicates.df)
}


#' @title Convert Strings to dates
#'
#' @description  Converts dates from string formats common in LexisNexis to a
#'   date object.
#'
#' @param x A character object to be converted.
#' @param format Either "auto" to guess the format based on a common order of
#'   day, month and year or provide a custom format (see
#'   \link[stringi]{stri_datetime_format} for format options).
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
#' @importFrom utils head menu
lnt_asDate <- function(x,
                       format = "auto",
                       locale = "auto") {
  dat <- x
  formats <- c(
    English = "MMMM d,yyyy",
    German = "d MMMM yyyy",
    Spanish = "d MMMM yyyy",
    Dutch = "d MMMM yyyy",
    French = "d MMMM yyyy",
    Portuguese = "d MMMM yyyy",
    Italian = "d MMMM yyyy",
    Russian = "d MMMM yyyy"
  )
  locales <- c(
    English = "en",
    German = "de",
    Spanish = "es",
    Dutch = "nl",
    French = "fr",
    Portuguese = "pt",
    Italian = "it",
    Russian = "ru"
  )

  if (!locale == "auto") locales <- locale
  for (loc in locales) {
    dat <- stri_replace_all_regex(
      str = dat,
      pattern = paste0(
        "\\b",
        c(
          stri_datetime_symbols(locale = loc)$Weekday,
          "PM", "AM", "GMT"
        ),
        "\\b"
      ),
      replacement = "",
      vectorize_all = FALSE,
      opts_regex = stri_opts_regex(case_insensitive = TRUE)
    )
  }
  dat <- stri_replace_all_regex(
    str = dat,
    pattern = c(
      "[A-Z]{3}$",
      "((?:(?:[0-1][0-9])|(?:[2][0-3])|(?:[0-9])):(?:[0-5][0-9])(?::[0-5][0-9])?(?:\\s?(?:am|AM|pm|PM))?)"
    ),
    replacement = "",
    vectorize_all = FALSE
  )
  dat <- stri_replace_all_fixed(
    str = dat,
    pattern = "Maerz",
    replacement = "M\u00c4rz",
    vectorize_all = FALSE
  )

  if (any(
    format == "auto",
    locale == "auto"
  )) {
    correct <- mapply(formats, locales, FUN = function(format, locale) {
      out <- stringi::stri_datetime_parse(
        str = dat,
        format = format,
        locale = locale,
        tz = NULL
      )
      out <- 1 - sum(is.na(out)) / length(dat)
      out * 100
    })
    most <- head(sort(correct[correct > 0.01], decreasing = TRUE), n = 3)
    if (is.na(most[1])) stop("No valid dates found.")
    if (most[1] < 100) {
      if (interactive()) {
        langchoice <- menu(
          choices = c(
            "Don't convert dates",
            paste0(names(most)[1], " (", round(most[1], 2), "%", ")"),
            paste0(names(most)[2], " (", round(most[2], 2), "%", ")"),
            if (length(most) == 3) paste0(names(most)[3], " (", round(most[3], 2), "%", ")")
          ),
          title = "More than one language was detected. Choose one:"
        )
      } else {
        warning("More than one language was detected. The most likely one was chosen (",
                paste0(names(most)[1], " ", round(most[1], 2), "%", ""),
                ")")
        langchoice <- 2
      }
    } else {
      langchoice <- 2
    }
    if (langchoice > 0) {
      format <- formats[names(formats) == names(most)[langchoice - 1]]
      locale <- locales[names(locales) == names(most)[langchoice - 1]]
    } else {
      return(x)
    }
  }
  if (!format[1] %in% formats) {
    message("A non-standard format was provided. Conversion is tried but might fail.")
  }
  dat <- stringi::stri_datetime_parse(
    str = dat,
    format = format,
    tz = "UTC",
    locale = locale
  )
  dat <- as.Date(dat)
  return(dat)
}


#' @title Lookup keywords in articles
#'
#' @description This function looks for the provided pattern in the string or
#'   LNToutput object. This can be useful, for example, to see which of the
#'   keywords you used when retrieving the data was used in each article.
#' @details If an LNToutput object is provided, the function will look for the
#'   pattern in the headlines and articles. The returned object is a list of
#'   hits. If a regular expression is provided, the returned word will be the
#'   actual value from the text.
#' @param x An LNToutput object or a string or vector of strings.
#' @param pattern A character vector of keywords. Word boundaries before and
#'   after the keywords are honoured. Regular expression can be used.
#' @param cores The number of CPU cores to use. Use \code{NULL} or \code{1} to
#'   turn off.
#' @param case_insensitive If FALSE, the pattern matching is case sensitive and
#'   if TRUE, case is ignored during matching.
#' @param unique_pattern If TRUE, duplicated mentions of the same pattern are
#'   removed.
#' @param word_boundaries If TRUE or "both", lookup is performed with word
#'   boundaries at beginning and end of the pattern (i.e., pattern "protest"
#'   will not identify "protesters" etc.). Additionally word boundaries can be
#'   either just in front of the pattern ("before") or after the pattern
#'   ("after"). FALSE searches without word boundaries.
#' @param verbose A logical flag indicating whether a status bar is printed to
#'   the screen.
#' @return A list of keyword hits.
#'
#' @examples
#' # Make LNToutput object from sample
#' LNToutput <- lnt_read(lnt_sample())
#'
#' # Lookup keywords
#' LNToutput@meta$Keyword <- lnt_lookup(
#'   LNToutput,
#'   "statistical computing"
#' )
#'
#' # Keep only articles which mention the keyword
#' LNToutput_stat <- LNToutput[!sapply(LNToutput@meta$Keyword, is.null)]
#'
#' # Covert list of keywords to string
#' LNToutput@meta$Keyword <- sapply(LNToutput@meta$Keyword, toString)
#' @author Johannes Gruber
#' @export
#' @importFrom pbapply pboptions pblapply
#' @importFrom parallel makeCluster stopCluster clusterExport
#' @importFrom stringi stri_join stri_extract_all_regex stri_opts_regex
lnt_lookup <- function(x,
                       pattern,
                       case_insensitive = FALSE,
                       unique_pattern = FALSE,
                       word_boundaries = c("both", "before", "after"),
                       cores = NULL,
                       verbose = TRUE) {
  if ("character" %in% class(x)) {
  } else if ("LNToutput" %in% class(x)) {
    IDs <- x@meta$ID
    x <- stringi::stri_join(x@meta$Headline,
      x@articles$Article,
      sep = " \n "
    )
    names(x) <- IDs
  } else {
    (
      stop("'x' must be either a character vector or LNToutput object.")
    )
  }
  if (!is.null(word_boundaries) | isFALSE(word_boundaries)) {
    if (word_boundaries[1] == "both" | isTRUE(word_boundaries)) {
      pattern <- paste0(
        "\\b",
        pattern,
        "\\b"
      )
    }
    if (word_boundaries[1] == "before") {
      pattern <- paste0(
        "\\b",
        pattern
      )
    }
    if (word_boundaries[1] == "after") {
      pattern <- paste0(
        pattern,
        "\\b"
      )
    }
  }
  if (!verbose) {
    pbapply::pboptions(type = "none")
  } else {
    pbapply::pboptions(type = "timer")
  }
  if (isTRUE(cores > 1)) {
    cl <- parallel::makeCluster(cores)
    force(pattern)
    parallel::clusterExport(cl = cl, varlist = "pattern", envir = environment())
  } else {
    cl <- NULL
  }
  return <- pbapply::pblapply(x, cl = cl, function(s) {
    out <- stringi::stri_extract_all_regex(
      str = s,
      pattern = pattern,
      vectorize_all = TRUE,
      omit_no_match = FALSE,
      simplify = FALSE,
      opts_regex = stringi::stri_opts_regex(
        case_insensitive = case_insensitive
      )
    )
    out[is.na(out)] <- NULL
    if (unique_pattern) {
      return(unique(unlist(out)))
    } else {
      return(unlist(out))
    }
  })
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  return(return)
}


#' @title Display diff of similar articles
#'
#' @description This function is a wrapper for \link[diffobj]{diffPrint}. It is
#'   intended to help performing a manual assessment of the difference between
#'   highly similar articles identified via \link{lnt_similarity}.
#'
#' @param x lnt_sim object as returned by \link{lnt_similarity}.
#' @param min Minimum value of rel_dist to include in diff.
#' @param max Maximum value of rel_dist to include in diff.
#' @param n Size of displayed sample.
#' @param output_html Set to TRUE to output html code, e.g. to use for knitting
#'   an rmarkdown document to html. Chunk option must be set to
#'   \code{results='asis'} in that case.
#' @param ... Currently not used.
#'
#' @examples
#' # Test similarity of articles
#' duplicates.df <- lnt_similarity(
#'   LNToutput = lnt_read(lnt_sample()),
#'   threshold = 0.97
#' )
#'
#' lnt_diff(duplicates.df, min = 0.18, max = 0.30)
#' @author Johannes Gruber
#' @export
#' @importFrom quanteda tokens
lnt_diff <- function(x,
                     min = 0.15,
                     max = 0.3,
                     n = 25,
                     output_html = FALSE,
                     ...) {
  if (!"lnt_sim" %in% class(x)) {
    warning("'x' should be an object returned by lnt_similarity().")
  }
  check_install("diffobj")
  if (!"rel_dist" %in% colnames(x)) {
    stop("'x' must contain a column with rel_dist information (see ?lnt_similarity)")
  }
  x <- x[x$rel_dist > min & x$rel_dist < max, ]
  if (nrow(x) < n) {
    n <- nrow(x)
  }
  sample <- sample(x = seq_len(nrow(x)), size = n)
  x <- x[sample, ]
  x <- x[order(x$rel_dist), ]
  for (i in seq_len(nrow(x))) {
    original <- unname(unlist(quanteda::tokens(x$text_original[i], what = "sentence")))
    duplicate <- unname(unlist(quanteda::tokens(x$text_duplicate[i], what = "sentence")))
    diff <- diffobj::diffPrint(
      current = original,
      target = duplicate,
      mode = "sidebyside",
      cur.banner = paste("ID:", x$ID_original[i]),
      tar.banner = paste0(
        "ID: ", x$ID_duplicate[i], ", rel_dist: ",
        round(x$rel_dist[i], digits = 2)
      ),
      format = ifelse(output_html, "html", "auto"),
      interactive = !output_html
    )
    print(diff)
  }
  if (output_html) {
    cat(
      "<style>", readLines(system.file("css", "diffobj.css", package = "diffobj")),
      "</style>"
    )
  }
}

# Conversion ------------------------------------------------------------

#' Convert LNToutput to other formats
#'
#' Takes output from \link{lnt_read} and converts it to other formats. You can
#' either use \code{lnt_convert()} and choose the output format via \code{to} or
#' use the individual functions directly.
#'
#' @param x An object of class LNToutput.
#' @param to Which format to convert into. Possible values are "rDNA",
#'   "corpustools", "tidytext", "tm", "SQLite" and "quanteda".
#' @param what Either "Articles" or "Paragraphs" to use articles or paragraphs as
#'   text in the output object.
#' @param collapse Only has an effect when \code{what = "Articles"}. If set to
#'   TRUE, an empty line will be added after each paragraphs. Alternatively you
#'   can enter a custom string (such as \code{"\\n"} for newline). \code{NULL}
#'   or \code{FALSE} turns off this feature.
#' @param file The name of the database to be written to (for lnt2SQLite only).
#' @param ... Passed on to different methods (see details).
#'
#' @details lnt_convert() provides conversion methods into several formats
#'   commonly used in prominent R packages for text analysis. Besides the
#'   options set here, the ... (ellipsis) is passed on to the individual methods
#'   for tuning the outcome:
#'
#'   * rDNA ... not used.
#'
#'   * quanteda ... passed on to [quanteda::corpus()].
#'
#'   * corpustools ... passed on to [corpustools::create_tcorpus()].
#'
#'   * tm ... passed on to [tm::Corpus()].
#'
#'   * tidytext ... passed on to [tidytext::unnest_tokens()].
#'
#'   * lnt2SQLite ... passed on to [RSQLite::dbWriteTable()].
#'
#' @examples
#' LNToutput <- lnt_read(lnt_sample())
#'
#' docs <- lnt_convert(LNToutput, to = "rDNA")
#'
#' corpus <- lnt_convert(LNToutput, to = "quanteda")
#'
#' dbloc <- lnt_convert(LNToutput, to = "lnt2SQLite")
#'
#' tCorpus <- lnt_convert(LNToutput, to = "corpustools")
#'
#' tidy <- lnt_convert(LNToutput, to = "tidytext")
#'
#' Corpus <- lnt_convert(LNToutput, to = "tm")
#' @export
#' @md

lnt_convert <- function(x,
                        to = "rDNA",
                        what = "Articles",
                        collapse = FALSE,
                        file = "LNT.sqlite",
                        ...) {
  if (to == "rDNA") {
    return(lnt2rDNA(x, what = what, collapse = collapse))
  } else if (to == "quanteda") {
    return(lnt2quanteda(x, what = what, collapse = collapse, ...))
  } else if (to == "SQLite") {
    return(lnt2SQLite(x, file = file, collapse = collapse, ...))
  } else if (to == "corpustools") {
    return(lnt2cptools(x, what = what, collapse = collapse, ...))
  } else if (to == "tm") {
    return(lnt2tm(x, what = what, collapse = collapse, ...))
  } else if (to == "tidytext") {
    return(lnt2tidy(x, what = what, collapse = collapse, ...))
  }
}

#' @rdname lnt_convert
#' @export
lnt2rDNA <- function(x, what = "Articles", collapse = TRUE) {
  if (!what %in% c("Articles", "Paragraphs")) {
    stop("Choose either \"Articles\" or \"Paragraphs\" as what argument.")
  }
  if (isTRUE(collapse)) {
    collapse <- "\n\n"
  } else if (is.logical(collapse) && length(collapse) == 1L && !is.na(collapse) && !collapse) {
    collapse <- NULL
  }
  if (what == "Articles") {
    if (is.null(collapse)) {
      text <- x@articles$Article
    } else if (!is.null(collapse)) {
      text <- vapply(x@meta$ID, FUN.VALUE = character(1), function(id) {
        stringi::stri_join(x@paragraphs$Paragraph[x@paragraphs$Art_ID == id],
          sep = "",
          collapse = collapse,
          ignore_null = FALSE
        )
      })
    }
    notes <- paste("ID:", x@meta$ID)
    order <- seq_along(x@meta$ID)
  } else if (what == "Paragraphs") {
    text <- x@paragraphs$Paragraph
    notes <- paste0("Art_ID: ", x@paragraphs$Art_ID, "; Par_ID", x@paragraphs$Par_ID)
    order <- match(x@paragraphs$Art_ID, x@meta$ID)
  }
  dta <- data.frame(
    id = seq_along(order),
    title = vapply(x@meta$Headline[order],
                   FUN.VALUE = character(1),
                   trim,
                   n = 197,
                   USE.NAMES = FALSE
    ),
    text = text,
    coder = 1,
    author = x@meta$Author[order],
    source = x@meta$Newspaper[order],
    section = x@meta$Section[order],
    notes = notes,
    type = "newspaper",
    date = x@meta$Date[order],
    stringsAsFactors = FALSE
  )
  if (any(grepl("Date", class(dta$date)))) {
    dta$date <- as.POSIXct.Date(dta$date)
  }
  if (any(is.na(dta$date), !any(grepl("POSIXct", class(dta$date))))) {
    warning(paste0(
      "One or more (or all) dates could not be converted to POSIXct.",
      "Na entries in 'date' were filled with the system's time and date instead."
    ))
    dta$date <- tryCatch(as.POSIXct(dta$date),
      error = function(e) NA
    )
    dta$date[is.na(dta$date)] <- Sys.time()
    if (class(dta$date) == "numeric") {
      dta$date <- as.POSIXct.numeric(dta$date, origin = "1970-01-01")
    }
  }
  dta[is.na(dta)] <- ""
  return(dta)
}

#' @rdname lnt_convert
#' @export
#' @importFrom quanteda corpus metacorpus
lnt2quanteda <- function(x, what = "Articles", collapse = NULL, ...) {
  if (!what %in% c("Articles", "Paragraphs")) {
    stop("Choose either \"Articles\" or \"Paragraphs\" as what argument.")
  }
  if (isTRUE(collapse)) {
    collapse <- "\n\n"
  } else if (is.logical(collapse) && length(collapse) == 1L && !is.na(collapse) && !collapse) {
    collapse <- NULL
  }
  if (what == "Articles") {
    if (is.null(collapse)) {
      text <- x@articles$Article
    } else if (!is.null(collapse)) {
      text <- vapply(x@meta$ID, FUN.VALUE = character(1), function(id) {
        stringi::stri_join(x@paragraphs$Paragraph[x@paragraphs$Art_ID == id],
          sep = "",
          collapse = collapse,
          ignore_null = FALSE
        )
      })
    }
    ID <- x@meta$ID
    meta <- x@meta
  } else if (what == "Paragraphs") {
    text <- x@paragraphs$Paragraph
    ID <- x@paragraphs$Par_ID
    meta <- merge(
      x@meta,
      x@paragraphs[, c("Art_ID", "Par_ID")],
      by.x = "ID",
      by.y = "Art_ID",
      all.x = FALSE,
      all.y = TRUE
    )
  }
  dots <- list(...)

  if (any(grepl("metacorpus", names(dots)))) {
    metacorpus <- c(list(
      converted_from = "LexiNexisTools"),
      dots$metacorpus
    )
    dots$metacorpus <- NULL
  } else {
    metacorpus <- list(converted_from = "LexiNexisTools")
  }

  dta <- corpus(
    x = text,
    docnames = as.character(ID),
    docvars = meta,
    dots
  )

  quanteda::metacorpus(dta, names(metacorpus)) <- unname(unlist(unname(metacorpus)))

  return(dta)
}


#' @rdname lnt_convert
#' @export
lnt2tm <- function(x, what = "Articles", collapse = NULL, ...) {
  if (!what %in% c("Articles", "Paragraphs")) {
    stop("Choose either \"Articles\" or \"Paragraphs\" as what argument.")
  }
  check_install("tm")
  if (isTRUE(collapse)) {
    collapse <- "\n\n"
  } else if (is.logical(collapse) && length(collapse) == 1L && !is.na(collapse) && !collapse) {
    collapse <- NULL
  }
  if (what == "Articles") {
    if (is.null(collapse)) {
      text <- x@articles$Article
    } else if (!is.null(collapse)) {
      text <- vapply(x@meta$ID, FUN.VALUE = character(1), function(id) {
        stringi::stri_join(x@paragraphs$Paragraph[x@paragraphs$Art_ID == id],
          sep = "",
          collapse = collapse,
          ignore_null = FALSE
        )
      })
    }
    df <- data.frame(
      doc_id = x@articles$ID,
      text = text
    )
    df <- merge.data.frame(df,
      x@meta,
      by.x = "doc_id",
      by.y = "ID"
    )
  } else if (what == "Paragraphs") {
    df <- data.frame(
      doc_id = x@paragraphs$Par_ID,
      text = x@paragraphs$Paragraph,
      par_id = x@paragraphs$Par_ID
    )
    df <- merge.data.frame(df,
      x@meta,
      by.x = "doc_id",
      by.y = "ID",
      all.x = TRUE
    )
  }
  corpus <- tm::Corpus(tm::DataframeSource(df), ...)
  return(corpus)
}


#' @rdname lnt_convert
#' @export
#' @importFrom methods slot slotNames
lnt2cptools <- function(x, what = "Articles", collapse = NULL, ...) {
  if (!what %in% c("Articles", "Paragraphs")) {
    stop("Choose either \"Articles\" or \"Paragraphs\" as what argument.")
  }
  check_install("corpustools")
  if (isTRUE(collapse)) {
    collapse <- "\n\n"
  } else if (is.logical(collapse) && length(collapse) == 1L && !is.na(collapse) && !collapse) {
    collapse <- NULL
  }
  if (what == "Articles") {
    if (is.null(collapse)) {
      text <- x@articles$Article
    } else if (!is.null(collapse)) {
      text <- vapply(x@meta$ID, FUN.VALUE = character(1), function(id) {
        stringi::stri_join(x@paragraphs$Paragraph[x@paragraphs$Art_ID == id],
          sep = "",
          collapse = collapse,
          ignore_null = FALSE
        )
      })
    }
    ID <- x@meta$ID
    meta <- x@meta
  } else if (what == "Paragraphs") {
    text <- x@paragraphs$Paragraph
    ID <- x@paragraphs$Par_ID
    meta <- merge(
      x@meta,
      x@paragraphs[, c("Art_ID", "Par_ID")],
      by.x = "ID",
      by.y = "Art_ID",
      all.x = FALSE,
      all.y = TRUE
    )
  }
  tcorpus <- corpustools::create_tcorpus(
    x = text,
    doc_id = ID,
    meta = meta,
    ...
  )
  return(tcorpus)
}


lnt2tidy <- function(x, what = "Articles", collapse = NULL, ...) {
  if (!what %in% c("Articles", "Paragraphs")) {
    stop("Choose either \"Articles\" or \"Paragraphs\" as what argument.")
  }
  check_install("tidytext")
  if (isTRUE(collapse)) {
    collapse <- "\n\n"
  } else if (is.logical(collapse) && length(collapse) == 1L && !is.na(collapse) && !collapse) {
    collapse <- NULL
  }
  if (what == "Articles") {
    if (!is.null(collapse)) {
      x@articles$Article <- vapply(x@meta$ID, FUN.VALUE = character(1), function(id) {
        stringi::stri_join(x@paragraphs$Paragraph[x@paragraphs$Art_ID == id],
          sep = "",
          collapse = collapse,
          ignore_null = FALSE
        )
      })
    }
    df <- merge.data.frame(x@meta,
      x@articles,
      by = "ID"
    )
    tidy <- tidytext::unnest_tokens(
      tbl = df,
      input = "Article",
      output = "Token",
      ...
    )
  } else if (what == "Paragraphs") {
    df <- merge.data.frame(
      x@paragraphs,
      x@meta,
      by.x = "Art_ID",
      by.y = "ID"
    )
    tidy <- tidytext::unnest_tokens(
      tbl = df,
      input = "Paragraph",
      output = "Token",
      ...
    )
  }
  return(tidy)
}


#' @rdname lnt_convert
#' @export
#' @importFrom methods slot slotNames
lnt2SQLite <- function(x, file = "LNT.sqlite", ...) {
  check_install("RSQLite")
  db <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  for (i in slotNames(x)) {
    RSQLite::dbWriteTable(
      conn = db,
      name = i,
      value = slot(x, i),
      ...
    )
  }
  on.exit(RSQLite::dbDisconnect(db))
  return(db)
}


#' Convert LNToutput to other formats
#'
#' Takes output from \link{lnt_read} and converts chosen articles to a BibTeX
#' citation.
#'
#' @param x An object of class LNToutput.
#' @param art_id The ID(s) of the article(s) to convert.
#' @param ... unused.
#'
#' @importFrom tools toTitleCase
#' @export
#'
#' @examples
#' LNToutput <- lnt_read(lnt_sample())
#'
#' bib <- lnt2bibtex(LNToutput, art_id = 1)
lnt2bibtex <- function(x, art_id, ...) {

  dat <- x[x@meta$ID %in% art_id]

  out <- lapply(seq_len(nrow(dat)), function(i) {

    meta <- dat[i]@meta

    bib <- c(
      "@article{",
      paste0("  author = {", tools::toTitleCase(tolower(meta$Author)), "},"),
      paste0("  year = {", meta$Date, "},"),
      paste0("  title = {", meta$Headline, "},"),
      paste0("  volume = {", format(meta$Date, "%Y"), "},"),
      paste0("  journal = {", meta$Newspaper, "}"),
      "}"
    )

    attr(bib, "names") <- c(
      "",
      "author",
      "year",
      "title",
      "volume",
      "journal",
      ""
    )

    class(bib) <- "Bibtex"
    return(bib)
  })

  if (length(out) > 1) {
    return(out)
  } else {
    return(out[[1]])
  }
}


# Miscellaneous ------------------------------------------------------------

#' Title
#'
#' @param pkg
#'
#' @noRd
#'
#' @importFrom utils install.packages menu
check_install <- function(pkg) {
  tested <- try(find.package(pkg), silent = TRUE)
  if (class(tested)[1] == "try-error") {
    if (interactive()) {
      message(
        "Package \"",
        pkg,
        "\" is needed for this function to work. ",
        "Should I install it for you?"
      )
      installchoice <- menu(c("yes", "no"))
      if (installchoice == 1) install.packages(pkgs = pkg)
    } else {
      stop("Package \"", pkg, "\" is needed for this function to work.",
        " Please install it.",
        call. = FALSE
      )
    }
  }
}


#' @title Adds or replaces articles
#'
#' @description This functions adds a dataframe to a slot in an LNToutput object
#'   or overwrites existing entries. The main use of the function is to add an
#'   extract of one of the data.frames back to an LNToutput object after
#'   operations were performed on it.
#' @details Note, that when adding paragraphs, the Par_ID column is used to
#'   determine if entries are already present in the set. For the other data
#'   frames the article ID is used.
#' @param to an LNToutput object to which something should be added.
#' @param what A data.frame which is added.
#' @param where Either "meta", "articles" or "paragraphs" to indicate the slot
#'   to which data is added.
#' @param replace If TRUE, will overwrite entries which have the same ID as
#'
#' @examples
#' # Make LNToutput object from sample
#' LNToutput <- lnt_read(lnt_sample())
#'
#' # extract meta and make corrections
#' correction <- LNToutput@meta[grepl("Wikipedia", LNToutput@meta$Headline), ]
#' correction$Newspaper <- "Wikipedia"
#'
#' # replace corrected meta information
#' LNToutput <- lnt_add(to = LNToutput, what = correction, where = "meta", replace = TRUE)
#' @author Johannes Gruber
#' @export
#' @importFrom methods slot
lnt_add <- function(to,
                    what,
                    where = "meta",
                    replace = TRUE) {
  if (where %in% c("meta", "articles")) {
    temp <- slot(to, where)
    if (any(what$ID %in% temp$ID)) {
      if (replace) {
        update <- what$ID %in% temp$ID
        temp <- temp[!temp$ID %in% what$ID, ]
        temp <- rbind(
          temp,
          what[update, ]
        )
        temp <- temp[order(temp$ID), ]
        message(sum(update), " entries in ", where, " replaced, ", sum(!update), " newly added.")
      } else {
        update <- !what$ID %in% temp$ID
        temp <- rbind(temp, what[update, ])
        temp <- temp[order(temp$ID), ]
        message(sum(update), " entries added to ", where, ", ", sum(!update), " already present.")
      }
    } else {
      temp <- rbind(temp, what[update, ])
      temp <- temp[order(temp$ID), ]
      message(nrow(what), " entries added to ", where, ".")
    }
  } else if (where %in% "paragraphs") {
    temp <- slot(to, where)
    if (any(what$Par_ID %in% temp$Par_ID)) {
      if (replace) {
        update <- what$Par_ID %in% temp$Par_ID
        temp <- temp[!temp$Par_ID %in% what$Par_ID, ]
        temp <- rbind(temp, what[update, ])
        temp <- temp[order(temp$Par_ID), ]
        message(sum(update), " entries in ", where, " replaced, ", sum(!update), " newly added.")
      } else {
        update <- !what$Par_ID %in% temp$Par_ID
        temp <- rbind(temp, what[update, ])
        temp <- temp[order(temp$Par_ID), ]
        message(sum(update), " entries added to ", where, ", ", sum(!update), " already present.")
      }
    } else {
      temp <- rbind(temp, what[update, ])
      temp <- temp[order(temp$Par_ID), ]
      message(nrow(what), " entries added to ", where, ".")
    }
  } else {
    stop("Choose either 'meta', 'articles' or 'paragraphs' as 'to' argument.")
  }
  if (where %in% "meta") {
    to@meta <- temp
  } else if (where %in% "articles") {
    to@articles <- temp
  } else if (where %in% "paragraphs") {
    to@paragraphs <- temp
  }
  if (!isTRUE(
    all.equal(length(to@meta$ID), length(to@articles$ID), length(unique(to@paragraphs$Art_ID)))
  )) {
    warning("Returned object is out of balance (one slot has more or less entries than another.")
  }
  return(to)
}


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
#' @param path The destination path for the sample file (current working
#'   directory if \code{NULL})
#' @param copy Logical. Should the file be copied to path/working directory? If
#'   \code{FALSE}, the function only returns the location of the sample file.
#'
#' @examples
#' \dontrun{
#'   lnt_sample()
#' }
#' @author Johannes Gruber
#' @export
lnt_sample <- function(overwrite = FALSE,
                       verbose = TRUE,
                       path = NULL,
                       copy = TRUE) {
  if (is.null(path)) {
    path <- getwd()
  }
  if (copy) {
    to <- paste0(path, "/", "sample.TXT")
    if (all(file.exists(paste0(path, "/", "sample.TXT")), !overwrite)) {
      if (verbose) {
        warning(
          "Sample file exists in wd. Use overwrite = TRUE to create fresh sample file."
        )
      }
    } else {
      file.copy(
        from = system.file("extdata", "sample.TXT", package = "LexisNexisTools"),
        to = to,
        overwrite = TRUE
      )
    }
  } else {
    to <- system.file("extdata", "sample.TXT", package = "LexisNexisTools")
  }
  return(to)
}


#' Truncate
#'
#' Internal function, used to truncate text
#'
#' @param x A character string
#' @param n Max number of characters to truncate to. Value \code{Inf} turns off
#'   truncation.
#' @param e String added at the end of x to signal it was truncated.
#'
#' @noRd
#' @author Johannes B. Gruber
trim <- function(object, n, e = "...") {
  ifelse(nchar(object) > n,
    paste0(
      gsub(
        "\\s+$", "",
        strtrim(object, width = n)
      ),
      e
    ),
    object
  )
}

#' Get files
#'
#' Find files from LexisNexis in folder(s).
#'
#' @param x character, name or names of file(s) or folder(s) to be searched.
#' @param pattern file pattern to be searched.
#' @param recursive logical. Should the listing recurse into directories?
#' @param ignore_case logical. Should pattern-matching be case-insensitive?
#'
#' @importFrom stringi stri_replace_all_fixed
#'
#' @noRd
get_files <- function(x,
                      pattern = ".txt$|.rtf$|.doc$|.pdf$",
                      recursive = TRUE,
                      ignore_case = TRUE) {
  # Check how files are provided
  # 1. nothing (search wd)
  # 2. file or files
  # 3. folder name(s)
  if (missing(x)) {
    message("No path was given. Should files",
            "in working directory be renamed? [y/n]")
    if (menu(c("yes", "no")) == 1) {
      x <- getwd()
    } else {
      stop("Aborted by user")
    }
  }
  if (all(grepl(pattern, x, ignore.case = ignore_case))) {
    files <- x
  } else if (any(grepl(pattern, x, ignore.case = ignore_case))) {
    warning("Not all provided files were TXT, DOC, RTF or PDF files. Other formats are ignored.")
    files <- grep(pattern, x, ignore.case = ignore_case, value = TRUE)
  } else if (any(dir.exists(x))) {
    if (length(x) > 1) {
      files <- unlist(lapply(x, function(f) {
        list.files(
          path = f,
          pattern = pattern,
          ignore.case = ignore_case,
          full.names = TRUE,
          recursive = recursive
        )
      }))
    } else {
      files <- list.files(
        path = x,
        pattern = pattern,
        ignore.case = ignore_case,
        full.names = TRUE,
        recursive = recursive
      )
    }
  } else {
    stop("Provide either file name(s) ending on ",
         stri_replace_all_fixed(
           pattern,
           c("$", "|"),
           c("", "or"),
           vectorize_all = FALSE
         ),
         " or folder name(s) to x or leave black to search wd.")
  }
  if (length(files) > 0) {
    return(files)
  } else {
    stop("No ",
         stri_replace_all_fixed(
           pattern,
           c("$", "|"),
           c("", " or "),
           vectorize_all = FALSE
         ),
         " files found.")
  }
}

#' Get files
#'
#' Internal function, used read files of differnt formats
#'
#' @param files character, name or names of files to be read.
#' @param encoding Encoding to be assumed for input files.
#'
#' @importFrom stringi stri_extract_last_regex stri_read_lines stri_paste
#'
#' @noRd
#' @author Johannes B. Gruber
#'
lnt_read_lines <- function(files,
                           encoding) {

  files <- split(files, tolower(stri_extract_last_regex(files, ".{4}$")))

  ### read in txt file
  if (length(files$.txt) > 0) {
    if (length(files$.txt) > 1) {
      lines_txt <- unlist(lapply(files$.txt, function(f) {
        out <- stri_read_lines(f, encoding = encoding)
        names(out) <- rep(f, times = length(out))
        out
      }))
    } else {
      lines_txt <- stri_read_lines(files$.txt, encoding = encoding)
      names(lines_txt) <- rep(files$.txt, times = length(lines_txt))
    }
  } else {
    lines_txt <- character()
  }

  ### read in doc file
  if (length(files$.doc) > 0) {
    check_install("striprtf")
    if (length(files$.doc) > 1) {
      lines_doc <- unlist(lapply(files$.doc, function(f) {
        out <- striprtf::read_rtf(f)
        names(out) <- rep(f, times = length(out))
        out
      }))
    } else {
      lines_doc <- striprtf::read_rtf(files$.doc)
      names(lines_doc) <- rep(files$.doc, times = length(lines_doc))
    }
  } else {
    lines_doc <- character()
  }

  ### read in rtf file
  if (length(files$.rtf) > 0) {
    check_install("striprtf")
    if (length(files$.rtf) > 1) {
      lines_rtf <- unlist(lapply(files$.rtf, function(f) {
        out <- striprtf::read_rtf(f)
        names(out) <- rep(f, times = length(out))
        out
      }))
    } else {
      lines_rtf <- striprtf::read_rtf(files$.rtf)
      names(lines_rtf) <- rep(files$.rtf, times = length(lines_rtf))
    }
  } else {
    lines_rtf <- character()
  }

  ### read in pdf file
  if (length(files$.pdf) > 0) {
    check_install("pdftools")
    if (length(files$.pdf) > 1) {
      lines_pdf <- unlist(lapply(files$.pdf, function(f) {
        out <- pdftools::pdf_text(f)
        out <- stri_paste(out, collapse = "\n")
        out <- unlist(stri_split_fixed(out, "\n"))
        # remove page number
        out <- out[!stri_detect_regex(out, "^\\s{75,}")]
        # remove page break
        out <- out[!stri_detect_regex(out, "^$")]
        names(out) <- rep(f, times = length(out))
        out
      }))
    } else {
      lines_pdf <- pdftools::pdf_text(files$.pdf)
      lines_pdf <- stri_paste(lines_pdf, collapse = "\n")
      lines_pdf <- unlist(stri_split_fixed(lines_pdf, "\n"))
      lines_pdf <- lines_pdf[!stri_detect_regex(lines_pdf, "^\\s{75,}")]
      lines_pdf <- lines_pdf[!stri_detect_regex(lines_pdf, "^$")]
      names(lines_pdf) <- rep(files$.pdf, times = length(lines_pdf))
    }
    warning("Reading PDFs is experimental. Extracting paragraphs from PDFs does ",
            "not work correctly. Page headers end up in articles.")
  } else {
    lines_pdf <- character()
  }
  return(c(lines_txt, lines_doc, lines_rtf, lines_pdf))
}
