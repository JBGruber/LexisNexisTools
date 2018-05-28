#' Check LexisNexis TXT files
#'
#' Read a LexisNexis TXT file and check consistency. 
#' 
#' The output will contain three tests:
#' - test1: 
#' Indicates whether the number of beginnings and the number of ends
#' match in a file. It is critical, that this is TRUE. Otherwise \link{lnt_read}
#' will not be able to separate individual articles from each other. 
#' - test2:
#' Indicates whether the number of beginnings and the number of lengths match.
#' As 'LENGTH' is used to separate metadata from actual articles, it is
#' critical, that this is TRUE. Otherwise \link{lnt_read} will fail with an error
#' when trying to read this file.
#' - test3:
#' Indicates whether the number of beginnings equals the number of articles
#' LexisNexis delivered. It is most likely not a problem if this is FALSE, as
#' some articles from nexis are empty and therefore get deleted. So far, this
#' has only been the case when an article contained a photo and nothing else.
#' @param x Name or names of LexisNexis TXT file to be converted.
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8
#'   (the LexisNexis standard value).
#' @param recursive A logical flag indicating whether subdirectories are
#'   searched for more txt files.
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @param start_keyword,end_keyword,length_keyword see \link{lnt_read}.
#' @keywords LexisNexis
#' @details Can check consistency of LexisNexis txt files. lnt_read needs at
#'   least Beginning, End and length in each article to work.
#' @author Johannes B. Gruber
#' @export
#' @examples
#' # Copy sample file to current wd
#' lnt_sample()
#' 
#' # Search for txt files in working directory
#' my_files<-list.files(pattern = ".TXT",
#'                      full.names = TRUE,
#'                      recursive = TRUE,
#'                      ignore.case = TRUE)
#' # Test consistency of files
#' checks.df <- lnt_checkFiles(my_files)
#' checks.df
lnt_checkFiles <- function(x,
                          encoding = "UTF-8",
                          start_keyword = "auto",
                          end_keyword = "auto",
                          length_keyword = "auto",
                          recursive = FALSE,
                          verbose = TRUE){
  if (missing(x)) {
    if (readline(prompt="No path was given. Should files in working direcotry be checked? [y/n]") 
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
  if(verbose){start.time <- Sys.time(); cat("Checking LN files...\n")}

  ### read in file
  if (length(files) == 0) {
    stop("No txt files found!")
  }
  out <- lapply(files, function(i){
    if(verbose){cat("\r\tChecking file:",i,"...\r",sep="")}
    articles.v <- stringi::stri_read_lines(i, encoding = encoding)
    Beginnings <- grep(start_keyword, articles.v)
    Ends <- grep(end_keyword, articles.v)
    lengths <- grep(length_keyword, articles.v)
    ### Debug lengths
    # one line before and after length are always empty
    lengths <-  lengths[!(articles.v[lengths+1]!=""|articles.v[lengths-1]!="")]
    #same for Ends
    Ends <-  Ends[!(articles.v[Ends-1]!="")]
    # does language appear in metadata before article
    for (n in 1:min(c(length(Beginnings), length(Ends), length(lengths)))){
      if(Beginnings[n] > Ends[n] & Ends[n] < lengths[n]){Ends <- Ends[-n]}
    }
    # Note: In some rare cases, this will delete articles that do not contain length for other reasons
    if(length(which(Ends[1:(length(lengths))]<lengths))>0) {
      for (n in 1:(length(Beginnings)-length(lengths))){
        # Which Ends are smaller than length? in those cases length is absent and the article gets neglected.
        empty.articles <- which(Ends[1:(length(lengths))]<lengths)
        if(length(empty.articles) > 0){
          Beginnings <-Beginnings[-(empty.articles[1])]
          Ends<-Ends[-(empty.articles[1])]
        }else{
          if(max(lengths)<max(Beginnings)){
            Beginnings <- Beginnings[-which.max(Beginnings)]
            Ends <- Ends[-which.max(Ends)]
          }
        }
      }
    }

    # range
    range.v <- articles.v[grep("^Download Request|^Ausgabeauftrag:", articles.v)]
    range.v <- unlist(strsplit(range.v, "-"))
    range.v <- gsub("[[:alpha:]]|[[:punct:]]|\\s+", "", range.v)
    range.v <- as.numeric(range.v[2])-as.numeric(range.v[1])+1
    out <- data.frame(file = i,
                      Beginnings = length(Beginnings),
                      Ends = length(Ends),
                      Lengths = length(lengths),
                      range = range.v,
                      Test1 = length(Beginnings) == length(Ends),
                      Test2 = length(Beginnings) == length(lengths),
                      Test3 = length(Beginnings) == range.v,
                      stringsAsFactors = FALSE)
    out
  })
  out <- data.table::rbindlist(out)

  if(verbose){cat("\nElapsed time: ", format((Sys.time()-start.time), digits = 2, nsmall = 2), ". ", sep = "")}
  cat(sum(!out$Test1) + sum(!out$Test2), "files with problem(s).\n")
  return(out)
}
