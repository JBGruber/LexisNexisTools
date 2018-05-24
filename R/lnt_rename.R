#' Assign proper names to LexisNexis TXT files
#'
#' Give proper names to LN TXT files based on search term and period retrieved
#' from each file cover page. This information is not always delivered by
#' LexisNexis though. If the information is not present in the file, new file
#' names will be empty.
#'
#' Warning: This will rename all txt files in a give folder.
#'
#' @param x Can be either a character vector of LexisNexis TXT file name(s),
#'   folder name(s) or can be left blank (see example).
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8
#'   (the LexisNexis standard value).
#' @param recursive A logical flag indicating whether subdirectories are
#'   searched for more txt files.
#' @param report A logical flag indicating whether the function will return a
#'   report which files were renamed.
#' @param simulate Should the renaming be simulated instead of actually done?
#'   This can help prevent accidental renaming of unrelated txt files which
#'   happen to be in the same directory as the files from 'LexisNexsis'.
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
#'  \dontrun{report.df <- lnt_rename(recursive = FALSE,
#'                         report = TRUE)}
#'
#' # Or provide file name(s)
#' my_files<-list.files(pattern = ".txt", full.names = TRUE,
#'                      recursive = TRUE, ignore.case = TRUE)
#' report.df <- lnt_rename(x = my_files,
#'                         recursive = FALSE,
#'                         report = TRUE)
#'
#' # Or provide folder name(s)
#' report.df <- lnt_rename(x = getwd())
#'
#' report.df
lnt_rename <- function(x, 
                       encoding = "UTF-8", 
                       recursive = FALSE, 
                       report = FALSE,
                       simulate = TRUE,
                       verbose = TRUE) {
  # Check how files are provided
  # 1. nothing (search wd)
  # 2. txt file or files
  # 3. folder name(s)
  if (missing(x)) {
    if (readline(prompt="No path was given. Should files in working direcotry be renamed? [y/n]") 
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
  # Track the time
  if(verbose) {start.time <- Sys.time(); cat("Checking LN files...\n")}
  files <- unique(files)
  if (verbose){start.time <- Sys.time(); cat(length(files), "files found to process...\n")}
  renamed <- data.frame(name.orig = files,
                        name.new = character(length = length(files)),
                        status = character(length = length(files)),
                        stringsAsFactors = FALSE)
  # start renaming files
  for(i in seq_len(length(files))){
    #read in the articles
    content.v <- readLines(files[i], encoding = encoding, n =50)
    #look for the range of articles
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
      date.v <- paste0(gsub("[^[:digit:]]", 
                            "",
                            term.v[1]),
                       "-",
                       gsub("[^[:digit:]]", 
                            "",
                            term.v[2]))
      term.v <-  gsub("[^[:alpha:]]", "", term.v[3])
    } else if (length(date.v) > 0) {
      date.v <- gsub("[^[:digit:]]", 
                     "",
                     term.v)
      term.v <-  gsub("[^[:alpha:]]", "", term.v[2])
    } else {
      date.v <- "NA"
      term.v <-  gsub("[^[:alpha:]]", "", term.v)
    }
    file.name <- sub("[^/]+$", "", files[i]) #take old filepath
    file.name <- paste0(file.name, term.v, "_", date.v, "_",range.v,".txt")
    #rename file
    if (!simulate) {
      if(file.exists(file.name)){ #file already exists
        renamed$name.new[i] <- renamed$name.orig[i]
        renamed$status[i] <- "not renamed (file exists)"
      }else{
        if(file.name=="__.txt"){ #file name is empty
          renamed$name.new[i] <- file.name
          renamed$status[i] <- "not renamed (file is empty)"
        }else{
          renamed$name.new[i] <- file.name
          renamed$status[i] <- "renamed"
          file.rename(files[i], file.name) #rename
        }
      }
      if (verbose) cat("\r\t...renaming files", scales::percent(i/length(files)), "\t\t")
    }
  }
  if (verbose) {
    cat("\n", sum(grepl("^renamed$", renamed$status)), "files renamed, ")
    if(sum(grepl("exists", renamed$status, fixed = TRUE)) > 0) {
      cat(sum(grepl("exists", renamed$status, fixed = TRUE)), "not renamed (file already exists), ")
    }
    if(sum(grepl("empty", renamed$status, fixed = TRUE)) > 0) {
      cat(sum(grepl("empty", renamed$status, fixed = TRUE)), "not renamed (no search term or time range found), ")
    }
  }
  renamed$status <- as.factor(renamed$status)
  if (verbose) cat("in", format((Sys.time()-start.time), digits = 2, nsmall = 2), if(simulate) "[cahnges only simulated]")
  if(report) renamed
}
