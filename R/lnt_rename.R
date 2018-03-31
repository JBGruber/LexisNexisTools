#' Assign proper names to LexisNexis TXT files
#'
#' Give proper names to LN TXT files based on search term and period retrieved
#' from each file cover page. This information is not always delivered by
#' LexisNexis though. If the information is not present in the file, new file
#' names will be empty.
#' @param x Name or names of LexisNexis TXT file to be renamed or a folder
#'   location. If folder location is provided, string must end with /
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8
#'   (the LexisNexis standard value).
#' @param recursive A logical flag indicating whether subdirectories are
#'   searched for more txt files
#' @param report A logical flag indicating whether the function will return a
#'   report which files were renamed
#' @param verbose A logical flag indicating whether information should be
#'   printed to the screen.
#' @keywords LexisNexis
#' @details Can check the consistency of LexisNexis txt files. lnt_read needs at
#'   least Beginning, End and length in each article to work
#' @author Johannes B. Gruber
#' @export
#' @importFrom stats na.omit
#' @importFrom stringi stri_extract_all_regex stri_join
#' @examples
#' # Copy sample file to current wd
#' lnt_sample()
#' 
#' # Rename files in folder wd and report back if successful
#' report.df <- lnt_rename(x = lnt_sample(),
#'                         recursive = FALSE,
#'                         report = TRUE)
#' report.df
lnt_rename <- function(x, encoding = "UTF-8", recursive = FALSE, report = FALSE, verbose = TRUE){
  if (missing(x)) {
    if (readline(prompt="No path was given. Should files in working direcotry be renamed? [y/n]") 
        %in% c("y", "yes", "Y", "Yes")) {
      x <- paste0(getwd(), "/")
    } else {
      stop("Aborted by user")
    }
  }
  # Track the time
  if(verbose){start.time <- Sys.time(); cat("Checking LN files...\n")}
  
  if(all(grepl(".txt$", x, ignore.case = TRUE))){files <- x}#all instances of x are txt files
  if(all(grepl("/$", x, ignore.case = TRUE))){# x is a path
    files <- list.files(path = x,
                        pattern = ".txt$", 
                        ignore.case = TRUE, 
                        full.names = TRUE,
                        recursive = recursive)
  }
  if(mean(grepl(".txt$", x, ignore.case = TRUE)) > 0 &
     mean(grepl("/$", x, ignore.case = TRUE)) > 0){ # some txt files but also at least one path
    files <- x[grepl(".txt$", x, ignore.case = TRUE)]
    files <- c(files,
               list.files(path = x[!grepl(".txt$", x, ignore.case = TRUE)],
                          pattern = ".txt$", ignore.case = TRUE, full.names = TRUE,
                          recursive = recursive))
  }
  files <- unique(files) # remove duplicated file names
  if(verbose){start.time <- Sys.time(); cat(length(files),"files found to process...\n")}
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
  if (verbose) cat("in", format((Sys.time()-start.time), digits = 2, nsmall = 2))
  if(report) renamed
}
