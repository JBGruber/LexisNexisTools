#' Assign proper names to LexisNexis TXT files
#'
#' Give proper names to LN txt files based on search term and period retrieved
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
#' @examples
#' \dontrun{
#' # rename files in folder "C:/Test/LNTools test/" and report back if successful
#' report.df <- lnt_rename(x = "C:/Test/LNTools test/",
#'                             recursive = TRUE,
#'                             report = TRUE)
#' }

lnt_rename <- function(x, encoding = "UTF-8", recursive = TRUE, report = FALSE, verbose = TRUE){
  # Track the time
  if(verbose){start.time <- Sys.time(); cat("Checking LN files...\n")}
  
  if(all(grepl(".txt$", x, ignore.case = TRUE))){files <- x}#all instances of x are txt files
  if(all(grepl("/$", x, ignore.case = TRUE))){# x is a path
    files <- list.files(path = x,
                        pattern = ".txt$", ignore.case = TRUE, full.names = TRUE,
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
    #erase everything in the line exept the actual range
    term.v <- gsub("^Terms: |^Begriffe: ", "", term.v)
    term.v <- unlist(strsplit(term.v, split = " AND | and | OR ", fixed = FALSE)) #splits term into elemets seprated by and or OR
    date.v <- gsub("[^[:digit:]]", "", grep("date(", term.v, fixed = TRUE, value = TRUE)[1]) # create from start date with everything but numbers (first element is start date)
    date.v <- paste0(date.v, "-", gsub("[^[:digit:]]", "", na.omit(grep("(date(", term.v, fixed = TRUE, value = TRUE)[2])))
    date.v <- gsub("-$", "", date.v)
    term.v <-  gsub("[^[:alpha:]]", "",grep("(date(", term.v, fixed = TRUE, value = TRUE, invert = TRUE)[1]) #extract first search term
    
    file.name <- sub("[^/]+$","",files[i]) #take old filepath
    file.name <- paste0(file.name, term.v, "_", date.v, "_",range.v,".txt")
    if(!exists("renamed")){renamed <- data.frame(name.orig = files,
                                                 name.new = character(length = length(files)),
                                                 status = character(length = length(files)),
                                                 stringsAsFactors = FALSE)}
    
    #rename file
    if(file.exists(file.name)){ #file already exists
      renamed$name.new[i] <- renamed$name.orig[i]
      renamed$status[i] <- 0
    }else{
      if(file.name=="__.txt"){ #file name is empty
        renamed$name.new[i] <- file.name
        renamed$status[i] <- 1
      }else{
        renamed$name.new[i] <- file.name
        renamed$status[i] <- 2
        file.rename(files[i], file.name) #rename
      }
    }
    
    cat("\r\t...renaming files", scales::percent(i/length(files)), "\t\t")
    #1. renamed #, 2. not renamed (does already exist), 3. not renamed (no search term or time range found)
  }
  cat("\n", length(which(renamed$status == 2)), "files renamed, ")
  if(length(which(renamed$status == 0)) > 0){cat(length(which(renamed$status == 0)), "not renamed (file already exists), ")}
  if(length(which(renamed$status == 1)) > 0){cat(length(which(renamed$status == 1)), "not renamed (no search term or time range found), ")}
  cat("in", format((Sys.time()-start.time), digits = 2, nsmall = 2))
  if(report){
    renamed$status[renamed$status==0] <- "not renamed"
    renamed$status[renamed$status==1] <- "not renamed"
    renamed$status[renamed$status==2] <- "renamed"
    renamed}
}
