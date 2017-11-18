#' Assign proper names to LexisNexis TXT files
#'
#' Give proper names to LN txt files based on search term and period retrieved from each files cover page.
#' @param x Name or names of LexisNexis TXT file to be renamed or a folder location. If folder location is provided, string must end with /
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8 (the LexisNexis standard value).
#' @param recursive A logical flag indicating whether sub directories are searched for mor txt files
#' @param report A logical flag indicating whether the function will return a report which files were renamed
#' @param verbose A logical flag indicating whether information should be printed to the screen.
#' @keywords LexisNexis
#' @details Can check consistency of LexisNexis txt files. read_LN needs at least Beginning, End and length in each article to work
#' @author Johannes B. Gruber
#' @export
#' @examples 

 
rename_LNfiles <- function(x, encoding = "UTF-8", recursive = TRUE, report = FALSE, verbose = TRUE){
  ###' Track the time
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
  for(i in 1:length(files)){
    #read in the articles
    content.v <- stringi::stri_read_lines(files[i], encoding = encoding)
    #look for the range of articles
    range.v <- content.v[grep("^Download Request:|^Ausgabeauftrag: Dokument", content.v)]
    # extract the actual range infromation from line
    range.v <- stringi::stri_extract_all_regex(range.v, pattern = "[[:digit:]]|-", simplify = TRUE)
    range.v <- stringi::stri_join(range.v, sep = "", collapse = "")
    
    # look for search term
    term.v <- content.v[grep("^Terms: |^Begriffe: ", content.v)]
    #erase everything in the line exept the actual range
    term.v <- gsub("^Terms: |^Begriffe: ", "", term.v)
    term.v <- unlist(strsplit(term.v, split = " and | OR ", fixed = FALSE)) #splits term into elemets seprated by and or OR
    date.v <- gsub("[^[:digit:]]", "", term.v[1]) # create from start date with everything but numbers (first element is start date)
    date.v <- paste0(date.v, "-", gsub("[^[:digit:]]", "", term.v[2]))
    term.v <-  gsub("[^[:alpha:]]", "",term.v[3]) #extract first search term
    
    file.name <- sub("[^/]+$","",files[i]) #take old filepath
    file.name <- paste0(file.name, term.v, "_", date.v, "_",range.v,".txt")
    if(!exists("renamed")){renamed = data.frame(name.orig = files, 
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
