

#' make S4 object
#' @noRd
#' @importFrom methods new
setClass("LNToutput",
         representation(meta = "data.frame", articles = "data.frame", paragraphs = "data.frame"))


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
            cat("\n\nMeta (6 of ", nrow(object@meta),"):\n", sep = "")
            print(meta)
            cat("\n\nArticles (6 of ", nrow(object@articles),"):\n", sep = "")
            print(articles)
            cat("\n\nParagraphs (6 of ", nrow(object@paragraphs),"):\n", sep = "")
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
#' @importFrom stringi stri_read_lines
lnt_read <- function(x,
                     encoding = "UTF-8",
                     extract_paragraphs = TRUE,
                     convert_date = TRUE,
                     start_keyword = "auto",
                     end_keyword = "auto",
                     length_keyword = "^LENGTH: |^L\u00c4NGE: |^LONGUEUR: ",
                     recursive = FALSE,
                     verbose = TRUE,
                     ...){
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
  if(verbose){start.time <- Sys.time(); cat("Creating LNToutput from a connection input...\n")}
  
  ### read in file
  if(length(files) > 1){
    articles.v <- unlist(lapply(files, function(f) {
      out <- stringi::stri_read_lines(f, encoding = encoding)
      names(out) <- rep(f, times = length(out))
      out
    }))
  } else {
    articles.v <- stringi::stri_read_lines(files, encoding = encoding)
    names(articles.v) <- rep(files, times = length(articles.v))
  }
  if(verbose){cat("\t...files loaded [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  
  #exclude some lines
  articles.v[grep("^LOAD-DATE: |^UPDATE: |^GRAFIK: |^GRAPHIC: |^DATELINE: ", articles.v)]<-""
  
  ### Find the beginning of each article marked by the expression "Dokument * von *", e.g. "21 of 500 DOCUMENTS"
  Beginnings <- grep(start_keyword, articles.v)
  
  ### Find ends. Language is the last line of the article; use this to mark the end of an article
  Ends <- grep(end_keyword, articles.v)
  
  ### Debug Beginnings and Ends
  if(!length(Beginnings)==length(Ends)){
    warning("Beginnings and ends do not match. Fall back on safer approach.")
    Ends <- c(Beginnings[2:length(Beginnings)] - 2, length(articles.v))
  }
  
  ### Find lengths. Length is the last line of meta information before the article starts
  lengths <- grep(length_keyword, articles.v)
  
  
  ### Debug lengths
  # one line before and after length are always empty
  lengths <-  lengths[!(articles.v[lengths+1]!=""|articles.v[lengths-1]!="")]
  #same for Ends
  Ends <-  Ends[!(articles.v[Ends-1]!="")]
  # #does "Language:" appear at beginning of headline
  # for (n in 1:min(c(length(Beginnings), length(Ends), length(lengths)))){
  #   if(Beginnings[n] > Ends[n] & Ends[n] < lengths[n]){Ends <- Ends[-n]}
  # }
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
  
  if(!length(Beginnings)==length(lengths)){cat("Warning: Missing or extra instances of Length\n")}
  
  ### get lengths for meta information
  lengths.v <- articles.v[lengths]
  lengths.v <- gsub("LENGTH: |L\u00e4NGE:|W\u00f6rter|words|\\s+", "", lengths.v)
  
  ### Source file
  if(length(files)>1){
    source.v <- gsub(".txt\\d+$","",names(articles.v[Beginnings]), ignore.case = TRUE)
  } else {
    source.v <- rep(files, times = length(Beginnings))
  }
  
  
  ### Newspaper
  Newspaper <- Beginnings+2
  # if Newspaper is an empty line we look ahead up to 5 lines to see if we find the newspaper name there
  for(i in 1:5){
    Newspaper <- ifelse(articles.v[Newspaper]=="",Newspaper+1,Newspaper)
  }
  newspaper.v<-articles.v[Newspaper]
  
  #remove if newspaper.v contains Date or Beginning
  newspaper.v[grep("January|February|March|April|May|June|July|August|September|October|November|December", newspaper.v)] <- ""
  newspaper.v[grep("\\d+ of \\d+ DOCUMENTS$| Dokument \\d+ von \\d+$", newspaper.v)] <- ""
  newspaper.v<-gsub("^\\s+|\\s+$", "", newspaper.v)
  
  ### Date
  dates.v <- ifelse(grepl("http://|https://", articles.v[Newspaper+2]), 
                    articles.v[Newspaper+4],
                    articles.v[Newspaper+2])
  
  
  ### Author (where available)
  author.v <- articles.v[lengths-4]
  # however, not every articles has this information and where it is not available the field should stay blank
  author.v <- ifelse(grepl("AUTOR: |Von|BYLINE: ",author.v),author.v,"")
  author.v <- gsub("AUTOR: |VON |BYLINE: ", "", author.v)
  
  ### section (where available)
  section.v <- articles.v[lengths-2]
  section.v <- ifelse(grepl("SECTION: |RUBRIK: ",section.v), section.v,"")
  section.v<- gsub("SECTION: |RUBRIK: ", "", section.v)
  
  ### edition (where available)
  edition.v <- sapply(seq_len(length(Beginnings)), function(i){
    edition.v <- articles.v[(Newspaper[i]+3):(lengths[i]-1)]
    empties <- grep("^$", edition.v)
    edition.v <- paste(edition.v[1:(empties[1]-1)], collapse = " ")
  })
  edition.v <- gsub("^\\s+|\\s+$", "", edition.v)
  
  ### Headline
  headlines.l <- lapply(seq_len(length(Beginnings)), function(i){
    start <- Newspaper[i]+3
    end <- lengths[i]-1
    headline.v <- articles.v[start:end]
    headline.v <- headline.v[grep("^$", headline.v)[1]:length(headline.v)] #cut everything before the first empty line
    # Now we also grabbed Byline and section where they appear. If we get rid of that what is left
    # is the headline
    headline.v[grep("^BYLINE:|^SECTION:|^RUBRIK:|^AUTOR:", headline.v)]<-""
    headline.v <- paste(headline.v, collapse=" ")
    headline.v
    
  })
  headlines.l <- gsub("^\\s+|\\s+$", "", headlines.l)
  headlines.l <- gsub("\\s+", " ", headlines.l)
  meta.df <- data.frame(ID = seq_len(length(Beginnings)),
                        Source_File = source.v,
                        Newspaper = newspaper.v,
                        Date = dates.v,
                        Length = lengths.v,
                        Section = section.v,
                        Author = author.v,
                        Edition = edition.v,
                        Headline = unlist(headlines.l),
                        row_names = seq_len(length(Beginnings)),
                        stringsAsFactors = FALSE)
  if(convert_date){
    meta.df$Date <- lnt_asDate(meta.df$Date, ...)
  }
  if(verbose){cat("\t...meta extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  ### Article
  articles.df <- data.frame(ID = seq_len(length(Beginnings)),
                            Article = sapply(seq_len(length(Beginnings)), function(i){
                              out <- paste(articles.v[(lengths[i]+1):(Ends[i]-1)], collapse=" ")
                              #cat("\r","working on article",i,"of",length(Beginnings))
                              out <- gsub("^\\s+|\\s+$", "", out)
                              #double blanks
                              out <- gsub("\\s+", " ", out)
                              out
                            }),
                            stringsAsFactors = FALSE)
  if(verbose){cat("\t...articles extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  
  if(extract_paragraphs){
    paragraphs.df <- lapply(seq_len(length(Beginnings)), function(i){
      article.lines.v <- articles.v[(lengths[i]+1):(Ends[i]-1)]
      empties <- grep("^$", article.lines.v)
      empties <- empties[-which(diff(empties)==1)]
      pars <- ifelse(length(empties)>1,length(empties)-1,1)
      out <- data.frame(Art_ID = as.integer(rep(i, times = pars)),
                        Par_ID = 1:pars,
                        Paragraph = NA,
                        stringsAsFactors = FALSE)
      if(length(empties)>1) {
        out$Paragraph <- sapply(1:pars, function(j){
          out <- paste(article.lines.v[(empties[j]+1):(empties[j+1]-1)], collapse=" ")
          out <- gsub("^\\s+|\\s+$", "", out)
          #double blanks
          out <- gsub("\\s+", " ", out)
          out
        })
      } else {
        out$Paragraph <- paste(article.lines.v, collapse=" ")
      }
      out
    })
    #relabel IDs
    for(i in 1:(length(paragraphs.df)-1)){
      paragraphs.df[[i+1]]$Par_ID <- paragraphs.df[[i+1]]$Par_ID + max(paragraphs.df[[i]]$Par_ID)
    }
    paragraphs.df <- data.table::rbindlist(paragraphs.df)
    if(verbose){cat("\t...paragraphs extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  }else{
    paragraphs.df <- data.frame(Art_ID = NA,
                                Par_ID = NA,
                                Paragraph = NA,
                                stringsAsFactors = FALSE)
  }
  
  out <- new("LNToutput", meta = meta.df, articles = articles.df, paragraphs = paragraphs.df)
  
  if(verbose){cat("Elapsed time: ", format((Sys.time()-start.time), digits = 2, nsmall = 2),"\n", sep = "")}
  out
}
