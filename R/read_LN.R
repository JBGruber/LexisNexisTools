

#' make S4 object
setClass("LNoutput", 
         representation(meta = "data.frame", articles = "data.frame", paragraphs = "data.frame"))



#' Read in a LexisNexis TXT file
#'
#' Read a LexisNexis TXT file and convert it to a data frame.
#' @param x Name or names of LexisNexis TXT file to be converted.
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8 (the LexisNexis standard value).
#' @param verbose A logical flag indicating whether information should be printed to the screen.
#' @param saveevery An integer determining after which number of file reads an interim result is saved as RDS file in the working directory. 
#' @param extractParagraphs A logical flag indicating if the returened object will include a third data frame with paragrahs.
#' @param convertDate A logical flag indicating if it should be tried to convert the date of each article into Date format. Fails for non standard dates provided by LexisNexis.
#' @keywords LexisNexis
#' @details The function can produce a LNoutput S4 object with two data.frame: meta, containing all meta information such as date, author and headline and articles, containing just the article ID and the text of the articles. When extractParagraphs is set to TRUE, the output contains a third data.frame, similar to articles but with articles split into paragraphs.
#' @author Johannes B. Gruber
#' @export
#' @examples 
#' LNoutput <- read_LN("myNexisDownload.txt")
#' meta.df <- LNoutput@meta
#' articles.df <- LNoutput@articles
#' paragraphs.df <- LNoutput@paragraphs
 
read_LN <- function(x, encoding = "UTF-8", verbose = TRUE, extractParagraphs=TRUE, convertDate = FALSE){
  ###' Track the time
  if(verbose){start.time <- Sys.time(); cat("Creating LNoutput from a connection input...\n")}
  
  ### read in file
  if(length(x)>1){
    articles.v <- unlist(sapply(x, stringi::stri_read_lines, encoding = encoding))
  } else {
    articles.v <- stringi::stri_read_lines(x, encoding = encoding)
  }
  if(verbose){cat("\t...files loaded [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  
  #exclude some lines
  articles.v[grep("^LOAD-DATE: |^UPDATE: |^GRAFIK: |^GRAPHIC: ", articles.v)]<-""
  
  ### Find the beginning of each article marked by the expression "Dokument * von *", e.g. "21 of 500 DOCUMENTS"
  Beginnings <- grep("\\d+ of \\d+ DOCUMENTS$| Dokument \\d+ von \\d+$", articles.v)
  
  ### Find ends. Language is the last line of the article; use this to mark the end of an article
  Ends <- grep("^LANGUAGE: |^SPRACHE: ", articles.v)
  
  ### Debug Beginnings and Ends
  # if the lines before or after Ends and Beginnings are not empty, the keyword is a coincidence
  if(length(which(articles.v[Ends+1]!=""|articles.v[Ends-1]!=""))>0) {
    Ends <- Ends[-(which(articles.v[Ends+1]!=""|articles.v[Ends-1]!=""))]
  }
  if(length(which(articles.v[Beginnings+1]!=""|articles.v[Beginnings-1]!=""))>0) {
    Beginnings <- Beginnings[-(which(articles.v[Beginnings+1]!=""|articles.v[Beginnings-1]!=""))]
  }
  if(!length(Beginnings)==length(Ends)){cat("Warning: Beginnings and ends do not match\n")}
  
  ### Find lengths. Length is the last line of meta information before the article starts
  lengths <- grep("^LENGTH: |^LÄNGE: ", articles.v)
  
  
  ### Debug lengths
  # one line before and after length are always empty
  lengths <-  lengths[!(articles.v[lengths+1]!=""|articles.v[lengths-1]!="")]
  #same for Ends
  #Ends <-  Ends[!(articles.v[Ends+1]!=""|articles.v[Ends-1]!="")]
  # Note: In some rare cases, this will delete articles that do not contain length for other reasons
  if(length(which(Ends[1:(length(lengths))]<lengths))>0) {
    for (i in 1:(length(Beginnings)-length(lengths))){
      # Which Ends are smaller than length? in those cases length is absent and the article gets neglected.
      empty.articles <- which(Ends[1:(length(lengths))]<lengths)
      Beginnings<-Beginnings[-(empty.articles[1]-1)]
      Ends<-Ends[-(empty.articles[1]-1)]
      rm(empty.articles)
    }
  }
  
  if(!length(Beginnings)==length(lengths)){cat("Warning: Missing or extra instances of Length\n")}
  
  ### get lengths for meta information
  lengths.v <- articles.v[lengths]
  lengths.v <- gsub("LENGTH: |LÄNGE:|Wörter|words|\\s+", "", lengths.v)
  
  ### Source file
  if(length(x)>1){
    source.v <- gsub(".txt\\d+$","",names(articles.v[Beginnings]), ignore.case = TRUE)
  } else {
    source.v <- rep(x, times=length(Beginnings))
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
  
  #The Date is always shown two lines after newspaper
  #To bring this in a more useful form
  dates.v<- gsub("Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag",
                 "", articles.v[Newspaper+2])
  
  
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
  edition.v <- sapply(1:length(Beginnings), function(i){
    edition.v <- articles.v[(Newspaper[i]+3):(lengths[i]-1)]
    empties <- grep("^$", edition.v)
    edition.v <- paste(edition.v[1:(empties[1]-1)], collapse = " ")
  })
  edition.v <- gsub("^\\s+|\\s+$", "", edition.v) 
  
  ### Headline 
  headlines.l <- lapply(1:length(Beginnings), function(i){
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
  meta.df <- data.frame(ID = 1:length(Beginnings),
                        Source.File = source.v,
                        Newspaper = newspaper.v,
                        Date = dates.v,
                        Length = lengths.v,
                        Section = section.v,
                        Author = author.v,
                        Edition = edition.v,
                        Headline = unlist(headlines.l),
                        row.names = 1:length(Beginnings),
                        stringsAsFactors = FALSE)
  if(convertDate){
    meta.df$Date <- gsub('EDITION[a-zA-Z0-9]$', '', meta.df$Date)
    meta.df$Date <- gsub('[^0-9,.]', '', meta.df$Date)
    meta.df$Date <- gsub('[[:punct:]]$', '', meta.df$Date)
    # And finally convert to date
    meta.df$Date <- as.Date(meta.df$Date, "%m.%d.%Y")}
  if(verbose){cat("\t...meta extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  ### Article
  articles.df <- data.frame(ID = 1:length(Beginnings),
                            Article = sapply(1:length(Beginnings), function(i){
                              out <- paste(articles.v[(lengths[i]+1):(Ends[i]-1)], collapse=" ")
                              #cat("\r","working on article",i,"of",length(Beginnings))
                              out <- gsub("^\\s+|\\s+$", "", out)
                              #double blanks
                              out <- gsub("\\s+", " ", out)
                              out
                            }),
                            stringsAsFactors = FALSE)
  if(verbose){cat("\t...articles extracted [", format((Sys.time()-start.time), digits = 2, nsmall = 2),"]\n", sep = "")}
  
  if(extractParagraphs){
    paragraphs.df <- lapply(1:length(Beginnings), function(i){
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
  
  out <- new("LNoutput", meta = meta.df, articles = articles.df, paragraphs = paragraphs.df)
  
  if(verbose){cat("Elapsed time: ", format((Sys.time()-start.time), digits = 2, nsmall = 2),"\n", sep = "")}
  out
}
