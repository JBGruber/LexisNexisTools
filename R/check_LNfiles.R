#' Check LexisNexis TXT files
#'
#' Read a LexisNexis TXT file and convert it to a data frame.
#' @param x Name or names of LexisNexis TXT file to be converted.
#' @param encoding Encoding to be assumed for input files. Defaults to UTF-8 (the LexisNexis standard value).
#' @param verbose A logical flag indicating whether information should be printed to the screen.
#' @keywords LexisNexis
#' @details Can check consistency of LexisNexis txt files. read_LN needs at least Beginning, End and length in each article to work
#' @author Johannes B. Gruber
#' @export
#' @examples 

 
check_LNfiles <- function(x, encoding = "UTF-8", verbose = TRUE){
  ###' Track the time
  if(verbose){start.time <- Sys.time(); cat("Checking LN files...\n")}
  
  ### read in file
  out <- lapply(x, function(i){
    if(verbose){cat("\r\tChecking file",i,"...")}
    articles.v <- stringi::stri_read_lines(i, encoding = encoding)
    Beginnings <- grep("\\d+ of \\d+ DOCUMENTS$| Dokument \\d+ von \\d+$", articles.v)
    Ends <- grep("^LANGUAGE: ENGLISH|^SPRACHE: ", articles.v)
    lengths <- grep("^LENGTH: |^LÄNGE: ", articles.v)
    ### Debug lengths
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
    
    if(length(which(articles.v[lengths+1]!=""|articles.v[lengths-1]!=""))>0) {
      lengths.v <- lengths.v[-(which(articles.v[lengths+1]!=""|articles.v[lengths-1]!=""))]
      lengths <-  lengths[-(which(articles.v[lengths+1]!=""|articles.v[lengths-1]!=""))]
    }
    out <- data.frame(file = x,
                      Beginnings = length(Beginnings),
                      Ends = length(Ends),
                      Lengths = length(lengths),
                      Test1 = length(Beginnings) == length(Ends),
                      Test2 = length(Beginnings) == length(lengths),
                      stringsAsFactors = FALSE)
    out
  })
  out <- data.table::rbindlist(out)

  if(verbose){cat("\nElapsed time: ", format((Sys.time()-start.time), digits = 2, nsmall = 2),"\n", sep = "")}
  out
}
