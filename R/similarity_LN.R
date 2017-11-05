#' Check for higly similar articles.
#'
#' Check for higly similar articles by comparing all articles published on the same date.
#' @param texts column with articles.
#' @param dates column with dates.
#' @param IDs IDs of articles.
#' @param threshold At which threshold of similarity is an article considered a duplicate. 
#' @param Rel.diff.on Calculate the relative Levenstein distance between two articles if set to TRUE (can take very long).
#' @keywords similarity
#' @details The function produces a data.frame consisting of information about duplicated articles
#' @author Johannes B. Gruber
#' @export
#' @examples 
#' duplicates.df <- similarity_LN(texts = LNoutput@articles$Article, dates = LNoutput@meta$Date, IDs = LNoutput@articles$ID)
#' meta.df <- LNoutput@meta[!LNoutput@meta$ID %in% duplicates.df$ID.duplicate,]
#' articles.df <- LNoutput@articles[!LNoutput@articles$ID %in% duplicates.df$ID.duplicate,]
#' paragraphs.df <- LNoutput@paragraphs[!LNoutput@paragraphs$ID %in% duplicates.df$ID.duplicate,]
 
similarity_LN <- function(texts, dates, IDs = NULL, threshold = 0.99, Rel.diff.on=FALSE) {
  start.time <- Sys.time()
  #first, we need to unique days so we can loop through them
  dates.d <-unique(dates)
  dates.d <-dates.d[order(dates.d)]
  duplicates.df <- lapply(dates.d ,FUN=function(dates.d){
    # convert to quanteda dfm
    text.dfm <- dfm(texts[grep(dates.d, dates)], verbose = F, tolower = TRUE)
    if(is.null(IDs)){IDs <- 1:length(texts)}
    text.dfm@Dimnames$docs <- IDs[grep(dates.d, dates)]
    #docnames(text.dfm)
    sample_sim <- as.matrix(textstat_simil(text.dfm, selection=NULL , method = "correlation",
                                           margin="documents"))
    
    # which values are bigger than test_sim but smaller 1 (direct duplicates are already gone)
    dup <- as.data.frame(which(sample_sim > threshold & sample_sim < 1, arr.ind=TRUE))
    # every pair of duplicates is in here twice. This doesn't make sense of course so let's get rid of them
    if (nrow(dup)>0){
      dup[,3] <- dup[,1]+dup[,2]
      dup <- dup[(which(duplicated(dup[,3])==FALSE)),]
      dup <- dup[,-3]
      # now the row/colnumbers are replaced by article IDs (which were stored as docnames)
      dup[,1] <- sapply(1:nrow(dup), function(i) dup[i,1] <- rownames(sample_sim)[dup[i,1]])
      dup[,2] <- sapply(1:nrow(dup), function(i) dup[i,2] <- rownames(sample_sim)[dup[i,2]])
      #now we can store duplicates along orignila, similarity and ID in data frame  
      
      duplicates.df <- data.frame(Date = rep(dates.d, nrow(dup)),
                                  ID.original=dup[,2], 
                                  Original=texts[match(dup[,2],IDs)], 
                                  ID.duplicate=dup[,1], 
                                  Duplicate=texts[match(dup[,1],IDs)],
                                  Similarity=sample_sim[as.matrix(dup)],stringsAsFactors=FALSE)
      # additionally to the similarity the relative distance (original text from duplicate divided by character 
      #length of longer text) can be added
      if(Rel.diff.on){
        duplicates.df$Rel.diff. <- sapply(1:nrow(dup), function(i) adist(texts[match(dup[i,2],IDs)],texts[match(dup[i,1],IDs)], ignore.case = TRUE)/# string distance
                                            max(c(nchar(texts[match(dup[i,2],IDs)]), nchar(texts[match(dup[i,1],IDs)]))))
      }
      
      
      cat("Processing date ", as.character(dates.d)," ... ", nrow(dup), " duplicates found", "\n", sep="")
      duplicates.df
    }
    else
    {cat("Processing date ", as.character(dates.d)," ... 0 duplicates found", "\n", sep="")}
  })
  #end loop
  duplicates.df <- as.data.frame(rbindlist(duplicates.df))
  if(Rel.diff.on){colnames(duplicates.df)[7] <- "Rel.diff."}
  end.time <- Sys.time()
  time.elapsed <- end.time - start.time
    cat(threshold, length(dates.d), "days processed...\n",nrow(duplicates.df[unique(duplicates.df$ID.duplicate)]), "duplicates found...\n", "In", format(time.elapsed, digits = 2, nsmall = 2))

  return(duplicates.df)
}
