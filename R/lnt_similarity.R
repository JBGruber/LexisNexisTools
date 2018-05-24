#' Check for highly similar articles.
#'
#' Check for highly similar articles by comparing all articles published on the
#' same date.
#' @param texts Provide texts to check for similarity.
#' @param dates Provide corresponding dates, same length as \code{text}.
#' @param LNToutput Alternatively to providing texts an dates individually, you can
#'   provide a LNToutput object.
#' @param IDs IDs of articles.
#' @param threshold At which threshold of similarity is an article considered a
#'   duplicate.
#' @param rel_dist Calculate the relative Levenshtein distance between two
#'   articles if set to TRUE (can take very long). The main difference between
#'   the similarity and distance value is that the distance takes word order
#'   into account while similarity employs the bag of words approach.
#' @keywords similarity
#' @details The function produces a data.frame consisting of information about
#'   duplicated articles
#' @author Johannes B. Gruber
#' @export
#' @importFrom utils adist
#' @importFrom stringdist stringdist
#' @importFrom quanteda dfm textstat_simil
#' @examples
#' # Copy sample file to current wd
#' lnt_sample()
#'
#' # Convert raw file to LNToutput object
#' LNToutput <- lnt_read(lnt_sample())
#'
#' # Test similarity of articles
#' duplicates.df <- lnt_similarity(texts = LNToutput@articles$Article,
#'                                dates = LNToutput@meta$Date,
#'                                IDs = LNToutput@articles$ID)
#'
#' # Create three separate data.frames from cleaned LNToutput object
#' meta.df <- LNToutput@meta[!LNToutput@meta$ID %in%
#'                            duplicates.df$ID_duplicate,]
#' articles.df <- LNToutput@articles[!LNToutput@articles$ID %in%
#'                                    duplicates.df$ID_duplicate,]
#' paragraphs.df <- LNToutput@paragraphs[!LNToutput@paragraphs$ID %in%
#'                                       duplicates.df$ID_duplicate,]
lnt_similarity <- function(texts,
                           dates,
                           LNToutput,
                           IDs = NULL,
                           threshold = 0.99,
                           rel_dist = TRUE) {
  if (any(missing(texts), missing(dates))) {
    texts <- LNToutput@articles$Article
    dates <- LNToutput@meta$Date
    if (is.null(IDs)) {
      IDs = LNToutput@articles$ID
    }
  } else {
    if (missing(LNToutput)) {
      LNToutput <- character()
    }
  }
  start.time <- Sys.time()
  #first, we need to unique days so we can loop through them
  dates.d <- unique(dates)
  dates.d <- dates.d[order(dates.d)]
  
  text.dfm <- quanteda::dfm(texts,
                            tolower = TRUE,
                            remove = "[^[:alnum:]]",
                            valuetype = "regex",
                            verbose = FALSE)
  if(is.null(IDs)) IDs <- seq_len(length(texts))
  text.dfm@Dimnames$docs <- IDs
  
  
  duplicates.df <- lapply(dates.d, function(x){
    if (length(grep(x, dates)) > 1) {
      sim <- as.matrix(quanteda::textstat_simil(text.dfm[text.dfm@Dimnames$docs %in%
                                                           IDs[grep(x, dates)]], 
                                                selection = NULL , 
                                                method = "correlation",
                                                margin="documents"))
      diag(sim) <- 0
      dup <- as.data.frame(which(sim > threshold, arr.ind = TRUE), check.names = FALSE)
      # every pair of duplicates is in here twice. This doesn't make sense of course so let's get rid of them
      if (nrow(dup) > 0){
        dup[, 3] <- dup[, 1] + dup[, 2]
        dup <- dup[!duplicated(dup[, 3]), ]
        dup <- dup[, -3]
        # now the row/colnumbers are replaced by article IDs (which were stored as docnames)
        dup[, 1] <- sapply(seq_len(nrow(dup)), function(i) dup[i, 1] <- rownames(sim)[dup[i, 1]])
        dup[, 2] <- sapply(seq_len(nrow(dup)), function(i) dup[i, 2] <- rownames(sim)[dup[i, 2]])
        #now we can store duplicates along orignil, similarity and ID in data frame
        duplicates.df <- data.frame(Date = rep(x, nrow(dup)),
                                    ID_original = dup[, 2],
                                    Original = texts[match(dup[, 2], IDs)],
                                    ID_duplicate = dup[, 1],
                                    Duplicate = texts[match(dup[, 1],IDs)],
                                    Similarity = sim[as.matrix(dup)],
                                    stringsAsFactors = FALSE)
        # additionally to the similarity the relative distance (original text from duplicate divided by character
        #length of longer text) can be added
        if(rel_dist){
          duplicates.df$rel_dist <- sapply(seq_len(nrow(dup)), function(i) { 
            adist(texts[match(dup[i,2],IDs)],texts[match(dup[i,1],IDs)], ignore.case = TRUE)
            stringdist(a = texts[match(dup[i,2],IDs)], 
                       b = texts[match(dup[i,1],IDs)], 
                       method = "lv", 
                       useBytes = FALSE,
                       nthread = getOption("sd_num_thread")) /# string distance
              max(c(nchar(texts[match(dup[i,2],IDs)]), nchar(texts[match(dup[i,1],IDs)]))) # by length of string
          })
        }
        cat("\rProcessing date ", as.character(x)," ... ", nrow(dup), " duplicates found \t\t", sep="")
        duplicates.df
      } else {
        cat("\rProcessing date ", as.character(x)," ... 0 duplicates found \t\t", sep="")
      }  
    } else {
      cat("\rProcessing date ", as.character(x)," ... 0 duplicates found \t\t", sep="")
    }
  })
  #end loop
  duplicates.df <- as.data.frame(data.table::rbindlist(duplicates.df))
  if(rel_dist) {colnames(duplicates.df)[7] <- "rel_dist"}
  end.time <- Sys.time()
  time.elapsed <- end.time - start.time
  cat("\rThreshold = ", threshold, "; ",
      length(dates.d), " days processed; ",
      nrow(duplicates.df[unique(duplicates.df$ID_duplicate),]), " duplicates found;",
      " in ", format(time.elapsed, digits = 2, nsmall = 2), sep = "")
  return(duplicates.df)
}
