## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library("kableExtra")

## ---- message=FALSE-----------------------------------------------------------
library("LexisNexisTools")

## ---- eval=FALSE--------------------------------------------------------------
#  lnt_sample()

## ---- eval=FALSE--------------------------------------------------------------
#  report <- lnt_rename()

## ---- eval=FALSE--------------------------------------------------------------
#  report <- lnt_rename(x = getwd(), report = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  my_files <- list.files(pattern = ".txt", path = getwd(),
#                         full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
#  report <- lnt_rename(x = my_files, report = TRUE)
#  
#  report

## ---- echo=FALSE--------------------------------------------------------------
library(kableExtra)
temp <- paste0(tempfile(), ".TXT")
silent <- file.copy(
  from = system.file("extdata", "sample.TXT", package = "LexisNexisTools"),
  to = temp,
  overwrite = TRUE
)

report <- lnt_rename(x = temp, simulate = FALSE, report = TRUE, verbose = FALSE)
report$name_orig <- "sample.TXT"
newfile <- report$name_new
report$name_new <- basename(report$name_new)
kable(report, format = "markdown")

## ----eval=FALSE---------------------------------------------------------------
#  LNToutput <- lnt_read(x = getwd())

## ----echo=FALSE, message=FALSE------------------------------------------------
LNToutput <- lnt_read(x = newfile)
LNToutput@meta$Source_File <- basename(LNToutput@meta$Source_File)

## ---- eval=FALSE--------------------------------------------------------------
#  meta_df <- LNToutput@meta
#  articles_df <- LNToutput@articles
#  paragraphs_df <- LNToutput@paragraphs
#  
#  # Print meta to get an idea of the data
#  head(meta_df, n = 3)
#  

## ---- echo=FALSE--------------------------------------------------------------
meta_df <- LNToutput@meta
articles_df <- LNToutput@articles
paragraphs_df <- LNToutput@paragraphs

meta_df$Source_File <- basename(meta_df$Source_File)
# Print meta to get an idea of the data
kable(head(meta_df, n = 3), format = "markdown")


## ---- message=FALSE-----------------------------------------------------------
meta_articles_df <- lnt_convert(LNToutput, to = "data.frame")

# Or keep the paragraphs
meta_paragraphs_df <- lnt_convert(LNToutput, to = "data.frame", what = "Paragraphs")

## ----eval=FALSE---------------------------------------------------------------
#  rDNA_docs <- lnt_convert(LNToutput, to = "rDNA")
#  
#  quanteda_corpus <- lnt_convert(LNToutput, to = "quanteda")
#  
#  tCorpus <- lnt_convert(LNToutput, to = "corpustools")
#  
#  tidy <- lnt_convert(LNToutput, to = "tidytext")
#  
#  Corpus <- lnt_convert(LNToutput, to = "tm")
#  
#  dbloc <- lnt_convert(LNToutput, to = "SQLite")

## ---- eval=FALSE--------------------------------------------------------------
#  # Either provide a LNToutput
#  duplicates_df <- lnt_similarity(LNToutput = LNToutput,
#                                  threshold = 0.97)

## ---- results='hide', message=FALSE-------------------------------------------
# Or the important parts separatley
duplicates_df <- lnt_similarity(texts = LNToutput@articles$Article,
                                dates = LNToutput@meta$Date,
                                IDs = LNToutput@articles$ID,
                                threshold = 0.97)


## ---- eval=FALSE--------------------------------------------------------------
#  lnt_diff(duplicates_df, min = 0, max = Inf)

## -----------------------------------------------------------------------------
duplicates_df <- duplicates_df[duplicates_df$rel_dist < 0.2]
LNToutput <- LNToutput[!LNToutput@meta$ID %in% duplicates_df$ID_duplicate, ]

## -----------------------------------------------------------------------------
LNToutput[1, ]

## ---- eval=FALSE--------------------------------------------------------------
#  #' generate new dataframes without highly similar duplicates
#  meta_df <- LNToutput@meta
#  articles_df <- LNToutput@articles
#  paragraphs_df <- LNToutput@paragraphs
#  
#  # Print e.g., meta to see how the data changed
#  head(meta_df, n = 3)

## ---- echo=FALSE--------------------------------------------------------------
meta_df <- LNToutput@meta
articles_df <- LNToutput@articles
paragraphs_df <- LNToutput@paragraphs
kable(head(meta_df, n = 3), format = "markdown")

## -----------------------------------------------------------------------------
lnt_lookup(LNToutput, pattern = "statistical computing")

## -----------------------------------------------------------------------------
LNToutput@meta$stats <- lnt_lookup(LNToutput, pattern = "statistical computing")
LNToutput <- LNToutput[!sapply(LNToutput@meta$stats, is.null), ]
LNToutput

## -----------------------------------------------------------------------------
lnt_lookup(LNToutput, pattern = "stat.*?")

## -----------------------------------------------------------------------------
table(unlist(lnt_lookup(LNToutput, pattern = "stat.+?\\b")))

## ----echo = FALSE-------------------------------------------------------------
unlink("sample.TXT")

