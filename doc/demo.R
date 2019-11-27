## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library("kableExtra")

## ---- message=FALSE------------------------------------------------------
library("LexisNexisTools")

## ---- eval=FALSE---------------------------------------------------------
#  # For example
#  setwd("C:/Test/LNTools_test")
#  
#  # Or
#  setwd("~/Test/LNTools_test")

## ---- eval=FALSE---------------------------------------------------------
#  lnt_sample()

## ---- eval=FALSE---------------------------------------------------------
#  report <- lnt_rename()

## ---- eval=FALSE---------------------------------------------------------
#  report <- lnt_rename(x = getwd(), report = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  my_files <- list.files(pattern = ".txt", path = getwd(),
#                         full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
#  report <- lnt_rename(x = my_files, report = TRUE)
#  
#  report

## ---- echo=FALSE---------------------------------------------------------
library(kableExtra)
unlink("SampleFile_20091201-20100511_1-10.txt")
report <- lnt_rename(x = lnt_sample(overwrite = TRUE), simulate = FALSE, report = TRUE, verbose = FALSE)
report$name_orig <- basename(report$name_orig)
report$name_new <- basename(report$name_new)
kable(report, format = "markdown")

## ----message=FALSE-------------------------------------------------------
LNToutput <- lnt_read(x = getwd())

## ---- eval=FALSE---------------------------------------------------------
#  meta_df <- LNToutput@meta
#  articles_df <- LNToutput@articles
#  paragraphs_df <- LNToutput@paragraphs
#  
#  # Print meta to get an idea of the data
#  head(meta_df, n = 3)
#  

## ---- echo=FALSE---------------------------------------------------------
meta_df <- LNToutput@meta
articles_df <- LNToutput@articles
paragraphs_df <- LNToutput@paragraphs

meta_df$Source_File <- basename(meta_df$Source_File)
# Print meta to get an idea of the data
kable(head(meta_df, n = 3), format = "markdown")


## ---- message=FALSE------------------------------------------------------
library("dplyr")
meta_articles_df <- meta_df %>%
  right_join(articles_df, by = "ID")

# Or keep the paragraphs
meta_paragraphs_df <- meta_df %>%
  right_join(paragraphs_df, by = c("ID" = "Art_ID"))

## ------------------------------------------------------------------------
quanteda_corpus <- lnt_convert(LNToutput, to = "rDNA")

corpus <- lnt_convert(LNToutput, to = "quanteda")

tCorpus <- lnt_convert(LNToutput, to = "corpustools")

tidy <- lnt_convert(LNToutput, to = "tidytext")

Corpus <- lnt_convert(LNToutput, to = "tm")

dbloc <- lnt_convert(LNToutput, to = "lnt2SQLite")

## ---- eval=FALSE---------------------------------------------------------
#  # Either provide a LNToutput
#  duplicates_df <- lnt_similarity(LNToutput = LNToutput,
#                                  threshold = 0.97)

## ---- results='hide', message=FALSE--------------------------------------
# Or the important parts separatley
duplicates_df <- lnt_similarity(texts = LNToutput@articles$Article,
                                dates = LNToutput@meta$Date,
                                IDs = LNToutput@articles$ID,
                                threshold = 0.97)


## ---- eval=FALSE---------------------------------------------------------
#  lnt_diff(duplicates_df, min = 0, max = Inf)

## ------------------------------------------------------------------------
duplicates_df <- duplicates_df[duplicates_df$rel_dist < 0.2]
LNToutput <- LNToutput[!LNToutput@meta$ID %in% duplicates_df$ID_duplicate, ]

## ------------------------------------------------------------------------
LNToutput[1, ]

## ---- eval=FALSE---------------------------------------------------------
#  #' generate new dataframes without highly similar duplicates
#  meta_df <- LNToutput@meta
#  articles_df <- LNToutput@articles
#  paragraphs_df <- LNToutput@paragraphs
#  
#  # Print e.g., meta to see how the data changed
#  head(meta_df, n = 3)

## ---- echo=FALSE---------------------------------------------------------
meta_df <- LNToutput@meta
articles_df <- LNToutput@articles
paragraphs_df <- LNToutput@paragraphs

kable(head(meta_df, n = 3), format = "markdown")

## ------------------------------------------------------------------------
lnt_lookup(LNToutput, pattern = "statistical computing")

## ------------------------------------------------------------------------
LNToutput@meta$stats <- lnt_lookup(LNToutput, pattern = "statistical computing")
LNToutput <- LNToutput[!sapply(LNToutput@meta$stats, is.null), ]
LNToutput

## ------------------------------------------------------------------------
lnt_lookup(LNToutput, pattern = "stat.*?")

## ------------------------------------------------------------------------
table(unlist(lnt_lookup(LNToutput, pattern = "stat.+?\\b")))

