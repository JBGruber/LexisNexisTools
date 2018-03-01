# LexisNexisTools
[![Travis-CI Build Status](https://travis-ci.org/JBGruber/LexisNexisTools.svg?branch=master)](https://travis-ci.org/JBGruber/LexisNexisTools)

My PhD supervisor once told me that everyone doing newspaper analysis
    starts by writing code to read in files from the LexisNexis newspaper archive. However,
    while I do recommend this excercise, not everyone has the time. This package takes
    TXT files downloaded from LexisNexis in most languages. I you run into any issues, 
    please file any issue report.

Since this packages takes in txt files which are unstructured in the sense that beginning 
    and end of an article is not clearly indicated, the main function read_LN relies on 
    certain keywords that signal to R where an article begins, ends and where meta-data is 
    stored (See picture below). check_LNfiles thus tests if all keywords are in place. Every article in every TXT 
    file should start with "X of X DOCUMENTS" and end with "LANGUAGE:". The end of the 
    meta-data is indicated by "LENGTH:". Some measures were taken to eliminate problems 
    but where these keywords appear inside an article or headline, test1 or test2 from the 
    check_LNfiles will fail and read_LN will not be able to do its job. In these cases it 
    is recommended to slighly alter the TXT files, e.g. by changing a headline to 
    "language: never stop learning new ones" instead of "LANGUAGE: never stop learning new ones".
    
  
<a href="https://ibb.co/fj5YjG"><img src="https://preview.ibb.co/fOfNdb/LN.png" alt="LN" border="0"></a>

## Installation

```{r eval = FALSE}
devtools::install_github("JBGruber/LexisNexisTools")
```

## Demo
###Load package
```{r eval = FALSE}
library("LexisNexisTools")
```
### set working directory to location of source files
```{r eval = FALSE}
setwd("C:/Test/LNTools test")
```
### Search for nexis files
```{r eval = FALSE}
my_files<-list.files(pattern = ".txt",
                     full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

```
### rename files
rename files to standard format (searchTerm_startDate-endDate_documenRange.txt);
will not rename if files already named correctly
```{r eval = FALSE}
report.df <- rename_LNfiles(x = "C:/Test/LNTools test/", recursive = TRUE, report = TRUE)

recreate my_files in case names have changed
my_files<-list.files(pattern = ".txt",
                     full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
```
### test consistency of files
```{r eval = FALSE}
checks.df <- check_LNfiles(my_files)
#' In how many files do Beginnings and Ends not match? Critical, will not work if some files are FALSE
table(checks.df$Test1)
#' In how many files do Beginnings and Lengths not match? Critical, will not work if some files are FALSE
table(checks.df$Test2)
#' In how many files do Beginnings and the number of articles not match
#' (unlike the above two a few 'FALSE' instances are okay here)
table(checks.df$Test3)
#' How many Beginnings, Ends, Lengths and articles are there (range)
colSums(checks.df[,2:5])
```
removed articles can be investigated. Normally only articles were length is
missing are removed which indicates empty articles ' e.g. when an article only
showed an image. checks.df_test <- checks.df[checks.df$Test3==FALSE]

### read in LexisNexis files to get meta, articles and paragraphs
```{r eval = FALSE}
LNoutput <- read_LN(my_files,
                    encoding = "UTF-8",
                    extractParagraphs = TRUE,
                    convertDate = FALSE,
                    dateFormat = "%B %d, %Y",
                    start_keyword = "\\d+ of \\d+ DOCUMENTS$| Dokument \\d+ von \\d+$",
                    end_keyword = "^LANGUAGE: |^SPRACHE: ",
                    length_keyword = "^LENGTH: |^LÄNGE: ",
                    verbose = TRUE)
```
### convert Output to three seperate data.frames
```{r eval = FALSE}                 
meta.df <- LNoutput@meta
articles.df <- LNoutput@articles
paragraphs.df <- LNoutput@paragraphs

```
### identify highly similar articles (nexis often delivers many of those)
```{r eval = FALSE}
duplicates.df <- similarity_LN(texts = LNoutput@articles$Article,
                               dates = LNoutput@meta$Date,
                               IDs = LNoutput@articles$ID,
                               Rel.diff.on = FALSE)

#' generate new dataframes without highly similar duplicates
meta.df <-
  LNoutput@meta[!LNoutput@meta$ID %in% duplicates.df$ID.duplicate, ]
articles.df <-
  LNoutput@articles[!LNoutput@articles$ID %in% duplicates.df$ID.duplicate, ]
paragraphs.df <-
  LNoutput@paragraphs[!LNoutput@paragraphs$ID %in% duplicates.df$ID.duplicate, ]
```
