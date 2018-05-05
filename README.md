# LexisNexisTools
[![Travis-CI Build Status](https://travis-ci.org/JBGruber/LexisNexisTools.svg?branch=master)](https://travis-ci.org/JBGruber/LexisNexisTools)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/LexisNexisTools)](http://cran.r-project.org/package=LexisNexisTools)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/LexisNexisTools)](http://cran.r-project.org/package=LexisNexisTools)

My PhD supervisor once told me that everyone doing newspaper analysis starts by
writing code to read in files from the 'LexisNexis' newspaper archive. However,
while I do recommend this exercise, not everyone has the time. This package
takes TXT files downloaded from 'LexisNexis' in most languages. If you run into any
issues, please file an issue report: <https://github.com/JBGruber/LexisNexisTools/issues>.

Since this package takes in txt files which are unstructured in the sense that
beginning and end of an article is not clearly indicated, the main function
`lnt_read()` relies on certain keywords that signal to R where an article begins,
ends and where meta-data is stored (See picture below). `lnt_checkFiles()` thus tests
if all keywords are in place. Every article in every TXT file should start with
"X of X DOCUMENTS" and end with "LANGUAGE:" (Or the equivalent in German or
French etc.). The end of the meta-data is indicated by "LENGTH:". Some measures
were taken to eliminate problems but where these keywords appear inside an
article or headline, test1 or test2 from the `lnt_checkFiles()` will fail and `lnt_read()`
will not be able to do its job. In these cases it is recommended to slightly
alter the source TXT files, e.g. by changing a headline to "language: never stop
learning new ones" instead of "LANGUAGE: never stop learning new ones".
    
  
<a href="https://ibb.co/fj5YjG"><img src="https://preview.ibb.co/fOfNdb/LN.png" alt="LN" border="0"></a>

## Installation
Install via:

```R
install.packages("LexisNexisTools")
```

Or get development version by installing `devtools` first (via `install.packages("devtools")`) then
use:

```R
devtools::install_github("JBGruber/LexisNexisTools")
```

## Demo
### Load Package
```R
library("LexisNexisTools")
```
### Set Working Directory to Location of Source Files
```R
setwd("C:/Test/LNTools test")
```
If you do not have files from 'LexisNexis', you can also use `lnt_sample()` to copy a sample file into your current working directory.

### Search for Nexis Files
```R
my_files <- list.files(pattern = ".txt",
                     full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
```

### Rename Files
'LexisNexis' does not give the txt files proper names. This function renames files
to a standard format: searchTerm_startDate-endDate_documenRange.txt. Note, that this will not work if your txt files lack a cover page with this information. Currently, it seems, like Nexis only delivers those cover pages when you first create a link to your search ("link to this search" on the results page), follow this link, and then download the txt files from there.
```R
report.df <- lnt_rename(x = my_files, report = TRUE)

# recreate my_files in case names have changed
my_files<-list.files(pattern = ".txt",
                     full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
```
The argument `report = TRUE` indicates that the output of the function in `R`
will be a data.frame containing a report on which files have changed on your
drive and how.

### Test Consistency of Files
`LexisNexisTools` relies on certain key terms to split the article texts from
their metadata. Experience has shown though that Nexis sometimes fails to
provide these properly or the key terms are coincidentally used inside the text.
`lnt_checkFiles()` provides a quick way to test the consistency of your files
before using `lnt_read()` on them---which takes much longer.

```R
checks.df <- lnt_checkFiles(my_files)

#' In how many files do Beginnings and Ends not match? Critical, will not work
if some files are FALSE
table(checks.df$Test1)

#' In how many files do Beginnings and Lengths not match? Critical, will not
work if some files are FALSE
table(checks.df$Test2)

#' In how many files do Beginnings and the number of articles not match
#' (unlike the above two a few 'FALSE' instances are okay here)
table(checks.df$Test3)

#' How many Beginnings, Ends, Lengths and articles are there (range)
colSums(checks.df[,2:5])
```
For this function and `lnt_read()`, it is necessary to remove empty articles.
However, this is accomplished by removing articles where 'LENGTH' is missing,
which might in rare cases have different reasons. You should, therefore, try to
investigate the removed articles. Most commonly, only articles which contain an
image and nothing else lack the LENGTH information. You can retrieve the names
of the files in which this happens with `checks.df[checks.df$Test3 == FALSE]`.

### Read in 'LexisNexis' Files to Get Meta, Articles and Paragraphs

The main function of this package is `lnt_read()`. It converts the raw text
from the source files into three different data.frames nested in a special S4
object of class `LNToutput`. The three data.frames contain (1.) the metadata of
the artilces (2.) the articles themeselves (3.) paragraphs, you have set the
`extractParagraphs` to `TRUE`.

```R
LNToutput <- lnt_read(my_files)
```
### Convert Output to Three Seperate data.frames
Objects of class `LNToutput` can easily be converted:
```R                 
meta.df <- LNToutput@meta
articles.df <- LNToutput@articles
paragraphs.df <- LNToutput@paragraphs

```
### Identify Highly Similar Articles (Nexis Often Delivers many of Those)
One common problem when working with 'LexisNexis' data is that many articles appear to be delivered twice or more times. While direct duplicates can be filtered out using `articles.df <- articles.df[!duplicated(articles.df$Article), ]` this does not work for articles with incremental differences from each other. Hence when one comma or whitespace is different between two articles, they are normally treated as different. The function `lnt_similarity()` thus creates a dataframe with two similarity measures for every article (if `Rel.diff.on = TRUE`): quanteda's fast `textstat_simil` and the much slower but more accurate Levenshtein distance. Articles are only checked for similarity when they were published on the same day, due to performance reasons.
```R
duplicates.df <- lnt_similarity(texts = LNToutput@articles$Article,
                                dates = LNToutput@meta$Date,
                                IDs = LNToutput@articles$ID,
                                rel_dist = FALSE)

#' generate new dataframes without highly similar duplicates
meta.df <-
  LNToutput@meta[!LNToutput@meta$ID %in% duplicates.df$ID.duplicate, ]
articles.df <-
  LNToutput@articles[!LNToutput@articles$ID %in% duplicates.df$ID.duplicate, ]
paragraphs.df <-
  LNToutput@paragraphs[!LNToutput@paragraphs$ID %in% duplicates.df$ID.duplicate, ]
```
