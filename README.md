# LexisNexisTools
My PhD supervisor once told me that everyone doing newspaper analysis
    starts by writing code to read in files from the LexisNexis newspaper archive. However,
    while I do recommend this excercise , not everyone has the time. This package takes
    TXT files downloaded from LexisNexis (tested in English and German). 
    If you want other languages included, ask me or fork this project :).

Since this packages takes in txt files which are unstructured in the sense that beginning 
    and end of an article is not clearly indicated, the main function read_LN relies on 
    certain keywords that signal to R where an article begins, ends and where meta-data is 
    stored. check_LNfiles thus tests if all keywords are in place. Every article in every TXT 
    file should start with "X of X DOCUMENTS" and end with "LANGUAGE:". The end of the 
    meta-data is indicated by "LENGTH:". Some measures were taken to eliminate problems 
    but where these keywords appear inside an article or headline, test1 or test2 from the 
    check_LNfiles will fail and read_LN will not be able to do its job. In these cases it 
    is recommended to slighly alter the TXT files, e.g. by changing a headline to 
    "language: never stop learning new ones" instead of "LANGUAGE: never stop learning new ones"
