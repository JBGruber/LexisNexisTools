context("Read sample file")

files <- c(lnt_sample(copy = FALSE),
           lnt_sample(copy = FALSE))

test_that("Read in sample file", {
  expect_equal({
    test <- lnt_read(files[1], verbose = TRUE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    # ensures compatibility with different version of tibble
    attributes(test@paragraphs)$.internal.selfref <- NULL
    test
  }, readRDS("../files/LNToutput.RDS"))
  expect_equal({
    test <- lnt_read(files, verbose = TRUE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    attributes(test@paragraphs)$.internal.selfref <- NULL
    test <- test[1:10]
    test
  }, readRDS("../files/LNToutput.RDS"))
  expect_equal({
    test <- lnt_read(files, verbose = TRUE, extract_paragraphs = FALSE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    attributes(test@paragraphs)$.internal.selfref <- NULL
    test
  }, readRDS("../files/LNToutput2.RDS"))
})

test_that("Read in folder", {
  expect_error({
    lnt_read("../")
  }, "No txt, rtf, doc, pdf, docx, zip files found.", fixed = TRUE)
  expect_that({
    test <- lnt_read(dirname(files),
                     recursive = TRUE,
                     extract_paragraphs = FALSE,
                     file_type = "txt")
    length(test@meta$ID)
  }, is_more_than(19))
})

test_that("Errors and warnings", {
  expect_error({
    writeLines("", "../files/emtpy.txt")
    lnt_read("../files/emtpy.txt", verbose = FALSE, extract_paragraphs = FALSE)
  }, "No articles found in provided file(s)", fixed = TRUE)
  expect_warning({
    lnt_read(dirname(files),
             convert_date = FALSE,
             file_pattern = "txt",
             verbose = FALSE)
  }, "The argument 'file_pattern' was used in earlier versions of the package and has been replaced by 'file_type'. Please consider changing your syntax.",
  fixed = TRUE)
})

teardown(unlink(
  "../files/emtpy.txt"
))
