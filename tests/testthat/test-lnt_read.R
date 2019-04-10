context("Read sample file")

files <- system.file("extdata", "sample.TXT", package = "LexisNexisTools")
file.copy(files, paste0(basename(files), "2.TXT"))
files <- c(files, paste0(basename(files), "2.TXT"))

test_that("Read in sample file", {
  expect_equal({
    test <- lnt_read(files[1], verbose = TRUE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    test
  }, readRDS("../files/LNToutput.RDS"))
  expect_equal({
    test <- lnt_read(files, verbose = TRUE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    test <- test[1:10]
    test
  }, readRDS("../files/LNToutput.RDS"))
  expect_equal({
    test <- lnt_read(files, verbose = TRUE, extract_paragraphs = FALSE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    test
  }, readRDS("../files/LNToutput2.RDS"))
})

test_that("Read in folder", {
  expect_error({
    lnt_read("../")
  }, "No .txt files found.", fixed = TRUE)
  expect_that({
    test <- lnt_read("../../", recursive = TRUE, extract_paragraphs = FALSE)
    length(test@meta$ID)
  }, is_more_than(19))
})

test_that("no articles found", {
  expect_error({
    writeLines("", "../files/emtpy.txt")
    lnt_read("../files/emtpy.txt", verbose = FALSE, extract_paragraphs = FALSE)
  }, "No articles found in provided file(s)", fixed = TRUE)
})

teardown(unlink(c(
  files[2],
  "../files/emtpy.txt"
)))
