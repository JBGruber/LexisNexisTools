context("Read sample file")
library(LexisNexisTools)


test_that("Read in sample file", {
  expect_equal({
    test <- lnt_read(lnt_sample(verbose = FALSE), verbose = FALSE)
    test@meta$Source_File <- "sample.TXT"
    attributes(test)$created$time <- "2018-07-06 08:29:21 BST"
    attributes(test)$created$Version <- "0.1.49.9000"
    test
    }, readRDS("../files/LNToutput.RDS"))
})

teardown(unlink(lnt_sample(verbose = FALSE)))       
