context("Read sample file")

test_that("Read in sample file", {
  expect_error(lnt_read("../files/test.DOCX"),
               "No articles found to parse.")
  skip_on_cran()
  skip_on_travis()
  expect_equal({
    test <- lnt_read("/home/johannes/Dropbox/LexisNexis_sample_files/should_work/Files(30).DOCX",
                     verbose = TRUE)
    test@meta$Source_File <- basename(test@meta$Source_File)
    attributes(test)$created$time <- "2018-12-15 01:00:38 GMT"
    attributes(test)$created$Version <- "0.2.1.9000"
    test
  }, readRDS("../files/LNToutput_uni.RDS"))
  expect_gt({
    test <- lnt_read("/home/johannes/Dropbox/LexisNexis_sample_files/should_work/",
                     convert_date = FALSE,
                     verbose = FALSE)
    nrow(test)
  }, 2970)
})
