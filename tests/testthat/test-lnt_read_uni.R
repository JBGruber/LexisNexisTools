context("Read sample file")
files <- c(system.file("extdata", "sample.DOCX", package = "LexisNexisTools"),
           system.file("extdata", "sample.DOCX", package = "LexisNexisTools"))

test_that("Read in sample file", {
  expect_error(lnt_read("../files/test.DOCX"),
               "No articles found to parse.")
  expect_equal({
    test <- lnt_read(files[1], verbose = TRUE)
    dim(test)
  }, c(Articles = 10, Meta_variable = 10, data.frames = 3))
  expect_equal({
    test <- lnt_read(files, verbose = TRUE, convert_date = FALSE)
    dim(test)
  }, c(Articles = 20, Meta_variable = 10, data.frames = 3))
  expect_equal({
    test <- lnt_read(files[1], verbose = TRUE)
    dim(test@paragraphs)
  }, c(83L, 3L))
})

test_that("Test local collection", {
  skip_if(!dir.exists("/home/johannes/Dropbox/LexisNexis_sample_files/"))
  expect_gt({
    test <- lnt_read("/home/johannes/Dropbox/LexisNexis_sample_files/should_work/",
                     convert_date = FALSE,
                     verbose = FALSE)
    nrow(test)
  }, 2970)
})

test_that("Read files from zip", {
  skip_on_cran()
  tempf <- paste0(tempfile(), ".zip")
  # zip fails if no zip application is installed
  t <- try(zip(zipfile = tempf, files, flags = "-j"))
  skip_if("try-error" %in% class(t))
  expect_equal(
    basename(LexisNexisTools:::get_files(x = tempf)),
    "sample.DOCX"
  )
  expect_equal({
    test <- lnt_read(x = tempf,
                     verbose = FALSE)
    dim(test)
  }, c(Articles = 10, Meta_variable = 10, data.frames = 3))
})