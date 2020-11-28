context("Create sample")
dir <- paste0(tempdir(check = TRUE), "/test")
dir.create(dir)
test_that("sample function works", {
  expect_equal(lnt_sample(copy = FALSE),
               system.file("extdata", "sample.TXT",
                           package = "LexisNexisTools"))
  expect_equal(lnt_sample(format = "docx", copy = FALSE),
               system.file("extdata", "sample.DOCX",
                           package = "LexisNexisTools"))
  expect_equal(basename(lnt_sample(verbose = FALSE, path = dir)),
               "sample.TXT")
  expect_equal(basename(lnt_sample(format = "docx", verbose = FALSE,
                                   path = dir)),
               "sample.DOCX")
  expect_equal(file.exists(lnt_sample(verbose = FALSE, path = dir)),
               TRUE)
  expect_equal(file.exists(lnt_sample(format = "docx", verbose = FALSE,
                                      path = dir)),
               TRUE)
  expect_warning(lnt_sample(verbose = TRUE, path = dir),
                 "Sample file exists in wd. Use overwrite = TRUE to create fresh sample file.")
  expect_error(lnt_sample(format = "rtf"),
               "Choose either \"txt\" or \"docx\" as format.")
})
teardown(unlink(dir, recursive = TRUE, force = TRUE))