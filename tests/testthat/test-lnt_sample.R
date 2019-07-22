context("Create sample")

dir <- paste0(tempdir(check = TRUE), "/test")
dir.create(dir)

test_that("sample exists", {
  skip_on_cran()
  expect_equal({
    file <- lnt_sample(verbose = FALSE)
    unlink(file)
    file
  }, paste0(getwd(), "/", "sample.TXT"))
  expect_equal(basename(lnt_sample(verbose = FALSE, path = dir)), "sample.TXT")
  expect_equal(file.exists(lnt_sample(verbose = FALSE, path = dir)), TRUE)
  expect_warning(lnt_sample(verbose = TRUE, path = dir),
                 "Sample file exists in wd. Use overwrite = TRUE to create fresh sample file.")
  expect_equal(lnt_sample(verbose = FALSE, copy = FALSE),
               system.file("extdata", "sample.TXT", package = "LexisNexisTools"))
})

teardown(unlink(dir, recursive = TRUE, force = TRUE))
