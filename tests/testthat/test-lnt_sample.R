context("Create sample")
library(LexisNexisTools)

test_that("sample exists", {
  expect_equal(basename(lnt_sample(verbose = FALSE)), "sample.TXT")
  expect_equal(file.exists(lnt_sample(verbose = FALSE)), TRUE)
})

teardown(unlink(lnt_sample(verbose = FALSE)))
