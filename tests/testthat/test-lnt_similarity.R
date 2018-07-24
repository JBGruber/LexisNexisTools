context("Lookup keyword")
library(LexisNexisTools)


LNToutput <- lnt_read(lnt_sample(verbose = FALSE), verbose = FALSE)

test_that("Test similarity", {
  expect_equal(lnt_similarity(LNToutput = LNToutput),
               readRDS("../files/duplicates.df.RDS"))
})

LNToutput@articles$Article[1] <- ""

test_that("Test similarity", {
  expect_warning(lnt_similarity(LNToutput = LNToutput))
})



teardown(unlink(lnt_sample(verbose = FALSE)))
