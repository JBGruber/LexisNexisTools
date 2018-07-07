context("Lookup keyword")
library(LexisNexisTools)


LNToutput <- lnt_read(lnt_sample(verbose = FALSE), verbose = FALSE)

test_that("Test similarity", {
  expect_equal(lnt_similarity(LNToutput = LNToutput),
               readRDS("../files/duplicates.df.RDS"))
})

teardown(unlink(lnt_sample(verbose = FALSE)))
