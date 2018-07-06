context("LNToutput methods")
library(LexisNexisTools)


LNToutput <- lnt_read(lnt_sample(verbose = FALSE), verbose = FALSE)

test_that("Show method", {
  expect_known_output(object = LNToutput, file = "../files/show")
})

test_that("Plus operator", {
  expect_length({
    test <- LNToutput + LNToutput
    test@meta$ID
  }, n = 20)
})

test_that("Subset method", {
  expect_length({
    test <- LNToutput[1:2]
    test@meta$ID
  }, n = 2)
  expect_equal({
    test <- LNToutput[c(2:3, 7), "ID"]
    test@meta$ID
  }, c(2:3, 7))
  expect_equal({
    test <- LNToutput["Guardian", "Newspaper"]
    test@meta$Newspaper
  }, c("Guardian", "Guardian"))
  expect_equal({
    test <- LNToutput["Guardian", "Newspaper", invert = TRUE]
    test@meta$Newspaper
  }, c("Guardian.com", "The Sun (England)", "The Times (London)",
       "The Times (London)", "The Times (London)",
       "MAIL ON SUNDAY (London)", "Sunday Mirror", "DAILY MAIL (London)"))
})

teardown(unlink(lnt_sample(verbose = FALSE)))
