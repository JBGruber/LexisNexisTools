context("Display diff")
library(LexisNexisTools)

duplicates.df <- lnt_similarity(LNToutput = lnt_read(lnt_sample(verbose = FALSE),
                                                     verbose = FALSE),
                                threshold = 0.95,
                                verbose = FALSE)

test_that("Show method", {
  expect_known_output(object = lnt_diff(duplicates.df, 
                                        min = 0.18, 
                                        max = 0.30,
                                        output_html = TRUE), 
                      file = "../files/diff")
})

