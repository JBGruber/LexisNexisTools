context("Display diff")

duplicates.df <- readRDS("../files/duplicates.df.RDS")

# writeLines(capture.output(lnt_diff(duplicates.df,
#                                    min = 0.18,
#                                    max = 0.30,
#                                    output_html = TRUE)),
#            "../files/diff")

test_that("Show method", {
  expect_known_output(object = lnt_diff(duplicates.df,
                                        min = 0.18,
                                        max = 0.30,
                                        output_html = TRUE),
                      file = "../files/diff")
})


test_that("lnt_diff warnings and errors", {
  expect_error({
    test <- data.frame()
    class(test) <- c("data.table", "data.frame", "lnt_sim")
    lnt_diff(test)
  },
  "'x' must contain a column with rel_dist information (see ?lnt_similarity)",
  fixed = TRUE)
  expect_warning({
    class(duplicates.df) <- "data.frame"
    diff <- capture_output(
      lnt_diff(x = duplicates.df,
               min = 0.18,
               max = 0.30,
               output_html = TRUE))
  },
  "'x' should be an object returned by lnt_similarity().")
})
