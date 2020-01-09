context("Lookup keyword")

LNToutput <- readRDS("../files/LNToutput.RDS")

test_that("Lookup stat. computing in sample", {
  expect_equal(lnt_lookup(LNToutput, "statistical computing", verbose = FALSE),
               list(`1` = NULL,
                    `2` = NULL,
                    `3` = NULL,
                    `4` = NULL,
                    `5` = NULL,
                    `6` = NULL,
                    `7` = NULL,
                    `8` = NULL,
                    `9` = c("statistical computing", "statistical computing"),
                    `10` = NULL))
  expect_equal(lnt_lookup(LNToutput, "statis",
                          verbose = FALSE,
                          word_boundaries = "before"),
               list(`1` = NULL,
                    `2` = NULL,
                    `3` = NULL,
                    `4` = NULL,
                    `5` = NULL,
                    `6` = NULL,
                    `7` = NULL,
                    `8` = NULL,
                    `9` = c("statis", "statis", "statis", "statis", "statis", "statis"),
                    `10` = NULL))
  expect_equal(lnt_lookup(LNToutput, "puting",
                          verbose = FALSE,
                          word_boundaries = "after"),
               list(`1` = NULL,
                    `2` = NULL,
                    `3` = NULL,
                    `4` = NULL,
                    `5` = NULL,
                    `6` = NULL,
                    `7` = "puting",
                    `8` = NULL,
                    `9` = c("puting", "puting", "puting"),
                    `10` = NULL))
  expect_equal(lnt_lookup(LNToutput, "statis",
                          verbose = TRUE,
                          word_boundaries = FALSE),
               list(`1` = NULL,
                    `2` = NULL,
                    `3` = NULL,
                    `4` = NULL,
                    `5` = NULL,
                    `6` = NULL,
                    `7` = NULL,
                    `8` = NULL,
                    `9` = c("statis", "statis", "statis", "statis", "statis", "statis"),
                    `10` = NULL))
})

test_that("Lookup stat. computing in character string", {
  expect_equal({
    test <- c("This contains statistical computing",
              "This does not")
    names(test) <- 1:2
    lnt_lookup(test, "statistical computing", verbose = FALSE)
    }, list(`1` = "statistical computing", `2` = NULL))
})
