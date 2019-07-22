context("LNToutput methods")

LNToutput <- readRDS("../files/LNToutput.RDS")

test_that("Show method", {
  expect_equal(nchar(capture_output(show(LNToutput))),
               2151,
               tolerance = 10) # different OS = slightly different printing
})

test_that("Plus operator", {
  expect_warning({
    expect_length({
      test <- LNToutput + LNToutput
      test@meta$ID
    }, n = 20)
  }, "After objects were merged, there were duplicated IDs. This was fixed.")
})

test_that("Subset method", {
  expect_length({
    test <- LNToutput[1:2]
    test@meta$ID
  }, n = 2)
  expect_equal({
    test <- LNToutput[c(2:3, 7),  j = "ID"]
    test@meta$ID
  }, c(2:3, 7))
  expect_equal({
    test <- LNToutput[LNToutput@articles$Article[1], j = "Article"]
    test@articles$Article
  }, LNToutput@articles$Article[1])
  expect_equal({
    test <- LNToutput[i = c(2:3, 7), j = "Par_ID"]
    test@paragraphs$Par_ID
  }, c(2:3, 7))
  expect_equal({
    test <- LNToutput["Guardian", "Newspaper"]
    test@meta$Newspaper
  }, c("Guardian", "Guardian"))
  expect_equal({
    test <- LNToutput[100, "Par_ID"]
    unique(test@paragraphs$Art_ID)
  }, 10)
  expect_equal({
    test <- LNToutput["Guardian", "Newspaper", invert = TRUE]
    test@meta$Newspaper
  }, c("Guardian.com", "The Sun (England)", "The Times (London)",
       "The Times (London)", "The Times (London)",
       "MAIL ON SUNDAY (London)", "Sunday Mirror", "DAILY MAIL (London)"))
  expect_error(LNToutput["Guardian", "not_valid"],
               "'j' was not found to be a valid column name.")
})


test_that("+", {
  expect_equal({
    suppressWarnings(dim(LNToutput + LNToutput))
  }, c(Articles = 20, Meta_variable = 10, data.frames = 3))
  expect_warning(
    LNToutput + LNToutput,
    "After objects were merged, there were duplicated IDs. This was fixed."
  )
})

test_that("add", {
  expect_equal({
    test <- readRDS("../files/LNToutput.RDS")
    meta <- test@meta
    meta$Graphic <- NULL
    test <- lnt_add(test, meta, where = "meta")
    ncol(test@meta)
  }, 9)
  expect_equal({
    test <- readRDS("../files/LNToutput.RDS")
    meta <- test@meta
    meta$Graphic <- NULL
    test <- lnt_add(test, meta, where = "meta", replace = FALSE)
    ncol(test@meta)
  }, 10)
  expect_equal({
    test <- readRDS("../files/LNToutput.RDS")
    meta <- test@meta
    meta$ID <- 11:20
    test <- lnt_add(test, meta, where = "meta")
    nrow(test@meta)
  }, 20)
  expect_warning({
    test <- readRDS("../files/LNToutput.RDS")
    meta <- test@meta
    meta[11, ] <- meta[10, ]
    meta[11, "ID"] <- 11
    test <- lnt_add(test, meta, where = "meta")
  }, "Some or all entries you added have no equivalent in other slots of \"to.\"")
  expect_equal({
    test <- readRDS("../files/LNToutput.RDS")
    paragraphs <- test@paragraphs
    test <- lnt_add(test, paragraphs, where = "paragraphs")
    nrow(test@paragraphs)
  }, 122)
  expect_error({
    test <- readRDS("../files/LNToutput.RDS")
    paragraphs <- test@paragraphs
    test <- lnt_add(test, paragraphs, where = "test")
  }, "Choose either 'meta', 'articles' or 'paragraphs' as 'to' argument.")
})

test_that("dim", {
  expect_equal({
    test <- readRDS("../files/LNToutput.RDS")
    dim(test)
  }, c(
    Articles = 10,
    Meta_variable = 10,
    data.frames = 3
  ))
})
