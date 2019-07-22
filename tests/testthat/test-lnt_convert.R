context("LNToutput Conversion")

test_that("Convert LNToutput to rDNA", {
  expect_equal({
    lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                to = "rDNA",
                what = "Articles",
                collapse = TRUE)
  }, readRDS("../files/rDNA.RDS"))
  expect_equal({
    test <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                        to = "rDNA",
                        what = "Paragraphs")
    test$notes
  }, readLines("../files/rDNA_ids"))
  expect_warning({
    test <- readRDS("../files/LNToutput.RDS")
    test@meta$Date[1] <- NA
    test <- lnt_convert(x = test,
                        to = "rDNA")
  }, "One or more (or all) dates could not be converted to POSIXct. NA entries in 'date' were filled with the system's time and date instead.",
  fixed = TRUE)
})

# saveRDS(lnt_convert(x = readRDS("../files/LNToutput.RDS"),
#                     to = "rDNA", what = "Articles", collapse = TRUE), "../files/rDNA.RDS")

test_that("Convert LNToutput to quanteda", {
  skip_if(packageVersion("quanteda") > "1.4.89")
  expect_equal({
    corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                          to = "quanteda", what = "Articles",
                          collapse = FALSE)
    quanteda::metacorpus(corpus, "created") <- "Wed Jul 25 19:33:20 2018"
    quanteda::metacorpus(corpus, "source") <-
      "/home/johannes/Documents/Github/LexisNexisTools/tests/testthat/* on x86_64 by johannes"
    corpus
  }, readRDS("../files/quanteda.RDS"))
  expect_equal({
    corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                          to = "quanteda",
                          what = "Paragraphs",
                          collapse = FALSE,
                          metacorpus = list(notes = "test"))
    unname(unlist(quanteda::metacorpus(corpus, "notes")))
  }, "test")
})

# corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
#                       to = "quanteda", what = "Articles")
# corpus$metadata$created <- "Wed Jul 25 19:33:20 2018"
# corpus
# saveRDS(corpus, "../files/quanteda.RDS")


test_that("Convert LNToutput to quanteda", {
  skip_if(packageVersion("quanteda") < "1.5.0")
  expect_equal({
    corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                          to = "quanteda", what = "Articles",
                          collapse = FALSE)
    quanteda::metacorpus(corpus, "created") <- "Mon Jul  8 10:34:12 2019"
    quanteda::metacorpus(corpus, "source") <-
      "/home/johannes/Documents/Github/LexisNexisTools/tests/testthat/* on x86_64 by johannes"
    corpus
  }, readRDS("../files/quanteda_1.5.RDS"))
  expect_equal({
    corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                          to = "quanteda",
                          what = "Paragraphs",
                          collapse = FALSE,
                          metacorpus = list(notes = "test"))
    unname(unlist(quanteda::metacorpus(corpus, "notes")))
  }, "test")
  expect_equal({
    corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                          to = "quanteda", what = "Articles",
                          collapse = TRUE)
    quanteda::texts(corpus)
    length(gregexpr("\n\n", quanteda::texts(corpus))[[1]])
  }, 4L)
  expect_equal({
    corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                          to = "quanteda", what = "Articles",
                          collapse = "%%")
    quanteda::texts(corpus)
    length(gregexpr("%%", quanteda::texts(corpus))[[1]])
  }, 4L)
})

# corpus <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
#                       to = "quanteda", what = "Articles")
# corpus$metadata$created <- "Mon Jul  8 10:34:12 2019"
# corpus
# saveRDS(corpus, "../files/quanteda_1.5.RDS")

test_that("Convert LNToutput to corpustools", {
  expect_equal({
    cptools <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                           to = "corpustools", what = "Articles")
    out <- list()
    out[[1]] <- class(cptools)
    out[[2]] <- cptools$get()
    out[[3]] <- cptools$get_meta()
  }, readRDS("../files/corpustools.RDS"))
  expect_equal({
    cptools <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                           to = "corpustools", what = "Paragraphs")
    c(nrow(cptools$get()), class(cptools), length(unique(cptools$get()$doc_id)))
  }, c("9992", "tCorpus", "R6", "122"))
})

# saveRDS({
#   cptools <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
#                          to = "corpustools", what = "Articles")
#   out <- list()
#   out[[1]] <- class(cptools)
#   out[[2]] <- cptools$get()
#   out[[3]] <- cptools$get_meta()
# }, "../files/corpustools.RDS")

test_that("Convert LNToutput to tidytext", {
  expect_equal({
    lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                           to = "tidytext", what = "Articles")
  }, readRDS("../files/tidytext.RDS"))
  expect_equal({
    test <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                        to = "tidytext", what = "Paragraphs")
    nrow(test)
  }, 8587)
})

# saveRDS(lnt_convert(x = readRDS("../files/LNToutput.RDS"),
#                     to = "tidytext", what = "Articles"), "../files/tidytext.RDS")

test_that("Convert LNToutput to tm", {
  expect_equal({
    lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                to = "tm", what = "Articles")
  }, readRDS("../files/tm.RDS"))
  expect_equal({
    test <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                         to = "tm",
                         what = "Articles",
                         collapse = TRUE)
    length(gregexpr("\n\n", test[["1"]][["content"]])[[1]])
  }, 4L)
  expect_equal({
    test <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                        to = "tm",
                        what = "Articles",
                        collapse = "%%")
    length(gregexpr("%%", test[["1"]][["content"]])[[1]])
  }, 4L)
  expect_equal({
    test <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                        to = "tm",
                        what = "Paragraphs")
    length(test)
  }, 122L)
})

# saveRDS(lnt_convert(x = readRDS("../files/LNToutput.RDS"),
#                     to = "tm", what = "Articles"), "../files/tm.RDS")

test_that("Convert LNToutput to SQLite", {
  expect_equal({
    unlink("../files/LNT.sqlite")
    conn <- lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                        to = "SQLite", what = "Articles",
                        file = "../files/LNT.sqlite")
    conn@dbname <- basename(conn@dbname)
    conn
  }, readRDS("../files/SQLite.RDS"))
})

test_that("Test error messages", {
  expect_error ({
    lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                to = "rDNA", what = "Article")
  }, "Choose either \"Articles\" or \"Paragraphs\" as what argument.", fixed = TRUE)
  expect_error ({
    lnt_convert(x = readRDS("../files/LNToutput.RDS"),
                to = "quanteda", what = "Paragraph")
  }, "Choose either \"Articles\" or \"Paragraphs\" as what argument.", fixed = TRUE)
})

# saveRDS(conn, "../files/SQLite.RDS")


test_that("Convert LNToutput to bibtex", {
  expect_equal({
    test <- lnt2bibtex(x = readRDS("../files/LNToutput.RDS"),
                       art_id = 1)
    test
  }, readRDS("../files/bibtex.RDS"))
})

# saveRDS(test, "../files/bibtex.RDS")
lnt2bibtex

teardown(unlink("../files/LNT.sqlite"))
