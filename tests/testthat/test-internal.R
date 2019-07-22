context("Internal functions")

test_that("get files throws correct errors", {
  expect_error(LexisNexisTools:::get_files(),
               "No path was given as x.")
  expect_error(LexisNexisTools:::get_files("test.mp3"),
               "Provide either file name(s) ending on .TXT or .RTF or .DOC or .PDF or .DOCX or folder name(s) to x or leave black to search wd.",
               fixed = TRUE)
})


test_that("check_install", {
  expect_error(LexisNexisTools:::check_install("test"),
               "Package \"test\" is needed for this function to work. Please install it.",
               fixed = TRUE)
})

