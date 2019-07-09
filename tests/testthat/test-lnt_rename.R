context("LNToutput methods")

dir <- paste0(tempdir(check = TRUE), "/test")
dir.create(dir)
smpl <- paste0(dir, "/", paste0("sample", c(".TXT", ".xlsx")))
file.copy(
  from = system.file("extdata", "sample.TXT", package = "LexisNexisTools"),
  to = smpl,
  overwrite = TRUE    
)

test_that("Rename Sample", {
  skip_on_cran()
  expect_equal({
    file <- lnt_rename(smpl[1], simulate = TRUE,
                       verbose = FALSE)
    basename(file$name_new)
  }, "SampleFile_20091201-20100511_1-10.txt")
  expect_equal({
    file <- lnt_rename(smpl[1], simulate = FALSE)
    smpl[1] <- file$name_new
    file.exists(file$name_new)
  }, TRUE)
  expect_warning({
    x <- lnt_rename(smpl, simulate = TRUE, verbose = FALSE)
  }, "Not all provided files were TXT, DOC, RTF or PDF files. Other formats are ignored.")
})

teardown(unlink(dir, recursive = TRUE, force = TRUE))
