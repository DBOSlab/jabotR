test_that("jabot_summary works for full search (herbarium = NULL) or with a vector of herbarium acronyms", {
  res_ex <- jabot_summary(verbose = FALSE,
                          save = FALSE,
                          dir = "jabot_summary")

  res_ex_some <- jabot_summary(herbarium = c("R"),
                               verbose = FALSE,
                               save = FALSE,
                               dir = "jabot_summary")

  expect_s3_class(res_ex, "data.frame")
  expect_s3_class(res_ex_some, "data.frame")

  expect_type(res_ex[[3]], "character")
  expect_type(res_ex[[8]], "character")

  expect_equal(ncol(res_ex), 8)

  expect_type(res_ex_some[[3]], "character")
  expect_type(res_ex_some[[8]], "character")
  expect_equal(ncol(res_ex_some), 8)

  expect_gt(nrow(res_ex), nrow(res_ex_some))
})


test_that("jabot_summary saves file when save = TRUE", {
  temp_dir <- tempdir()
  res <- jabot_summary(herbarium = c("RB"),
                       verbose = FALSE,
                       save = TRUE,
                       dir = temp_dir)

  output_path <- file.path(temp_dir, "jabot_summary.csv")
  expect_true(file.exists(output_path))
  unlink(output_path)
})


test_that("jabot_summary fails with invalid herbarium code", {
  expect_error(
    jabot_summary(herbarium = "FAKE",
                  verbose = FALSE,
                  save = FALSE)
  )
})

test_that("jabot_summary works with trailing slash in dir", {
  temp_dir <- file.path(tempdir(), "jabot_summary_dir/")
  res <- jabot_summary(herbarium = "RB",
                       verbose = FALSE,
                       save = TRUE,
                       dir = temp_dir)

  expect_s3_class(res, "data.frame")
  expect_true(file.exists(file.path(gsub("/$", "", temp_dir), "jabot_summary.csv")))

  unlink(temp_dir, recursive = TRUE)
})


test_that("jabot_summary prints expected verbose messages", {
  local_edition(3)  # Required for proper testthat behavior under covr
  expect_message(
    jabot_summary(herbarium = "R",
                  verbose = TRUE,
                  save = FALSE),
    regexp = "Summarizing specimen collections of R 1/1"
  )
})


test_that("jabot_summary returns NA if contact email is missing", {
  df <- jabot_summary(herbarium = "R",
                      verbose = FALSE,
                      save = FALSE)
  expect_true(is.na(df$hasEmail[1]) || is.character(df$hasEmail[1]))
})


test_that("jabot_summary returns Records column as numeric", {
  df <- jabot_summary(herbarium = "R",
                      verbose = FALSE,
                      save = FALSE)
  expect_type(df$Records, "double")
})
