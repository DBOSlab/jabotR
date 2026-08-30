test_that("jabot_summary works for all herbaria and selected herbaria", {
  skip_if_no_jabot()

  res_all <- jabot_summary(
    verbose = FALSE,
    save = FALSE
  )

  res_selected <- jabot_summary(
    herbarium = "R",
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(res_all, "data.frame")
  expect_s3_class(res_selected, "data.frame")

  expect_equal(ncol(res_all), 8)
  expect_equal(ncol(res_selected), 8)

  expect_type(res_all[[3]], "character")
  expect_type(res_all[[8]], "character")

  expect_type(res_selected[[3]], "character")
  expect_type(res_selected[[8]], "character")

  expect_gt(nrow(res_all), nrow(res_selected))
})


test_that("jabot_summary saves CSV when save = TRUE", {
  skip_if_no_jabot()

  tmp_dir <- tempfile("jabot_summary_save_")
  dir.create(tmp_dir)

  jabot_summary(
    herbarium = "RB",
    verbose = FALSE,
    save = TRUE,
    dir = tmp_dir
  )

  output_path <- file.path(
    tmp_dir,
    "jabot_summary.csv"
  )

  expect_true(file.exists(output_path))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_summary rejects invalid herbarium code", {
  skip_if_no_jabot()

  expect_error(
    jabot_summary(
      herbarium = "FAKE",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_summary handles trailing slash in dir", {
  skip_if_no_jabot()

  tmp_dir <- tempfile("jabot_summary_trailing_")
  dir.create(tmp_dir)

  trailing_dir <- paste0(tmp_dir, "/")

  result <- jabot_summary(
    herbarium = "RB",
    verbose = FALSE,
    save = TRUE,
    dir = trailing_dir
  )

  expect_s3_class(result, "data.frame")

  expect_true(
    file.exists(
      file.path(tmp_dir, "jabot_summary.csv")
    )
  )

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_summary prints expected verbose message", {
  skip_if_no_jabot()

  local_edition(3)

  expect_message(
    jabot_summary(
      herbarium = "R",
      verbose = TRUE,
      save = FALSE
    ),
    regexp = "Summarizing specimen collections of R 1/1"
  )
})


test_that("jabot_summary handles missing contact email", {
  skip_if_no_jabot()

  result <- jabot_summary(
    herbarium = "R",
    verbose = FALSE,
    save = FALSE
  )

  expect_true(
    is.na(result$hasEmail[1]) ||
      is.character(result$hasEmail[1])
  )
})


test_that("jabot_summary returns Records as numeric", {
  skip_if_no_jabot()

  result <- jabot_summary(
    herbarium = "R",
    verbose = FALSE,
    save = FALSE
  )

  expect_type(result$Records, "double")
})
