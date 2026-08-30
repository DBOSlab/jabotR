test_that("jabot_download downloads multiple herbaria correctly", {
  skip_if_no_jabot()

  temp_dir <- file.path(tempdir(), "jabot_download_test")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  jabot_download(
    herbarium = c("AFR", "UPCB"),
    verbose = FALSE,
    dir = temp_dir
  )

  folders <- list.files(temp_dir)
  expect_equal(length(folders), 2)

  contents <- lapply(file.path(temp_dir, folders), list.files)
  expect_true(all(lengths(contents) >= 3))

  unlink(temp_dir, recursive = TRUE)
})


test_that("jabot_download creates _Jabot.csv per herbarium", {
  skip_if_no_jabot()

  temp_dir <- file.path(tempdir(), "jabot_csv_test")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  jabot_download(
    herbarium = c("AFR", "UPCB"),
    verbose = FALSE,
    dir = temp_dir
  )

  downloaded_dirs <- list.files(temp_dir, full.names = TRUE)

  all_csvs <- unlist(
    lapply(
      downloaded_dirs,
      function(x) {
        list.files(
          x,
          pattern = "_Jabot\\.csv$",
          full.names = TRUE
        )
      }
    )
  )

  expect_equal(length(all_csvs), 2)
  expect_true(all(file.exists(all_csvs)))

  unlink(temp_dir, recursive = TRUE)
})


test_that("jabot_download returns silently with existing dwca folder", {
  skip_if_no_jabot()

  temp_dir <- file.path(tempdir(), "jabot_download_cached")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  dir.create(temp_dir)

  jabot_download(
    herbarium = "R",
    verbose = FALSE,
    dir = temp_dir
  )

  expect_silent(
    jabot_download(
      herbarium = "R",
      verbose = FALSE,
      dir = temp_dir
    )
  )

  unlink(temp_dir, recursive = TRUE)
})


test_that("invalid herbarium code throws an error", {
  skip_if_no_jabot()

  expect_error(
    jabot_summary(
      herbarium = "INVALIDCODE",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_download creates directory if it doesn't exist", {
  skip_if_no_jabot()

  tmp_dir <- file.path(tempdir(), "jabot_auto_dir")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  expect_false(dir.exists(tmp_dir))

  jabot_download(
    herbarium = "AFR",
    verbose = FALSE,
    dir = tmp_dir
  )

  expect_true(dir.exists(tmp_dir))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_download prints messages when verbose = TRUE", {
  skip_if_no_jabot()

  tmp_dir <- file.path(tempdir(), "jabot_verbose_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  expect_message(
    jabot_download(
      herbarium = "AFR",
      verbose = TRUE,
      dir = tmp_dir
    ),
    "Downloading DwC-A files"
  )

  unlink(tmp_dir, recursive = TRUE)
})
