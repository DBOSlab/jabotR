test_that("jabot_indets returns a data.frame and filters by level 'FAMILY'", {
  skip_if_no_jabot()

  df <- jabot_indets(
    level = "FAMILY",
    herbarium = "AFR",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(df, "data.frame")
  expect_true(all(df$taxonRank %in% "FAMILY"))
})


test_that("jabot_indets filters by level 'GENUS'", {
  skip_if_no_jabot()

  df <- jabot_indets(
    level = "GENUS",
    herbarium = "AFR",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(df, "data.frame")
  expect_true(all(df$taxonRank %in% "GENUS"))
})


test_that("jabot_indets filters by year range and state", {
  skip_if_no_jabot()

  df <- jabot_indets(
    level = "FAMILY",
    herbarium = "R",
    recordYear = c("2000", "2024"),
    state = c("Bahia", "Minas Gerais"),
    verbose = FALSE,
    save = FALSE
  )

  expect_true(all(df$stateProvince %in% c("Bahia", "Minas Gerais")))
  expect_true(all(!is.na(df$year)))
  expect_true(all(df$year >= 2000 & df$year <= 2024))
})


test_that("jabot_indets saves output when save = TRUE", {
  skip_if_no_jabot()

  tmpdir <- tempdir()
  outfile <- "test_indets"

  expected_file <- file.path(tmpdir, paste0(outfile, ".csv"))

  if (file.exists(expected_file)) {
    unlink(expected_file)
  }

  jabot_indets(
    level = "GENUS",
    herbarium = "R",
    taxon = "Fabaceae",
    filename = outfile,
    verbose = FALSE,
    save = TRUE,
    dir = tmpdir
  )

  expect_true(file.exists(expected_file))

  unlink(expected_file)
})


test_that("jabot_indets returns more rows when level is NULL", {
  skip_if_no_jabot()

  all_levels <- jabot_indets(
    level = NULL,
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )

  only_family <- jabot_indets(
    level = "FAMILY",
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )

  expect_gt(nrow(all_levels), nrow(only_family))
})


test_that("jabot_indets uses updates = FALSE with provided path", {
  skip_if_no_jabot()

  temp_path <- file.path(tempdir(), "jabot_indets_updates_false")

  if (dir.exists(temp_path)) {
    unlink(temp_path, recursive = TRUE)
  }

  dir.create(temp_path)

  jabot_download(
    herbarium = "R",
    dir = temp_path,
    verbose = FALSE
  )

  df <- jabot_indets(
    level = "FAMILY",
    herbarium = "R",
    taxon = "Fabaceae",
    path = temp_path,
    updates = FALSE,
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(df, "data.frame")

  unlink(temp_path, recursive = TRUE)
})


test_that("jabot_indets uses updates = TRUE with provided path", {
  skip_if_no_jabot()

  temp_path <- file.path(tempdir(), "jabot_indets_updates_true")

  if (dir.exists(temp_path)) {
    unlink(temp_path, recursive = TRUE)
  }

  dir.create(temp_path)

  jabot_download(
    herbarium = "R",
    dir = temp_path,
    verbose = FALSE
  )

  df <- jabot_indets(
    level = "GENUS",
    herbarium = "R",
    taxon = "Fabaceae",
    path = temp_path,
    updates = TRUE,
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(df, "data.frame")

  unlink(temp_path, recursive = TRUE)
})


test_that("jabot_indets applies custom reorder", {
  skip_if_no_jabot()

  df <- jabot_indets(
    level = "FAMILY",
    herbarium = "R",
    taxon = "Fabaceae",
    reorder = c("year", "taxa"),
    verbose = FALSE,
    save = FALSE
  )

  expect_true("year" %in% names(df))
})


test_that("jabot_indets creates directory if missing", {
  skip_if_no_jabot()

  tmpdir <- file.path(tempdir(), "new_test_indets_dir")

  if (dir.exists(tmpdir)) {
    unlink(tmpdir, recursive = TRUE)
  }

  jabot_indets(
    level = "FAMILY",
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = tmpdir
  )

  expect_true(dir.exists(tmpdir))

  unlink(tmpdir, recursive = TRUE)
})


test_that("jabot_indets rejects invalid level", {
  skip_if_no_jabot()

  expect_error(
    jabot_indets(
      level = "species",
      herbarium = "R",
      taxon = "Fabaceae",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_indets rejects unknown taxon", {
  skip_if_no_jabot()

  expect_error(
    jabot_indets(
      level = "FAMILY",
      herbarium = "R",
      taxon = "Fakeplantus",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_indets prints messages when verbose = TRUE", {
  skip_if_no_jabot()

  expect_message(
    jabot_indets(
      herbarium = "R",
      level = "FAMILY",
      verbose = TRUE,
      save = FALSE
    )
  )
})


test_that("jabot_indets saves CSV and log", {
  skip_if_no_jabot()

  tmpdir <- tempfile("jabot_indets_save_")
  dir.create(tmpdir)

  jabot_indets(
    herbarium = "R",
    verbose = FALSE,
    save = TRUE,
    dir = tmpdir,
    filename = "test_save"
  )

  expect_true(file.exists(file.path(tmpdir, "test_save.csv")))
  expect_true(file.exists(file.path(tmpdir, "log.txt")))

  unlink(tmpdir, recursive = TRUE)
})
