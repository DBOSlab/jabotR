test_that("jabot_records basic usage returns a data.frame", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    save = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")
})


test_that("jabot_records rejects unknown taxon", {
  skip_if_no_jabot()

  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fakeplantus invalidus",
      save = FALSE,
      verbose = FALSE
    )
  )
})


test_that("jabot_records applies state and year filters", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    state = c("Bahia", "Minas Gerais"),
    recordYear = c("2000", "2024"),
    save = FALSE,
    verbose = FALSE
  )

  expect_true(
    all(result$stateProvince %in% c("Bahia", "Minas Gerais"))
  )

  years <- suppressWarnings(as.numeric(result$year))

  expect_true(all(!is.na(years)))
  expect_true(all(years >= 2000 & years <= 2024))
})


test_that("jabot_records reorders columns properly", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    reorder = c("year", "herbarium"),
    save = FALSE,
    verbose = FALSE
  )

  expect_true("year" %in% names(result))
  expect_true("institutionCode" %in% names(result))
})


test_that("jabot_records saves file when save = TRUE", {
  skip_if_no_jabot()

  tmp_dir <- tempfile("jabot_records_save_")
  dir.create(tmp_dir)

  test_file <- "test_output"

  jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = tmp_dir,
    filename = test_file
  )

  output_path <- file.path(
    tmp_dir,
    paste0(test_file, ".csv")
  )

  expect_true(file.exists(output_path))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_records removes indeterminate specimens when indets = FALSE", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    indets = FALSE,
    verbose = FALSE,
    save = FALSE
  )

  indeterminate_ranks <- c(
    "family",
    "genus",
    "FAMILY",
    "GENERO",
    "FAMILIA",
    "SUB_FAMILIA",
    "TRIBO",
    "DIVISAO",
    "ORDEM",
    "CLASSE"
  )

  expect_false(
    any(result$taxonRank %in% indeterminate_ranks)
  )
})


test_that("jabot_records triggers automatic download when path is NULL", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    path = NULL,
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(result, "data.frame")
})


test_that("jabot_records rejects invalid state", {
  skip_if_no_jabot()

  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      state = "ZZ",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_records uses updates = FALSE with preexisting path", {
  skip_if_no_jabot()

  test_path <- tempfile("jabot_records_updates_false_")
  dir.create(test_path)

  jabot_download(
    herbarium = "R",
    verbose = FALSE,
    dir = test_path
  )

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    path = test_path,
    updates = FALSE,
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(result, "data.frame")

  unlink(test_path, recursive = TRUE)
})


test_that("jabot_records handles partial reorder vector", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    reorder = c("taxa", "year"),
    verbose = FALSE,
    save = FALSE
  )

  expect_true(
    all(c("family", "year") %in% names(result))
  )
})


test_that("jabot_records rejects invalid year range", {
  skip_if_no_jabot()

  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      recordYear = c("1880", "1870"),
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_records updates data when path is given and updates = TRUE", {
  skip_if_no_jabot()

  tmp_path <- tempfile("jabot_records_updates_true_")
  dir.create(tmp_path)

  jabot_download(
    herbarium = "R",
    dir = tmp_path,
    verbose = FALSE
  )

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    path = tmp_path,
    updates = TRUE,
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(result, "data.frame")

  unlink(tmp_path, recursive = TRUE)
})


test_that("jabot_records works with herbarium = NULL", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = NULL,
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(result, "data.frame")
})


test_that("jabot_records saves CSV and log when save = TRUE", {
  skip_if_no_jabot()

  tmp_dir <- tempfile("jabot_records_log_")
  dir.create(tmp_dir)

  test_file <- "log_test"

  jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = tmp_dir,
    filename = test_file
  )

  expect_true(
    file.exists(
      file.path(tmp_dir, paste0(test_file, ".csv"))
    )
  )

  expect_true(
    file.exists(
      file.path(tmp_dir, "log.txt")
    )
  )

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_records prints herbarium check message when verbose = TRUE", {
  skip_if_no_jabot()

  expect_message(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      verbose = TRUE,
      save = FALSE
    ),
    "Checking whether the input herbarium code exists"
  )
})


test_that("jabot_records handles NULL filename when save = FALSE", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE,
    filename = NULL
  )

  expect_s3_class(result, "data.frame")
})


test_that("jabot_records rejects invalid reorder column", {
  skip_if_no_jabot()

  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      reorder = "INVALID_COLUMN",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_records works with genus-level taxon", {
  skip_if_no_jabot()

  result <- jabot_records(
    herbarium = "R",
    taxon = "Inga",
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(result, "data.frame")
})


test_that("jabot_records prints messages when verbose = TRUE", {
  skip_if_no_jabot()

  expect_message(
    jabot_records(
      herbarium = "R",
      verbose = TRUE,
      save = FALSE
    )
  )
})


test_that("jabot_records reports DwC-A update when path is given and updates = TRUE", {
  skip_if_no_jabot()

  tmp_path <- tempfile("jabot_dwca_update_")
  dir.create(tmp_path)

  jabot_download(
    herbarium = "R",
    verbose = FALSE,
    dir = tmp_path
  )

  expect_message(
    result <- jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      path = tmp_path,
      updates = TRUE,
      verbose = TRUE,
      save = FALSE
    ),
    regexp = "Updating dwca files within"
  )

  expect_s3_class(result, "data.frame")

  unlink(tmp_path, recursive = TRUE)
})
