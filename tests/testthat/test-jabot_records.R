test_that("jabot_records basic usage returns a data.frame", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
})


test_that("jabot_records handles empty taxon search", {
  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fakeplantus invalidus",
      save = FALSE,
      verbose = FALSE)
  )
})


test_that("jabot_records applies state and year filters", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    state = c("Bahia", "Minas Gerais"),
    recordYear = c("2000", "2024"),
    save = FALSE,
    verbose = FALSE
  )
  expect_true(all(result$stateProvince %in% c("Bahia", "Minas Gerais")))
  expect_true(all(as.numeric(result$year) >= 2000 & as.numeric(result$year) <= 2024))
})


test_that("jabot_records reorders columns properly", {
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
  temp_dir <- tempdir()
  test_file <- "test_output"
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = temp_dir,
    filename = test_file
  )
  output_path <- file.path(temp_dir, paste0(test_file, ".csv"))
  expect_true(file.exists(output_path))

  unlink(output_path)
})


test_that("jabot_records removes indeterminate specimens with indets = FALSE", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    indets = FALSE,
    verbose = FALSE,
    save = FALSE
  )
  expect_false(any(result$taxonRank %in% c("family", "genus", "FAMILY", "GENERO", "FAMILIA", "SUB_FAMILIA",
                                           "TRIBO", "DIVISAO", "ORDEM", "CLASSE")))
})


test_that("jabot_records triggers auto download when path is NULL", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    path = NULL,
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records creates new dir if not present", {
  tmp_dir <- file.path(tempdir(), "new_jabot_records_dir")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
  expect_silent(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      verbose = FALSE,
      save = TRUE,
      dir = tmp_dir
    )
  )
  expect_true(dir.exists(tmp_dir))
  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_records returns empty data.frame if no match after filters", {
  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      state = "ZZ",  # invalid state
      verbose = FALSE,
      save = FALSE)
  )
})


test_that("jabot_records uses updates = FALSE with preexisting path", {
  test_path <- tempdir()
  jabot_download(herbarium = "R",
                 verbose = FALSE,
                 dir = test_path)

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    path = test_path,
    updates = FALSE,
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records handles partial reorder vector", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    reorder = c("taxa", "year"),
    verbose = FALSE,
    save = FALSE
  )
  expect_true(all(c("family", "year") %in% colnames(result)))
})


test_that("jabot_records stops on invalid year", {
  expect_error(
    jabot_records(
      taxon = "Fabaceae",
      herbarium = "R",
      recordYear = c("1880", "1870"), # invalid range
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_records updates data when path is given and updates = TRUE", {
  tmp_path <- tempdir()
  jabot_download(herbarium = "R", dir = tmp_path, verbose = FALSE)
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    path = tmp_path,
    updates = TRUE,
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records works with herbarium = NULL", {
  result <- jabot_records(
    herbarium = NULL,
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records saves CSV and log.txt with save = TRUE", {
  tmp_dir <- tempdir()
  test_file <- "log_test"

  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = TRUE,
    dir = tmp_dir,
    filename = test_file
  )

  expect_true(file.exists(file.path(tmp_dir, paste0(test_file, ".csv"))))
  expect_true(file.exists(file.path(tmp_dir, "log.txt")))

  unlink(file.path(tmp_dir, paste0(test_file, ".csv")))
  unlink(file.path(tmp_dir, "log.txt"))
})


test_that("jabot_records prints message when verbose = TRUE", {
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


test_that("jabot_records handles NULL filename (if supported)", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Fabaceae",
    verbose = FALSE,
    save = FALSE,
    filename = NULL
  )
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records handles invalid reorder column gracefully", {
  expect_error(
    jabot_records(
      herbarium = "R",
      taxon = "Fabaceae",
      reorder = c("INVALID_COLUMN"),
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_records works with genus-level taxon only", {
  result <- jabot_records(
    herbarium = "R",
    taxon = "Inga",
    verbose = FALSE,
    save = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records with default values still returns results", {
  result <- jabot_records()
  expect_s3_class(result, "data.frame")
})


test_that("jabot_records prints messages with verbose = TRUE", {
  expect_message(jabot_records(herbarium = "R",
                               verbose = TRUE,
                               save = FALSE))
})


test_that("jabot_records creates directory when not found", {
  tmpdir <- tempdir()
  expect_message(
    jabot_records(herbarium = "R",
                  state = "Bahia",
                  recordYear = "2000",
                  updates = FALSE,
                  verbose = TRUE,
                  save = FALSE,
                  dir = "new_dir"),
    "Creating directory 'new_dir' in working directory..."
  )
})

