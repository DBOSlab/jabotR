test_that("jabot_indets returns a data.frame and filters by level 'FAMILY'", {
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
  tmpdir <- tempdir()
  outfile <- "test_indets"
  df <- jabot_indets(
    level = "GENUS",
    herbarium = "R",
    taxon = "Fabaceae",
    filename = outfile,
    verbose = FALSE,
    save = TRUE,
    dir = tmpdir
  )
  expected_file <- file.path(tmpdir, paste0(outfile, ".csv"))
  expect_true(file.exists(expected_file))
  unlink(expected_file)
})


test_that("jabot_indets returns more rows when level is NULL (all indets)", {
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
  temp_path <- tempdir()
  jabot_download(herbarium = "R", dir = temp_path, verbose = FALSE)

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
})


test_that("jabot_indets updates = TRUE and path is given", {
  temp_path <- tempdir()
  jabot_download(herbarium = "R",
                 dir = temp_path,
                 verbose = FALSE)

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
})


test_that("jabot_indets applies custom reorder", {
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
  tmpdir <- file.path(tempdir(), "new_test_indets_dir")
  if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)

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


test_that("jabot_indets handles non-matching level filter", {
  expect_error(
    jabot_indets(
      level = "species", # invalid for this function
      herbarium = "R",
      taxon = "Fabaceae",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_indets returns empty for unknown taxon", {
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


test_that("jabot_indets works with no filters (all default args)", {
  tmpdir <- file.path(tempdir(), "new_test_indets_dir")
  if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)
jabot_indets(
  verbose = FALSE,
  save = TRUE,
  dir = tmpdir,
)
  expect_true(length(list.files(tmpdir)) > 0)
  unlink(tmpdir, recursive = TRUE)
})


test_that("jabot_indets prints messages with verbose = TRUE", {
  expect_message(jabot_indets(herbarium = "R",
                              level = "FAMILY",
                              verbose = TRUE,
                              save = FALSE))
})


test_that("jabot_indets saves CSV and log", {
  tmp <- tempfile()
  dir.create(tmp)
  result <- jabot_indets(herbarium = "R",
                         verbose = FALSE,
                         save = TRUE,
                         dir = tmp,
                         filename = "test_save")
  expect_true(file.exists(file.path(tmp, "test_save.csv")))
  expect_true(file.exists(file.path(tmp, "log.txt")))
  unlink(tmp, recursive = TRUE)
})


