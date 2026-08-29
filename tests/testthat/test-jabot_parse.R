.parse_test_fixture <- function(root) {
  afr_dir <- file.path(root, "dwca_AFR_001")
  rb_dir <- file.path(root, "dwca_JBRJ_002")

  dir.create(afr_dir, recursive = TRUE)
  dir.create(rb_dir, recursive = TRUE)

  utils::write.csv(
    data.frame(collectionCode = "AFR", Records = 1),
    file.path(afr_dir, "AFR_Jabot.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(collectionCode = "RB", Records = 1),
    file.path(rb_dir, "RB_Jabot.csv"),
    row.names = FALSE
  )

  invisible(c(afr_dir, rb_dir))
}


.mock_jabot_dwca_read <- function(input, read, encoding, na.strings) {
  collection <- if (grepl("JBRJ", input)) "RB" else "AFR"

  occurrence <- data.frame(
    occurrenceID = paste0(collection, "-1"),
    institutionCode = "JBRJ",
    collectionCode = collection,
    catalogNumber = "1",
    taxonRank = NA_character_,
    family = "fabaceae",
    genus = "inga",
    specificEpithet = "edulis",
    infraspecificEpithet = NA_character_,
    scientificNameAuthorship = "Mart.",
    scientificName = "Inga edulis Mart.",
    stringsAsFactors = FALSE
  )

  list(
    data = list("occurrence.txt" = occurrence),
    files = list(xml_files = file.path(input, "eml.xml"))
  )
}


test_that("jabot_parse errors for a directory that does not exist", {
  bad_path <- tempfile("jabot_parse_missing_")

  expect_error(
    jabot_parse(path = bad_path, verbose = FALSE),
    "There is no folder"
  )
})


test_that("jabot_parse parses all DwC-A folders and attaches JABOT summaries", {
  tmp <- tempfile("jabot_parse_")
  dir.create(tmp)
  .parse_test_fixture(tmp)

  testthat::local_mocked_bindings(
    .arg_check_path = function(...) invisible(TRUE),
    .std_inside_columns = function(df, herbarium = NULL, i = NULL, verbose = TRUE) df,
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    dwca_read = .mock_jabot_dwca_read,
    .package = "finch"
  )

  result <- jabot_parse(path = tmp, verbose = FALSE)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_setequal(names(result), c("dwca_AFR_001", "dwca_JBRJ_002"))

  afr <- result[["dwca_AFR_001"]]
  occ <- afr$data[["occurrence.txt"]]

  expect_s3_class(occ, "data.frame")
  expect_equal(occ$family, "Fabaceae")
  expect_equal(occ$genus, "Inga")
  expect_equal(occ$taxonRank, "FAMILY")
  expect_true(all(c("kingdom", "division", "class", "order",
                    "species", "taxonName") %in% names(occ)))
  expect_true(all(is.na(occ[c("kingdom", "division", "class", "order")])))

  expect_true("summary_AFR" %in% names(afr))
  expect_equal(afr$summary_AFR$collectionCode, "AFR")
  expect_true("summary_RB" %in% names(result[["dwca_JBRJ_002"]]))

  unlink(tmp, recursive = TRUE)
})


test_that("jabot_parse filters herbarium acronyms and maps JBRJ to RB", {
  tmp <- tempfile("jabot_parse_filter_")
  dir.create(tmp)
  .parse_test_fixture(tmp)

  calls <- new.env(parent = emptyenv())
  calls$inputs <- character()

  mock_read <- function(input, read, encoding, na.strings) {
    calls$inputs <- c(calls$inputs, input)
    .mock_jabot_dwca_read(input, read, encoding, na.strings)
  }

  testthat::local_mocked_bindings(
    .arg_check_path = function(...) invisible(TRUE),
    .std_inside_columns = function(df, herbarium = NULL, i = NULL, verbose = TRUE) df,
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    dwca_read = mock_read,
    .package = "finch"
  )

  result <- jabot_parse(
    path = tmp,
    herbarium = "RB",
    verbose = FALSE
  )

  expect_length(result, 1)
  expect_named(result, "dwca_JBRJ_002")
  expect_length(calls$inputs, 1)
  expect_match(calls$inputs, "dwca_JBRJ_002", fixed = TRUE)
  expect_true("summary_RB" %in% names(result[[1]]))

  unlink(tmp, recursive = TRUE)
})


test_that("jabot_parse prints progress messages when verbose = TRUE", {
  tmp <- tempfile("jabot_parse_verbose_")
  dir.create(tmp)
  .parse_test_fixture(tmp)

  testthat::local_mocked_bindings(
    .arg_check_path = function(...) invisible(TRUE),
    .std_inside_columns = function(df, herbarium = NULL, i = NULL, verbose = TRUE) df,
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    dwca_read = .mock_jabot_dwca_read,
    .package = "finch"
  )

  expect_message(
    jabot_parse(path = tmp, herbarium = "AFR", verbose = TRUE),
    "Parsing data from dwca folders"
  )

  unlink(tmp, recursive = TRUE)
})
