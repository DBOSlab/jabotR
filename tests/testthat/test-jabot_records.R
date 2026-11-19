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


# Offline tests: downloads and parsing are mocked ----

# Synthetic occurrence table shaped like jabot_parse()'s standardized
# "occurrence.txt" (same column names/types the merge step expects).
.records_occ <- function(collection, rows) {
  n <- nrow(rows)
  data.frame(
    id = paste0("urn:catalog:", collection, ":", seq_len(n)),
    occurrenceID = paste0(collection, "-", seq_len(n)),
    institutionCode = "JBRJ",
    collectionCode = collection,
    catalogNumber = as.character(seq_len(n)),
    taxonRank = rows$taxonRank,
    kingdom = NA,
    family = rows$family,
    genus = rows$genus,
    specificEpithet = rows$specificEpithet,
    species = ifelse(is.na(rows$specificEpithet), NA_character_,
                     paste(rows$genus, rows$specificEpithet)),
    taxonName = ifelse(is.na(rows$specificEpithet),
                       ifelse(is.na(rows$genus), rows$family, rows$genus),
                       paste(rows$genus, rows$specificEpithet)),
    recordedBy = rows$recordedBy,
    # numeric on purpose: .merge_occur_txt() must coerce to character
    recordNumber = rows$recordNumber,
    eventDate = paste0(rows$year, "-01-01"),
    year = as.integer(rows$year),
    month = 1L,
    day = 1L,
    country = "Brasil",
    stateProvince = rows$stateProvince,
    municipality = "Somewhere",
    minimumElevationInMeters = 10,
    maximumElevationInMeters = 20,
    decimalLatitude = "-12.5",
    decimalLongitude = "-38.5",
    verbatimLatitude = NA,
    verbatimLongitude = NA,
    stringsAsFactors = FALSE
  )
}

.records_dwca <- function() {
  afr <- data.frame(
    taxonRank = c("SPECIES", "GENUS", "FAMILY"),
    family = c("Fabaceae", "Fabaceae", "Fabaceae"),
    genus = c("Inga", "Inga", NA),
    specificEpithet = c("edulis", NA, NA),
    recordedBy = c("Silva", "Abreu", "Costa"),
    recordNumber = c(10, 5, 7),
    stateProvince = c("Bahia", "Minas Gerais", "Bahia"),
    year = c(2001, 1995, 2010),
    stringsAsFactors = FALSE
  )
  rb <- data.frame(
    taxonRank = c("SPECIES", "FAMILY", "SPECIES"),
    family = c("Ochnaceae", "Ochnaceae", "Fabaceae"),
    genus = c("Ouratea", NA, "Mimosa"),
    specificEpithet = c("castaneifolia", NA, "pudica"),
    recordedBy = c("Abreu", "Lima", "Silva"),
    recordNumber = c(3, 1, 2),
    stateProvince = c("Rio de Janeiro", "Bahia", "Minas Gerais"),
    year = c(2001, 1980, 2020),
    stringsAsFactors = FALSE
  )
  list(
    dwca_AFR_001 = list(
      data = list("occurrence.txt" = .records_occ("AFR", afr)),
      files = list(xml_files = "eml.xml"),
      summary_AFR = data.frame(collectionCode = "AFR", Records = 3)
    ),
    dwca_JBRJ_002 = list(
      data = list("occurrence.txt" = .records_occ("RB", rb)),
      files = list(xml_files = "eml.xml"),
      summary_RB = data.frame(collectionCode = "RB", Records = 3)
    )
  )
}

# Mocks the network-touching pieces: herbarium validation metadata, the
# download and the parser. Records every call in the returned environment.
.local_records_mocks <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$download <- list()
  calls$parse <- list()

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) {
      list(list("meta-afr", "meta-rb"), c("afr", "jbrj_rb"), c("AFR", "RB"))
    },
    jabot_download = function(herbarium = NULL, verbose = TRUE, dir = NULL) {
      calls$download[[length(calls$download) + 1]] <-
        list(herbarium = herbarium, verbose = verbose, dir = dir)
      invisible(NULL)
    },
    jabot_parse = function(path = NULL, herbarium = NULL, verbose = TRUE) {
      calls$parse[[length(calls$parse) + 1]] <-
        list(path = path, herbarium = herbarium, verbose = verbose)
      dwca <- .records_dwca()
      if (!is.null(herbarium)) {
        code <- c(dwca_AFR_001 = "AFR", dwca_JBRJ_002 = "RB")
        dwca <- dwca[code[names(dwca)] %in% herbarium]
      }
      dwca
    },
    .package = "jabotR",
    .env = env
  )
  calls
}


test_that("offline: jabot_records with path = NULL downloads into 'jabot_download'", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_records_mocks()

  result <- jabot_records(save = FALSE, verbose = FALSE)

  expect_length(calls$download, 1)
  expect_equal(calls$download[[1]]$dir, "jabot_download")
  expect_null(calls$download[[1]]$herbarium)
  expect_equal(calls$parse[[1]]$path, "jabot_download")

  # All six records from both archives are merged
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)
  expect_setequal(result$collectionCode, c("AFR", "RB"))

  # .merge_occur_txt() type coercions
  expect_type(result$recordNumber, "character")
  expect_type(result$year, "character")
  expect_type(result$month, "character")
  expect_type(result$eventDate, "character")
  expect_type(result$minimumElevationInMeters, "character")
  expect_type(result$decimalLatitude, "double")
  expect_type(result$decimalLongitude, "double")

  # All-NA columns are dropped
  expect_false(any(c("kingdom", "verbatimLatitude", "verbatimLongitude") %in%
                     names(result)))

  # Default reorder: collectionCode, family, genus, specificEpithet, ...
  expect_equal(result$collectionCode, c("AFR", "AFR", "AFR", "RB", "RB", "RB"))
  expect_equal(result$taxonName[1:3], c("Inga edulis", "Inga", "Fabaceae"))
  expect_equal(result$taxonName[4:6],
               c("Mimosa pudica", "Ouratea castaneifolia", "Ochnaceae"))

  # Nothing written when save = FALSE
  expect_false(dir.exists("jabot_records"))
})


test_that("offline: jabot_records with a path and updates = TRUE refreshes that path", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_records_mocks()

  suppressMessages(expect_message(
    result <- jabot_records(herbarium = "RB", path = "my_dwca",
                            updates = TRUE, verbose = TRUE, save = FALSE),
    "Updating dwca files within 'my_dwca'"
  ))

  expect_length(calls$download, 1)
  expect_equal(calls$download[[1]]$dir, "my_dwca")
  expect_equal(calls$download[[1]]$herbarium, "RB")
  expect_true(calls$download[[1]]$verbose)
  expect_equal(calls$parse[[1]]$path, "my_dwca")
  expect_equal(calls$parse[[1]]$herbarium, "RB")

  expect_equal(unique(result$collectionCode), "RB")
  expect_equal(nrow(result), 3)
})


test_that("offline: jabot_records with a path and updates = FALSE skips download", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_records_mocks()

  result <- jabot_records(path = "my_dwca", updates = FALSE,
                          verbose = FALSE, save = FALSE)

  expect_length(calls$download, 0)
  expect_equal(calls$parse[[1]]$path, "my_dwca")
  expect_equal(nrow(result), 6)
})


test_that("offline: jabot_records rejects an unknown herbarium before downloading", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_records_mocks()

  expect_error(
    jabot_records(herbarium = "XYZ", save = FALSE, verbose = FALSE),
    "not recognized by JABOT: .XYZ."
  )
  expect_length(calls$download, 0)
  expect_length(calls$parse, 0)
})


test_that("offline: jabot_records validates recordYear and dir arguments", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_records_mocks()

  expect_error(jabot_records(recordYear = "20", save = FALSE, verbose = FALSE),
               "4-digit")
  expect_error(jabot_records(recordYear = c("2020", "2000"), save = FALSE,
                             verbose = FALSE),
               "first year must be less")
  expect_error(jabot_records(recordYear = c("2000", "2001", "2002"),
                             save = FALSE, verbose = FALSE),
               "single year or a range")
  expect_error(jabot_records(dir = 1, save = FALSE, verbose = FALSE),
               "dir should be a character")
  expect_length(calls$download, 0)
})


test_that("offline: jabot_records filters by family, genus and species names", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  fam <- jabot_records(taxon = "Ochnaceae", save = FALSE, verbose = FALSE)
  expect_equal(nrow(fam), 2)
  expect_true(all(fam$family == "Ochnaceae"))

  gen <- jabot_records(taxon = "Inga", save = FALSE, verbose = FALSE)
  expect_equal(nrow(gen), 2)
  expect_true(all(gen$genus == "Inga"))

  spp <- jabot_records(taxon = c("Mimosa pudica", "Ouratea castaneifolia"),
                       save = FALSE, verbose = FALSE)
  expect_setequal(spp$taxonName, c("Mimosa pudica", "Ouratea castaneifolia"))
  expect_equal(nrow(spp), 2)

  mixed <- jabot_records(taxon = c("Ochnaceae", "Mimosa", "Inga edulis"),
                         save = FALSE, verbose = FALSE)
  expect_equal(nrow(mixed), 4)
  expect_setequal(mixed$taxonName, c("Ouratea castaneifolia", "Ochnaceae",
                                     "Mimosa pudica", "Inga edulis"))
})


test_that("offline: jabot_records reports unmatched taxa and errors when none match", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  suppressMessages(expect_message(
    res <- jabot_records(taxon = c("Inga", "Fakeplantus"), save = FALSE,
                         verbose = TRUE),
    "not found in any column: Fakeplantus"
  ))
  expect_equal(nrow(res), 2)

  suppressMessages(expect_message(
    jabot_records(taxon = "Inga", save = FALSE, verbose = TRUE),
    "Filtering taxon names"
  ))

  expect_error(
    jabot_records(taxon = "Fakeplantus invalidus", save = FALSE,
                  verbose = FALSE),
    "at least one name existing"
  )
})


test_that("offline: jabot_records filters states given as acronyms or names", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  msgs <- testthat::capture_messages(
    res <- jabot_records(state = c("BA", "Rio de Janeiro"), save = FALSE,
                         verbose = TRUE)
  )
  expect_true(any(grepl("valid Brazilian state", msgs)))
  expect_true(any(grepl("Filtering states", msgs)))
  expect_setequal(unique(res$stateProvince), c("Bahia", "Rio de Janeiro"))
  expect_equal(nrow(res), 4)

  suppressMessages(expect_message(
    jabot_records(state = c("Bahia", "SP"), save = FALSE, verbose = TRUE),
    "states were not found: S\u00e3o Paulo"
  ))

  expect_error(
    jabot_records(state = "SP", save = FALSE, verbose = FALSE),
    "at least one name existing"
  )
})


test_that("offline: jabot_records filters a single recordYear or a year range", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  msgs <- testthat::capture_messages(
    one <- jabot_records(recordYear = "2001", save = FALSE, verbose = TRUE)
  )
  expect_true(any(grepl("recordYear range exists", msgs)))
  expect_true(any(grepl("Filtering recordYear", msgs)))
  expect_equal(nrow(one), 2)
  expect_true(all(one$year == "2001"))

  rng <- jabot_records(recordYear = c("1995", "2010"), save = FALSE,
                       verbose = FALSE)
  expect_setequal(rng$year, c("1995", "2001", "2010"))
  expect_equal(nrow(rng), 4)

  suppressMessages(expect_message(
    jabot_records(recordYear = c("2001", "2005"), save = FALSE, verbose = TRUE),
    "recordYear were not found: 2005"
  ))

  expect_error(
    jabot_records(recordYear = "1900", save = FALSE, verbose = FALSE),
    "at least one year existing"
  )
})


test_that("offline: jabot_records combines taxon, state and year filters", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  res <- jabot_records(taxon = "Fabaceae", state = "BA",
                       recordYear = c("2001", "2010"),
                       save = FALSE, verbose = FALSE)

  expect_equal(nrow(res), 2)
  expect_true(all(res$family == "Fabaceae"))
  expect_true(all(res$stateProvince == "Bahia"))
  expect_setequal(res$year, c("2001", "2010"))
})


test_that("offline: jabot_records indets = FALSE drops family-level records", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  res <- jabot_records(indets = FALSE, save = FALSE, verbose = FALSE)

  expect_false("FAMILY" %in% res$taxonRank)
  expect_true(all(c("Inga edulis", "Mimosa pudica", "Ouratea castaneifolia") %in%
                    res$taxonName))
  expect_equal(nrow(res), 4)

  all_res <- jabot_records(indets = TRUE, save = FALSE, verbose = FALSE)
  expect_equal(sum(all_res$taxonRank == "FAMILY"), 2)
})


test_that("offline: jabot_records honours each reorder option", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  by_year <- jabot_records(reorder = "year", save = FALSE, verbose = FALSE)
  expect_equal(by_year$year, sort(by_year$year))

  by_herb <- jabot_records(reorder = "herbarium", save = FALSE, verbose = FALSE)
  expect_equal(by_herb$collectionCode, sort(by_herb$collectionCode))

  # "collector" as the last element
  by_coll <- jabot_records(reorder = c("year", "collector"), save = FALSE,
                           verbose = FALSE)
  expect_equal(by_coll$year, sort(by_coll$year))
  expect_equal(by_coll$recordedBy[by_coll$year == "2001"], c("Abreu", "Silva"))

  # "area" in the middle of the vector
  by_area <- jabot_records(reorder = c("herbarium", "area", "year"),
                           save = FALSE, verbose = FALSE)
  afr <- by_area[by_area$collectionCode == "AFR", ]
  expect_equal(afr$stateProvince, c("Bahia", "Bahia", "Minas Gerais"))
  expect_equal(afr$year[afr$stateProvince == "Bahia"], c("2001", "2010"))

  # "taxa" first, followed by other keys
  by_taxa <- jabot_records(reorder = c("taxa", "year"), save = FALSE,
                           verbose = FALSE)
  expect_equal(by_taxa$family, sort(by_taxa$family))
  expect_equal(by_taxa$taxonName[1:4],
               c("Inga edulis", "Inga", "Mimosa pudica", "Fabaceae"))

  # reorder = NULL falls back to the default ordering
  by_null <- jabot_records(reorder = NULL, save = FALSE, verbose = FALSE)
  by_default <- jabot_records(save = FALSE, verbose = FALSE)
  expect_equal(by_null, by_default)
})


test_that("offline: jabot_records save = TRUE writes CSV and appends to log", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()

  msgs <- testthat::capture_messages(
    res <- jabot_records(herbarium = c("AFR", "RB"), path = "dwca",
                         updates = FALSE, save = TRUE, verbose = TRUE,
                         dir = "out/", filename = "my_search")
  )
  expect_true(any(grepl("Creating directory 'out'", msgs)))
  expect_true(any(grepl("Writing spreadsheet 'my_search.csv' within 'out'",
                        msgs)))

  expect_true(file.exists(file.path("out", "my_search.csv")))
  csv <- utils::read.csv(file.path("out", "my_search.csv"),
                         stringsAsFactors = FALSE)
  expect_equal(nrow(csv), nrow(res))
  expect_equal(names(csv), names(res))
  expect_equal(csv$occurrenceID, res$occurrenceID)

  log <- readLines(file.path("out", "log.txt"))
  expect_true(any(grepl("Downloaded: AFR, RB | Records saved to: out/my_search.csv",
                        log, fixed = TRUE)))
  expect_true(any(grepl("Total records: 6", log)))
  expect_true(any(grepl("Records per genus:", log)))
  # More than one genus: the per-species block is omitted
  expect_false(any(grepl("Records per species:", log)))

  # A second run with a single genus appends a species breakdown
  jabot_records(taxon = "Inga", path = "dwca", updates = FALSE,
                save = TRUE, verbose = FALSE, dir = "out",
                filename = "inga")
  log2 <- readLines(file.path("out", "log.txt"))
  expect_gt(length(log2), length(log))
  expect_equal(sum(grepl("Total records:", log2)), 2)
  expect_true(any(grepl("Downloaded: ALL | Records saved to: out/inga.csv",
                        log2, fixed = TRUE)))
  expect_true(any(grepl("Records per species:", log2)))
  expect_true(file.exists(file.path("out", "inga.csv")))
})


test_that("offline: jabot_records save = TRUE reuses an existing directory quietly", {
  withr::local_dir(withr::local_tempdir())
  .local_records_mocks()
  dir.create("existing")

  msgs <- testthat::capture_messages(
    jabot_records(save = TRUE, verbose = TRUE, dir = "existing",
                  filename = "f", path = "dwca", updates = FALSE)
  )
  expect_false(any(grepl("Creating directory", msgs)))
  expect_true(file.exists(file.path("existing", "f.csv")))

  # verbose = FALSE emits no messages at all
  expect_silent(
    jabot_records(save = TRUE, verbose = FALSE, dir = "existing",
                  filename = "g", path = "dwca", updates = FALSE)
  )
  expect_true(file.exists(file.path("existing", "g.csv")))
})
