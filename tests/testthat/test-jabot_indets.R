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
# Offline tests: downloads and parsing are mocked ----

# Synthetic occurrence table shaped like jabot_parse()'s standardized
# "occurrence.txt" (same column names/types the merge step expects).
.indets_occ <- function(collection, rows) {
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

.indets_dwca <- function() {
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
      data = list("occurrence.txt" = .indets_occ("AFR", afr)),
      files = list(xml_files = "eml.xml"),
      summary_AFR = data.frame(collectionCode = "AFR", Records = 3)
    ),
    dwca_JBRJ_002 = list(
      data = list("occurrence.txt" = .indets_occ("RB", rb)),
      files = list(xml_files = "eml.xml"),
      summary_RB = data.frame(collectionCode = "RB", Records = 3)
    )
  )
}

# Mocks the network-touching pieces: herbarium validation metadata, the
# download and the parser. Records every call in the returned environment.
.local_indets_mocks <- function(env = parent.frame()) {
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
      dwca <- .indets_dwca()
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


test_that("offline: jabot_indets keeps higher-rank indeterminates by default", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_indets_mocks()

  res <- jabot_indets(save = FALSE, verbose = FALSE)

  expect_length(calls$download, 1)
  expect_equal(calls$download[[1]]$dir, "jabot_download")
  expect_equal(calls$parse[[1]]$path, "jabot_download")

  expect_equal(nrow(res), 3)
  expect_setequal(res$taxonRank, c("GENUS", "FAMILY"))
  expect_setequal(res$taxonName, c("Inga", "Fabaceae", "Ochnaceae"))
  # Default reorder: by collectionCode, then family/genus
  expect_equal(res$collectionCode, c("AFR", "AFR", "RB"))
  expect_equal(res$taxonName, c("Inga", "Fabaceae", "Ochnaceae"))
  # All-NA columns (specificEpithet, species, kingdom...) are dropped
  expect_false(any(c("specificEpithet", "species", "kingdom") %in% names(res)))

  # save = FALSE writes no CSV/log, but the output dir is still created
  expect_true(dir.exists("jabot_indets"))
  expect_length(list.files("jabot_indets"), 0)
})


test_that("offline: jabot_indets filters by level FAMILY or GENUS", {
  withr::local_dir(withr::local_tempdir())
  .local_indets_mocks()

  suppressMessages(expect_message(
    fam <- jabot_indets(level = " family ", save = FALSE, verbose = TRUE),
    "Checking whether the input level is correct"
  ))
  expect_equal(nrow(fam), 2)
  expect_true(all(fam$taxonRank == "FAMILY"))

  gen <- jabot_indets(level = "GENUS", save = FALSE, verbose = FALSE)
  expect_equal(nrow(gen), 1)
  expect_equal(gen$taxonName, "Inga")
  expect_equal(gen$taxonRank, "GENUS")
})


test_that("offline: jabot_indets rejects invalid level and herbarium", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_indets_mocks()

  expect_error(jabot_indets(level = "SPECIES", save = FALSE, verbose = FALSE),
               "invalid: .SPECIES.")
  expect_error(jabot_indets(herbarium = "XYZ", save = FALSE, verbose = FALSE),
               "not recognized by JABOT: .XYZ.")
  expect_error(jabot_indets(dir = TRUE, save = FALSE, verbose = FALSE),
               "dir should be a character")
  expect_error(jabot_indets(recordYear = "abcd", save = FALSE, verbose = FALSE),
               "4-digit")
  expect_length(calls$download, 0)
  expect_length(calls$parse, 0)
})


test_that("offline: jabot_indets with a path honours updates TRUE/FALSE", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_indets_mocks()

  suppressMessages(expect_message(
    res <- jabot_indets(herbarium = "AFR", path = "dwca", updates = TRUE,
                        save = FALSE, verbose = TRUE),
    "Updating dwca files within 'dwca'"
  ))
  expect_length(calls$download, 1)
  expect_equal(calls$download[[1]]$dir, "dwca")
  expect_equal(calls$download[[1]]$herbarium, "AFR")
  expect_equal(calls$parse[[1]]$path, "dwca")
  expect_equal(calls$parse[[1]]$herbarium, "AFR")
  expect_equal(unique(res$collectionCode), "AFR")
  expect_equal(nrow(res), 2)

  res2 <- jabot_indets(path = "dwca", updates = FALSE, save = FALSE,
                       verbose = FALSE)
  expect_length(calls$download, 1)
  expect_length(calls$parse, 2)
  expect_equal(nrow(res2), 3)
})


test_that("offline: jabot_indets applies taxon, state and recordYear filters", {
  withr::local_dir(withr::local_tempdir())
  .local_indets_mocks()

  fab <- jabot_indets(taxon = "Fabaceae", save = FALSE, verbose = FALSE)
  expect_equal(nrow(fab), 2)
  expect_true(all(fab$family == "Fabaceae"))

  msgs <- testthat::capture_messages(
    ba <- jabot_indets(state = "BA", save = FALSE, verbose = TRUE)
  )
  expect_true(any(grepl("input state list exists in the JABOT", msgs)))
  expect_true(any(grepl("Filtering states", msgs)))
  expect_setequal(ba$taxonName, c("Fabaceae", "Ochnaceae"))
  expect_true(all(ba$stateProvince == "Bahia"))

  msgs <- testthat::capture_messages(
    y <- jabot_indets(recordYear = "1995", save = FALSE, verbose = TRUE)
  )
  expect_true(any(grepl("recordYear range exists", msgs)))
  expect_equal(y$taxonName, "Inga")

  rng <- jabot_indets(recordYear = c("1980", "2010"), save = FALSE,
                      verbose = FALSE)
  expect_setequal(rng$year, c("1980", "1995", "2010"))

  combo <- jabot_indets(level = "FAMILY", taxon = "Ochnaceae", state = "Bahia",
                        recordYear = "1980", save = FALSE, verbose = FALSE)
  expect_equal(nrow(combo), 1)
  expect_equal(combo$collectionCode, "RB")

  # Filters are applied after the rank filter: a species name never matches
  expect_error(
    jabot_indets(taxon = "Inga edulis", save = FALSE, verbose = FALSE),
    "at least one name existing"
  )
  expect_error(
    jabot_indets(state = "Rio de Janeiro", save = FALSE, verbose = FALSE),
    "at least one name existing"
  )
})


test_that("offline: jabot_indets honours reorder", {
  withr::local_dir(withr::local_tempdir())
  .local_indets_mocks()

  by_year <- jabot_indets(reorder = "year", save = FALSE, verbose = FALSE)
  expect_equal(by_year$year, c("1980", "1995", "2010"))

  by_coll <- jabot_indets(reorder = c("herbarium", "collector"), save = FALSE,
                          verbose = FALSE)
  expect_equal(by_coll$recordedBy, c("Abreu", "Costa", "Lima"))
})


test_that("offline: jabot_indets save = TRUE writes CSV and log", {
  withr::local_dir(withr::local_tempdir())
  .local_indets_mocks()

  msgs <- testthat::capture_messages(
    res <- jabot_indets(save = TRUE, verbose = TRUE, dir = "indets_out/",
                        filename = "my_indets")
  )
  expect_true(any(grepl("Creating directory 'indets_out'", msgs)))
  expect_true(any(grepl("Writing spreadsheet 'my_indets.csv'", msgs)))

  csv <- utils::read.csv(file.path("indets_out", "my_indets.csv"),
                         stringsAsFactors = FALSE)
  expect_equal(csv$occurrenceID, res$occurrenceID)
  expect_equal(names(csv), names(res))

  log <- readLines(file.path("indets_out", "log.txt"))
  expect_true(any(grepl("Downloaded: ALL | Records saved to: indets_out/my_indets.csv",
                        log, fixed = TRUE)))
  expect_true(any(grepl("Total records: 3", log)))

  # Existing directory: no "Creating directory" message
  msgs2 <- testthat::capture_messages(
    jabot_indets(save = TRUE, verbose = TRUE, dir = "indets_out",
                 filename = "again")
  )
  expect_false(any(grepl("Creating directory", msgs2)))
  expect_true(file.exists(file.path("indets_out", "again.csv")))
})
