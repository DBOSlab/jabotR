test_that("jabot_fieldbook locates the local application", {

  path <- jabot_fieldbook(browser = FALSE)

  expect_type(path, "character")
  expect_length(path, 1L)
  expect_true(file.exists(path))
  expect_equal(basename(path), "index.html")
})


test_that("jabot_fieldbook validates browser argument", {

  expect_error(
    jabot_fieldbook(browser = "yes"),
    "`browser` must be TRUE or FALSE"
  )

  expect_error(
    jabot_fieldbook(browser = NA),
    "`browser` must be TRUE or FALSE"
  )

  expect_error(
    jabot_fieldbook(browser = c(TRUE, FALSE)),
    "`browser` must be TRUE or FALSE"
  )
})


test_that("jabot_fieldbook required assets are installed", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  expected_files <- c(
    "index.html",
    "styles.css",
    "app.js",
    "jabot-translations.js",
    "jabot-language.js",
    "jabotr_hex_sticker.png",
    "jabot_original_logo.png",
    file.path("vendor", "xlsx.full.min.js")
  )

  for (file in expected_files) {

    expect_true(
      file.exists(file.path(root, file)),
      info = paste("Missing jabot_fieldbook asset:", file)
    )
  }
})


test_that("jabot_fieldbook index references local assets", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  index <- readLines(
    file.path(root, "index.html"),
    warn = FALSE,
    encoding = "UTF-8"
  )

  index <- paste(index, collapse = "\n")

  expect_match(
    index,
    'src="vendor/xlsx\\.full\\.min\\.js"'
  )

  expect_match(
    index,
    'src="jabot-translations\\.js"'
  )

  expect_match(
    index,
    'src="jabot-language\\.js"'
  )

  expect_match(
    index,
    'src="app\\.js"'
  )

  expect_match(
    index,
    'href="styles\\.css"'
  )
})


test_that("jabot_fieldbook contains both final outputs", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  index <- paste(
    readLines(
      file.path(root, "index.html"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  expect_match(
    index,
    'data-action="export-filled"'
  )

  expect_match(
    index,
    'data-action="export-fieldbook-pdf"'
  )
})


test_that("jabot_fieldbook app implements collection events and exports", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  app <- paste(
    readLines(
      file.path(root, "app.js"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  expect_match(
    app,
    "function addEvent\\("
  )

  expect_match(
    app,
    "function removeEvent\\("
  )

  expect_match(
    app,
    "function addSpecimen\\("
  )

  expect_match(
    app,
    "function exportFilledWorkbook\\("
  )

  expect_match(
    app,
    "function exportFieldbookPDF\\("
  )

  expect_match(
    app,
    '"export-filled"\\s*:\\s*exportFilledWorkbook'
  )

  expect_match(
    app,
    '"export-fieldbook-pdf"\\s*:\\s*exportFieldbookPDF'
  )
})


test_that("jabot_fieldbook includes the 58 JABOT columns", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  app <- paste(
    readLines(
      file.path(root, "app.js"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  expected_columns <- c(
    "numtombo", "sufixo", "family", "genus", "cf", "sp1",
    "author1", "rank1", "sp2", "author2", "rank2", "sp3",
    "author3", "vernacular", "typestat", "country", "majorarea",
    "minorarea", "gazetteer", "uc", "latitude", "longitude",
    "lat_grau", "lat_min", "lat_seg", "ns", "long_grau",
    "long_min", "long_seg", "ew", "altprof", "altprofmax",
    "unidmedaltprof", "locnotes", "flor", "fruto", "fuste",
    "altura", "unidmedaltura", "collector", "number", "addcoll",
    "colldd", "collmm", "collyy", "detby", "detdd", "detmm",
    "detyy", "sigla_colbot_origem", "dups", "nrdups", "notes",
    "usos", "uso_especifico", "projeto", "habitat", "habito"
  )

  expect_length(expected_columns, 58L)

  for (column in expected_columns) {
    expect_match(
      app,
      paste0('"', column, '"'),
      fixed = TRUE
    )
  }
})


test_that("jabot_fieldbook has Portuguese as default language", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  translations <- paste(
    readLines(
      file.path(root, "jabot-translations.js"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  expect_match(
    translations,
    'defaultLanguage:\\s*"pt"'
  )

  expect_match(
    translations,
    'availableLanguages:\\s*\\["pt",\\s*"en"\\]'
  )
})


test_that("jabot_fieldbook own assets do not require internet access", {

  root <- dirname(
    jabot_fieldbook(browser = FALSE)
  )

  own_files <- file.path(
    root,
    c(
      "index.html",
      "styles.css",
      "app.js",
      "jabot-translations.js",
      "jabot-language.js"
    )
  )

  contents <- paste(
    unlist(
      lapply(
        own_files,
        readLines,
        warn = FALSE,
        encoding = "UTF-8"
      )
    ),
    collapse = "\n"
  )

  expect_false(
    grepl(
      "https?://",
      contents,
      ignore.case = TRUE
    )
  )
})


# ---- merge = TRUE ----------------------------------------------------------

fixture_dir <- function() {
  dir <- tempfile("fieldbook_fixture_")
  dir.create(dir)
  dir
}

# Writes a PDF carrying the same embedded data block that the field notebook
# app prints at the end of every field notebook PDF.
write_fieldbook_fixture <- function(file, payload, wrap = 60L) {

  json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  b64 <- jsonlite::base64_enc(charToRaw(enc2utf8(as.character(json))))
  b64 <- gsub("[[:space:]]+", "", b64)

  block <- paste0(
    "JABOTR-FIELDBOOK-DATA-BEGIN:", nchar(b64), ":", b64,
    ":JABOTR-FIELDBOOK-DATA-END"
  )

  starts <- seq(1L, nchar(block), by = wrap)
  lines <- substring(block, starts, pmin(starts + wrap - 1L, nchar(block)))

  grDevices::pdf(file, width = 8.27, height = 11.69)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::text(0.02, 0.98, "Domingos Cardoso 2020-2022", adj = c(0, 1))
  y <- 0.9 - (seq_along(lines) - 1) * 0.02
  graphics::text(0.02, y, lines, adj = c(0, 1), cex = 0.6, family = "mono")
  grDevices::dev.off()

  invisible(file)
}


fixture_payload <- function() {
  list(
    format = "jabotR-fieldbook",
    version = 1L,
    created = "2025-05-04T12:00:00.000Z",
    events = list(
      list(
        header = list(
          colldd = "3", collmm = "5", collyy = "2025",
          country = "Brasil", majorarea = "Bahia", minorarea = "Ilhéus",
          collector = "Domingos Cardoso"
        ),
        specimens = list(
          list(number = "2020", family = "Fabaceae", genus = "Inga"),
          list(number = "2021", family = "Myrtaceae")
        )
      ),
      list(
        header = list(country = "Brasil", collector = "Domingos Cardoso"),
        specimens = list(list(number = "2022", family = "Annonaceae"))
      )
    )
  )
}


test_that("jabot_fieldbook validates merge arguments", {

  expect_error(
    jabot_fieldbook(browser = FALSE, merge = "yes"),
    "`merge` must be TRUE or FALSE"
  )

  expect_error(
    jabot_fieldbook(browser = FALSE, merge = NA),
    "`merge` must be TRUE or FALSE"
  )

  expect_error(
    jabot_fieldbook(browser = FALSE, merge = TRUE, pdf_path = 1),
    "`pdf_path` must be NULL"
  )

  expect_error(
    jabot_fieldbook(browser = FALSE, merge = TRUE, pdf_path = ""),
    "`pdf_path` must be NULL"
  )
})


test_that("the app embeds field notebook data using the R markers", {

  root <- dirname(jabot_fieldbook(browser = FALSE))

  app <- paste(
    readLines(file.path(root, "app.js"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  expect_match(app, .fieldbook_data_begin, fixed = TRUE)
  expect_match(app, .fieldbook_data_end, fixed = TRUE)
  expect_match(app, paste0('"', .fieldbook_data_format, '"'), fixed = TRUE)
  expect_match(app, "window.JABOTR_FIELDBOOK_MERGE", fixed = TRUE)
  expect_match(app, "function applyMergeData(", fixed = TRUE)
})


test_that("embedded data is read back from a field notebook PDF", {

  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")

  dir <- fixture_dir()
  pdf <- file.path(dir, "fieldbook_Cardoso2020-2022.pdf")
  write_fieldbook_fixture(pdf, fixture_payload())

  found <- .fieldbook_read_pdf(pdf)

  expect_false(is.null(found))
  expect_equal(found$payload$format, "jabotR-fieldbook")
  expect_length(found$payload$events, 2L)
  expect_equal(
    found$payload$events[[1]]$header$minorarea,
    "Ilhéus"
  )

  info <- .fieldbook_summary(found$payload)

  expect_equal(info$events, 2L)
  expect_equal(info$specimens, 3L)
  expect_equal(info$last, 2022)
})


test_that("merge = TRUE builds an app copy carrying the previous notebook", {

  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")

  dir <- fixture_dir()
  pdf <- file.path(dir, "fieldbook_Cardoso2020-2022.pdf")
  write_fieldbook_fixture(pdf, fixture_payload())

  expect_message(
    app <- jabot_fieldbook(browser = FALSE, merge = TRUE, pdf_path = dir),
    "continue from 2023"
  )

  installed <- jabot_fieldbook(browser = FALSE)

  expect_false(identical(app, installed))
  expect_true(file.exists(app))
  expect_true(file.exists(file.path(dirname(app), "app.js")))

  index <- paste(
    readLines(app, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  # The merge data must load before app.js.
  merge_pos <- regexpr('src="jabot-merge-data.js"', index, fixed = TRUE)
  app_pos <- regexpr('src="app.js"', index, fixed = TRUE)

  expect_gt(merge_pos, 0)
  expect_gt(app_pos, merge_pos)

  merge_js <- paste(
    readLines(file.path(dirname(app), "jabot-merge-data.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_true(startsWith(merge_js, "window.JABOTR_FIELDBOOK_MERGE = {"))
  expect_match(merge_js, "fieldbook_Cardoso2020-2022.pdf", fixed = TRUE)
})


test_that("merge = TRUE uses the most recent field notebook", {

  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")

  dir <- fixture_dir()

  old <- fixture_payload()
  old$events <- old$events[1]
  old_pdf <- file.path(dir, "fieldbook_Cardoso2020-2021.pdf")
  write_fieldbook_fixture(old_pdf, old)
  Sys.setFileTime(old_pdf, Sys.time() - 3600)

  new_pdf <- file.path(dir, "fieldbook_Cardoso2020-2022.pdf")
  write_fieldbook_fixture(new_pdf, fixture_payload())

  found <- suppressMessages(.fieldbook_find_previous(dir))

  expect_equal(basename(found$file), basename(new_pdf))
})


test_that("merge = TRUE without a previous notebook warns or errors", {

  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")

  dir <- fixture_dir()

  expect_warning(
    app <- jabot_fieldbook(browser = FALSE, merge = TRUE, pdf_path = dir),
    "No field notebook PDF"
  )

  expect_identical(app, jabot_fieldbook(browser = FALSE))

  plain <- file.path(dir, "fieldbook_plain.pdf")
  grDevices::pdf(plain)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "not a jabotR field notebook")
  grDevices::dev.off()

  expect_error(
    jabot_fieldbook(browser = FALSE, merge = TRUE, pdf_path = plain),
    "no embedded data"
  )

  expect_error(
    jabot_fieldbook(
      browser = FALSE,
      merge = TRUE,
      pdf_path = file.path(dir, "missing.pdf")
    ),
    "`pdf_path` not found"
  )
})


test_that("merged events are kept out of the JABOT spreadsheet", {

  root <- dirname(jabot_fieldbook(browser = FALSE))

  app <- paste(
    readLines(file.path(root, "app.js"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  # Imported events are flagged on merge ...
  expect_match(app, "imported: true", fixed = TRUE)
  # ... and the spreadsheet is built only from the non-imported ones.
  expect_match(app, "function spreadsheetEvents(", fixed = TRUE)
  expect_match(app, "flattenRows(events)", fixed = TRUE)
  expect_match(app, "collectionFileBaseName(events)", fixed = TRUE)
})


# ---- Additional coverage ---------------------------------------------------

# Writes a PDF with an arbitrary data block (in a monospaced font), optionally
# preceded by an empty page.
write_fieldbook_block <- function(file, block, blank_first_page = FALSE,
                                  wrap = 60L) {
  starts <- seq(1L, nchar(block), by = wrap)
  lines <- substring(block, starts, pmin(starts + wrap - 1L, nchar(block)))

  grDevices::pdf(file, width = 8.27, height = 11.69)
  graphics::par(mar = c(0, 0, 0, 0))
  if (blank_first_page) graphics::plot.new()
  graphics::plot.new()
  graphics::text(0.02, 0.98, "Domingos Cardoso 2020-2022", adj = c(0, 1))
  y <- 0.9 - (seq_along(lines) - 1) * 0.02
  graphics::text(0.02, y, lines, adj = c(0, 1), cex = 0.6, family = "mono")
  grDevices::dev.off()

  invisible(file)
}


fieldbook_block <- function(payload, declared_length = NULL) {
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  b64 <- jsonlite::base64_enc(charToRaw(enc2utf8(as.character(json))))
  b64 <- gsub("[[:space:]]+", "", b64)
  if (is.null(declared_length)) declared_length <- nchar(b64)
  paste0("JABOTR-FIELDBOOK-DATA-BEGIN:", declared_length, ":", b64,
         ":JABOTR-FIELDBOOK-DATA-END")
}


# Mocks base::system.file (or the pkgload shim under devtools::load_all()).
local_fieldbook_system_file <- function(fake, env = parent.frame()) {
  imports <- parent.env(asNamespace("jabotR"))
  if (exists("system.file", envir = imports, inherits = FALSE)) {
    testthat::local_mocked_bindings(system.file = fake, .package = "jabotR",
                                    .env = env)
  } else {
    testthat::local_mocked_bindings(system.file = fake, .package = "base",
                                    .env = env)
  }
}


test_that("jabot_fieldbook(browser = TRUE) opens the app as a file URL", {
  opened <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) opened$url <- c(opened$url, url),
    .package = "utils"
  )

  app <- jabot_fieldbook()

  prefix <- if (.Platform$OS.type == "windows") "file:///" else "file://"
  expect_equal(opened$url, paste0(prefix, utils::URLencode(app, reserved = FALSE)))
  expect_identical(app, jabot_fieldbook(browser = FALSE))
  expect_length(opened$url, 1L)
})


test_that("jabot_fieldbook(browser = TRUE) uses file:// outside Windows", {
  opened <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) opened$url <- url,
    .package = "utils"
  )
  platform <- .Platform
  platform$OS.type <- "unix"

  app <- testthat::with_mocked_bindings(
    jabot_fieldbook(),
    .Platform = platform,
    .package = "base"
  )

  expect_equal(opened$url,
               paste0("file://", utils::URLencode(app, reserved = FALSE)))
  expect_false(startsWith(opened$url, "file:///C:"))
})


test_that("jabot_fieldbook errors when the application is not installed", {
  local_fieldbook_system_file(function(...) "")

  expect_error(
    jabot_fieldbook(browser = FALSE),
    "field notebook could not be found.*Reinstall jabotR"
  )
})


test_that(".fieldbook_require_merge_pkgs reports the missing packages", {
  real <- base::requireNamespace
  missing_mock <- function(gone) {
    function(package, ..., quietly = TRUE) {
      if (package %in% gone) FALSE else real(package, ..., quietly = quietly)
    }
  }

  expect_error(
    testthat::with_mocked_bindings(
      .fieldbook_require_merge_pkgs(),
      requireNamespace = missing_mock("pdftools"),
      .package = "base"
    ),
    "requires the package\\(s\\): pdftools\\.\nInstall with install\\.packages\\(c\\(\"pdftools\"\\)\\)"
  )

  expect_error(
    testthat::with_mocked_bindings(
      .fieldbook_require_merge_pkgs(),
      requireNamespace = missing_mock(c("pdftools", "jsonlite")),
      .package = "base"
    ),
    "pdftools, jsonlite.*\"pdftools\", \"jsonlite\"",
    fixed = FALSE
  )

  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")
  expect_true(.fieldbook_require_merge_pkgs())
})


test_that(".fieldbook_downloads_dir points to the user's Downloads folder", {
  home <- withr::local_tempdir()
  withr::local_envvar(USERPROFILE = home)

  expected <- if (.Platform$OS.type == "windows") {
    file.path(home, "Downloads")
  } else {
    file.path(path.expand("~"), "Downloads")
  }
  expect_equal(.fieldbook_downloads_dir(), expected)

  platform <- .Platform
  platform$OS.type <- "unix"
  unix_dir <- testthat::with_mocked_bindings(
    .fieldbook_downloads_dir(),
    .Platform = platform,
    .package = "base"
  )
  expect_equal(unix_dir, file.path(path.expand("~"), "Downloads"))

  platform$OS.type <- "windows"
  win_dir <- testthat::with_mocked_bindings(
    .fieldbook_downloads_dir(),
    .Platform = platform,
    .package = "base"
  )
  expect_equal(win_dir, file.path(home, "Downloads"))
})


test_that("merge = TRUE searches the working directory and Downloads by default", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")

  wd <- withr::local_tempdir()
  home <- withr::local_tempdir()
  downloads <- file.path(home, "Downloads")
  dir.create(downloads)
  testthat::local_mocked_bindings(
    .fieldbook_downloads_dir = function() downloads,
    .package = "jabotR"
  )
  withr::local_dir(wd)

  # Nothing found anywhere: warning naming both locations
  expect_warning(
    app <- jabot_fieldbook(browser = FALSE, merge = TRUE),
    paste0("found in: .*", basename(wd), ".*", "Downloads",
           ".*Opening an empty field notebook")
  )
  expect_identical(app, jabot_fieldbook(browser = FALSE))
  expect_equal(.fieldbook_candidate_pdfs(), character(0))

  # A notebook in Downloads is picked up
  pdf <- file.path(downloads, "fieldbook_Cardoso2020-2022.pdf")
  write_fieldbook_fixture(pdf, fixture_payload())

  expect_equal(basename(.fieldbook_candidate_pdfs()), basename(pdf))
  expect_message(
    found <- .fieldbook_find_previous(),
    "3 specimen\\(s\\); collection numbers continue from 2023"
  )
  expect_equal(normalizePath(found$file), normalizePath(pdf))
})


test_that(".fieldbook_read_pdf skips empty pages and rejects bad data", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")

  dir <- withr::local_tempdir()

  # An empty first page does not prevent reading the data block
  two_pages <- file.path(dir, "fieldbook_two_pages.pdf")
  write_fieldbook_block(two_pages, fieldbook_block(fixture_payload()),
                        blank_first_page = TRUE)
  expect_equal(pdftools::pdf_info(two_pages)$pages, 2L)
  found <- .fieldbook_read_pdf(two_pages)
  expect_false(is.null(found))
  expect_length(found$payload$events, 2L)

  # Declared length does not match the data: warning and NULL
  truncated <- file.path(dir, "fieldbook_truncated.pdf")
  block <- fieldbook_block(fixture_payload())
  real_len <- nchar(sub("^[^:]*:[0-9]+:([^:]+):.*$", "\\1", block))
  write_fieldbook_block(truncated,
                        fieldbook_block(fixture_payload(), real_len + 4L))
  expect_warning(
    expect_null(.fieldbook_read_pdf(truncated)),
    "fieldbook_truncated.pdf is incomplete and was ignored"
  )

  # Valid block but not a jabotR field notebook payload
  other <- file.path(dir, "fieldbook_other.pdf")
  write_fieldbook_fixture(other, list(format = "something-else",
                                      events = list(list(header = list()))))
  expect_null(.fieldbook_read_pdf(other))

  no_events <- file.path(dir, "fieldbook_no_events.pdf")
  payload <- fixture_payload()
  payload$events <- list()
  write_fieldbook_fixture(no_events, payload)
  expect_null(.fieldbook_read_pdf(no_events))

  # A file that is not a PDF at all
  junk <- file.path(dir, "fieldbook_junk.pdf")
  writeLines("not a pdf", junk)
  expect_null(suppressMessages(suppressWarnings(.fieldbook_read_pdf(junk))))
})


test_that(".fieldbook_merged_app errors when the app copy cannot be prepared", {
  skip_if_not_installed("jsonlite")

  previous <- list(file = "fieldbook_x.pdf", data = "QUJD")

  # index.html without the app.js script tag
  src <- withr::local_tempdir()
  writeLines(c("<html>", "<body>no script</body>", "</html>"),
             file.path(src, "index.html"))
  expect_error(
    .fieldbook_merged_app(file.path(src, "index.html"), previous),
    "Could not prepare the field notebook for merging"
  )

  # Copy of the application fails
  app <- jabot_fieldbook(browser = FALSE)
  expect_error(
    testthat::with_mocked_bindings(
      .fieldbook_merged_app(app, previous),
      file.copy = function(...) FALSE,
      .package = "base"
    ),
    "Could not prepare a copy of the field notebook for merging"
  )
})


test_that(".fieldbook_merged_app injects the merge data before app.js", {
  skip_if_not_installed("jsonlite")

  src <- withr::local_tempdir()
  writeLines(c("<html>", "<body>", '  <script src="app.js" defer></script>',
               "</body>", "</html>"),
             file.path(src, "index.html"))
  writeLines("// app", file.path(src, "app.js"))

  out <- .fieldbook_merged_app(
    file.path(src, "index.html"),
    list(file = file.path(src, "fieldbook_A1-2.pdf"), data = "QUJD")
  )

  index <- readLines(out)
  expect_equal(index[3], '  <script src="jabot-merge-data.js" defer></script>')
  expect_equal(index[4], '  <script src="app.js" defer></script>')
  expect_equal(
    readLines(file.path(dirname(out), "jabot-merge-data.js"))[1],
    'window.JABOTR_FIELDBOOK_MERGE = {"source":"fieldbook_A1-2.pdf","data":"QUJD"};'
  )
  # The original application is left untouched
  expect_length(readLines(file.path(src, "index.html")), 5L)
})
