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
