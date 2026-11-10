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


# Offline tests: the IPT and the downloads are mocked --------------------------

.mock_download_env <- function() {
  env <- new.env(parent = emptyenv())
  env$calls <- list()

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .get_ipt_info = function(herbarium) {
      codes <- c("AFR", "RB", "EMPTY")
      keep <- if (is.null(herbarium)) rep(TRUE, 3) else codes %in% herbarium
      list(list("meta-afr", "meta-rb", "meta-empty")[keep],
           c("afr", "jbrj_rb", "empty")[keep],
           codes[keep])
    },
    .get_herb_info = function(herb_URLs, ipt_metadata, i) {
      records <- c(afr = "1,234", jbrj_rb = "5", empty = "0")[[herb_URLs[i]]]
      list(c("1.5", "2026-01-01", records),
           "Curator", "curator@example.org", "Rights holder",
           paste0("https://ipt.jbrj.gov.br/jabot/resource?r=", herb_URLs[i]))
    },
    .ipt_http_headers = function() c(`User-Agent` = "jabotR-test"),
    .package = "jabotR",
    .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, method, mode, headers, ...) {
      env$calls[[length(env$calls) + 1]] <- list(url = url, destfile = destfile,
                                                 method = method, mode = mode)
      writeLines("zip", destfile)
      invisible(0L)
    },
    unzip = function(zipfile, exdir, ...) {
      dir.create(exdir, recursive = TRUE)
      writeLines("id", file.path(exdir, "occurrence.txt"))
      invisible(NULL)
    },
    .package = "utils",
    .env = parent.frame()
  )
  env
}


test_that("jabot_download downloads the DwC-A in binary mode and writes the summary", {
  tmp <- tempfile("jabot_download_mock_")
  env <- .mock_download_env()

  jabot_download(herbarium = c("AFR", "RB", "EMPTY"), verbose = FALSE, dir = tmp)

  # one download per collection with records; "wb" keeps the zip intact on Windows
  expect_length(env$calls, 2)
  expect_true(all(vapply(env$calls, function(x) identical(x$mode, "wb"), logical(1))))
  expect_true(all(vapply(env$calls, function(x) identical(x$method, "libcurl"),
                         logical(1))))
  expect_equal(env$calls[[1]]$url,
               "https://ipt.jbrj.gov.br/jabot/archive.do?r=afr&v=1.5")
  expect_equal(env$calls[[2]]$url,
               "https://ipt.jbrj.gov.br/jbrj/archive.do?r=jbrj_rb&v=1.5")

  folders <- list.files(tmp)
  expect_setequal(folders, c("dwca_afr_v1_5", "dwca_jbrj_rb_v1_5"))
  csv <- utils::read.csv(file.path(tmp, "dwca_afr_v1_5", "AFR_Jabot.csv"))
  expect_equal(csv$collectionCode, "AFR")
  expect_equal(csv$Records, 1234)
  expect_true(file.exists(file.path(tmp, "dwca_afr_v1_5", "occurrence.txt")))
  # the temporary zip is removed
  expect_false(file.exists(file.path(tmp, "dwca-afr-v1")))

  unlink(tmp, recursive = TRUE)
})


test_that("jabot_download does not download again what is already available", {
  tmp <- tempfile("jabot_download_mock_")
  env <- .mock_download_env()

  jabot_download(herbarium = "AFR", verbose = FALSE, dir = tmp)
  expect_length(env$calls, 1)

  jabot_download(herbarium = "AFR", verbose = FALSE, dir = tmp)
  expect_length(env$calls, 1)

  # an older version of the same herbarium is replaced
  old <- file.path(tmp, "dwca_afr_v1_4")
  dir.create(old)
  unlink(file.path(tmp, "dwca_afr_v1_5"), recursive = TRUE)
  jabot_download(herbarium = "AFR", verbose = FALSE, dir = tmp)
  expect_length(env$calls, 2)
  expect_false(dir.exists(old))

  unlink(tmp, recursive = TRUE)
})


test_that("jabot_download reports its progress", {
  tmp <- tempfile("jabot_download_mock_")
  .mock_download_env()

  expect_message(
    jabot_download(herbarium = "AFR", verbose = TRUE, dir = tmp),
    "Downloading DwC-A files for the collection... AFR 1/1"
  )

  unlink(tmp, recursive = TRUE)
})
