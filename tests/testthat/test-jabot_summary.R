test_that("jabot_summary works for all herbaria and selected herbaria", {
  skip_if_no_jabot()

  res_all <- jabot_summary(
    verbose = FALSE,
    save = FALSE
  )

  res_selected <- jabot_summary(
    herbarium = "R",
    verbose = FALSE,
    save = FALSE
  )

  expect_s3_class(res_all, "data.frame")
  expect_s3_class(res_selected, "data.frame")

  expect_equal(ncol(res_all), 8)
  expect_equal(ncol(res_selected), 8)

  expect_type(res_all[[3]], "character")
  expect_type(res_all[[8]], "character")

  expect_type(res_selected[[3]], "character")
  expect_type(res_selected[[8]], "character")

  expect_gt(nrow(res_all), nrow(res_selected))
})


test_that("jabot_summary saves CSV when save = TRUE", {
  skip_if_no_jabot()

  tmp_dir <- tempfile("jabot_summary_save_")
  dir.create(tmp_dir)

  jabot_summary(
    herbarium = "RB",
    verbose = FALSE,
    save = TRUE,
    dir = tmp_dir
  )

  output_path <- file.path(
    tmp_dir,
    "jabot_summary.csv"
  )

  expect_true(file.exists(output_path))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_summary rejects invalid herbarium code", {
  skip_if_no_jabot()

  expect_error(
    jabot_summary(
      herbarium = "FAKE",
      verbose = FALSE,
      save = FALSE
    )
  )
})


test_that("jabot_summary handles trailing slash in dir", {
  skip_if_no_jabot()

  tmp_dir <- tempfile("jabot_summary_trailing_")
  dir.create(tmp_dir)

  trailing_dir <- paste0(tmp_dir, "/")

  result <- jabot_summary(
    herbarium = "RB",
    verbose = FALSE,
    save = TRUE,
    dir = trailing_dir
  )

  expect_s3_class(result, "data.frame")

  expect_true(
    file.exists(
      file.path(tmp_dir, "jabot_summary.csv")
    )
  )

  unlink(tmp_dir, recursive = TRUE)
})


test_that("jabot_summary prints expected verbose message", {
  skip_if_no_jabot()

  local_edition(3)

  expect_message(
    jabot_summary(
      herbarium = "R",
      verbose = TRUE,
      save = FALSE
    ),
    regexp = "Summarizing specimen collections of R 1/1"
  )
})


test_that("jabot_summary handles missing contact email", {
  skip_if_no_jabot()

  result <- jabot_summary(
    herbarium = "R",
    verbose = FALSE,
    save = FALSE
  )

  expect_true(
    is.na(result$hasEmail[1]) ||
      is.character(result$hasEmail[1])
  )
})


test_that("jabot_summary returns Records as numeric", {
  skip_if_no_jabot()

  result <- jabot_summary(
    herbarium = "R",
    verbose = FALSE,
    save = FALSE
  )

  expect_type(result$Records, "double")
})


# Offline tests: downloads and parsing are mocked ----

# Mocks the IPT metadata (.get_ipt_info) and resource pages (.get_herb_info).
# Codes are deliberately unsorted; "EMPTY" has zero records and no email.
.local_summary_mocks <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$herb_info <- character()

  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) {
      codes <- c("RB", "EMPTY", "AFR")
      keep <- if (is.null(herbarium)) rep(TRUE, 3) else codes %in% herbarium
      list(list("meta-rb", "meta-empty", "meta-afr")[keep],
           c("jbrj_rb", "empty", "afr")[keep],
           codes[keep])
    },
    .get_herb_info = function(herb_URLs, ipt_metadata, i) {
      calls$herb_info <- c(calls$herb_info, herb_URLs[i])
      info <- list(
        jbrj_rb = list(c("2.10", "2026-02-01", "1,234"), "RB Curator",
                       "rb@example.org", "Jardim Botanico do Rio de Janeiro"),
        empty = list(c("1.0", "2025-01-01", "0"), "Nobody", character(0),
                     "Empty Herbarium"),
        afr = list(c("1.5", "2026-01-01", "5"), "AFR Curator",
                   character(0), "AFR Rights Holder")
      )[[herb_URLs[i]]]
      c(info, paste0("https://ipt.jbrj.gov.br/jabot/resource?r=", herb_URLs[i]))
    },
    .package = "jabotR",
    .env = env
  )
  calls
}


test_that("offline: jabot_summary summarizes, sorts and drops empty collections", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_summary_mocks()

  res <- jabot_summary(verbose = FALSE, save = FALSE)

  expect_setequal(calls$herb_info, c("jbrj_rb", "empty", "afr"))
  expect_named(res, c("collectionCode", "rightsHolder", "contactPoint",
                      "hasEmail", "Version", "Published.on", "Records",
                      "Jabot_URL"))
  # EMPTY (0 records) removed; remaining rows sorted by collectionCode
  expect_equal(res$collectionCode, c("AFR", "RB"))
  expect_equal(res$Records, c(5, 1234))
  expect_type(res$Records, "double")
  expect_equal(res$Version, c("1.5", "2.10"))
  expect_equal(res$Published.on, c("2026-01-01", "2026-02-01"))
  expect_equal(res$contactPoint, c("AFR Curator", "RB Curator"))
  expect_equal(res$hasEmail, c(NA, "rb@example.org"))
  expect_equal(res$rightsHolder,
               c("AFR Rights Holder", "Jardim Botanico do Rio de Janeiro"))
  expect_equal(res$Jabot_URL,
               paste0("https://ipt.jbrj.gov.br/jabot/resource?r=",
                      c("afr", "jbrj_rb")))

  # save = FALSE writes nothing
  expect_false(dir.exists("jabot_summary"))
})


test_that("offline: jabot_summary restricts to selected herbaria with progress messages", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_summary_mocks()

  msgs <- testthat::capture_messages(
    res <- jabot_summary(herbarium = c("RB", "AFR"), verbose = TRUE,
                         save = FALSE)
  )

  expect_true(any(grepl("Checking whether the input herbarium code exists",
                        msgs)))
  expect_true(any(grepl("Summarizing specimen collections of RB 1/2", msgs)))
  expect_true(any(grepl("Summarizing specimen collections of AFR 2/2", msgs)))
  expect_setequal(calls$herb_info, c("jbrj_rb", "afr"))
  expect_equal(res$collectionCode, c("AFR", "RB"))

  # A selection whose only collection is empty yields zero rows
  empty <- jabot_summary(herbarium = "EMPTY", verbose = FALSE, save = FALSE)
  expect_equal(nrow(empty), 0)
  expect_named(empty, names(res))
})


test_that("offline: jabot_summary validates herbarium and dir", {
  withr::local_dir(withr::local_tempdir())
  calls <- .local_summary_mocks()

  expect_error(
    jabot_summary(herbarium = c("RB", "NOPE"), verbose = FALSE, save = FALSE),
    "not recognized by JABOT: .NOPE."
  )
  expect_error(jabot_summary(dir = 42, verbose = FALSE, save = FALSE),
               "dir should be a character")
  expect_length(calls$herb_info, 0)
})


test_that("offline: jabot_summary save = TRUE writes jabot_summary.csv", {
  withr::local_dir(withr::local_tempdir())
  .local_summary_mocks()

  msgs <- testthat::capture_messages(
    res <- jabot_summary(verbose = TRUE, save = TRUE, dir = "sum_out/")
  )
  expect_true(any(grepl(
    "Writing spreadsheet 'jabot_summary.csv' within 'sum_out' folder on disk.",
    msgs, fixed = TRUE
  )))

  path <- file.path("sum_out", "jabot_summary.csv")
  expect_true(file.exists(path))
  csv <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(csv$collectionCode, res$collectionCode)
  expect_equal(csv$Records, res$Records)
  expect_named(csv, names(res))
  # .save_csv() writes no log for summaries
  expect_false(file.exists(file.path("sum_out", "log.txt")))

  # Writing again into the existing directory overwrites quietly
  expect_silent(jabot_summary(verbose = FALSE, save = TRUE, dir = "sum_out"))
  expect_equal(list.files("sum_out"), "jabot_summary.csv")
})
