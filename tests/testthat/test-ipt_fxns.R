# Offline tests for the JBRJ IPT access helpers in R/auxiliary_fxns.R.
#
# The fixtures under fixtures/ipt/ are trimmed copies of the real JBRJ IPT
# pages (DCAT Turtle feeds of the "jabot" and "jbrj" IPT instances and a
# resource page). The real readers use base::url() + readLines(), which also
# work on file:// URLs, so the readers are exercised for real against local
# files while the hard-coded https URLs are redirected to the fixtures.

.ipt_fixture <- function(name) {
  test_path("fixtures", "ipt", name)
}

.ipt_file_url <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!startsWith(p, "/")) p <- paste0("/", p)
  paste0("file://", p)
}

# Redirect the DCAT reader to the fixtures (through the real reader) and
# record the URLs requested.
.mock_ipt_dcat <- function(jabot = "jabot_dcat.ttl", jbrj = "jbrj_dcat.ttl",
                           env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$urls <- character()
  real_reader <- jabotR:::.read_ipt_dcat
  files <- c(jabot = jabot, jbrj = jbrj)
  testthat::local_mocked_bindings(
    .read_ipt_dcat = function(url) {
      calls$urls <- c(calls$urls, url)
      instance <- sub("^https://ipt[.]jbrj[.]gov[.]br/([^/]+)/dcat$", "\\1", url)
      f <- files[[instance]]
      if (file.exists(f)) real_reader(.ipt_file_url(f))
      else real_reader(.ipt_file_url(.ipt_fixture(f)))
    },
    .package = "jabotR",
    .env = env
  )
  calls
}

# Copy of an internal function whose free variables (e.g. base functions that
# testthat cannot mock) are looked up first in `overrides`. The body (and hence
# covr's instrumentation) is shared with the package function.
.with_overrides <- function(fun, ...) {
  e <- list2env(list(...), parent = environment(fun))
  environment(fun) <- e
  fun
}


# .ipt_http_headers -------------------------------------------------------------

test_that(".ipt_http_headers returns a named browser-like User-Agent", {
  h <- .ipt_http_headers()
  expect_type(h, "character")
  expect_named(h, "User-Agent")
  expect_match(h[["User-Agent"]], "^Mozilla/5[.]0")
})


# .read_ipt_dcat ----------------------------------------------------------------

test_that(".read_ipt_dcat reads every line of the DCAT feed", {
  lines <- .read_ipt_dcat(.ipt_file_url(.ipt_fixture("jbrj_dcat.ttl")))
  expect_type(lines, "character")
  expect_equal(lines[1], "@prefix schema: <http://schema.org/> .")
  expect_equal(sum(grepl("a dcat:Dataset ;", lines, fixed = TRUE)), 2)
  expect_true(any(grepl("archive.do?r=jbrj_rb", lines, fixed = TRUE)))
  # UTF-8 content survives
  expect_true(any(grepl("Jardim Botânico", lines)))
})

test_that(".read_ipt_dcat turns a connection warning into an informative error", {
  missing <- file.path(withr::local_tempdir(), "no_such_feed.ttl")
  url <- paste0("file://", if (.Platform$OS.type == "windows") "/" else "",
                gsub("\\\\", "/", missing))
  expect_error(
    suppressWarnings(.read_ipt_dcat(url)),
    "Unable to access the JBRJ IPT service at:"
  )
  err <- tryCatch(.read_ipt_dcat(url), error = function(e) conditionMessage(e))
  expect_match(err, "no_such_feed.ttl", fixed = TRUE)
  expect_match(err, "Please try again later.", fixed = TRUE)
})

test_that(".read_ipt_dcat turns a read error into an informative error", {
  reader <- .with_overrides(
    .read_ipt_dcat,
    readLines = function(...) stop("HTTP error 503", call. = FALSE)
  )
  url <- .ipt_file_url(.ipt_fixture("jbrj_dcat.ttl"))
  expect_error(reader(url), "Unable to access the JBRJ IPT service at:")
  expect_error(reader(url), "temporarily unavailable", fixed = TRUE)
})


# .read_ipt_resource_page -------------------------------------------------------

test_that(".read_ipt_resource_page reads the page once when latestVersion is present", {
  n_reads <- 0
  real_readLines <- base::readLines
  reader <- .with_overrides(
    .read_ipt_resource_page,
    readLines = function(...) {
      n_reads <<- n_reads + 1
      real_readLines(...)
    }
  )
  page <- reader(.ipt_file_url(.ipt_fixture("resource_hatm.html")))
  expect_equal(n_reads, 1)
  expect_equal(page[1], "<!DOCTYPE html>")
  expect_true(any(grepl("latestVersion", page)))
})

test_that(".read_ipt_resource_page falls back to a second read without latestVersion", {
  page_file <- withr::local_tempfile(fileext = ".html")
  writeLines(c("<html>", "<body>Resource not found</body>", "</html>"), page_file)

  n_reads <- 0
  real_readLines <- base::readLines
  reader <- .with_overrides(
    .read_ipt_resource_page,
    readLines = function(...) {
      n_reads <<- n_reads + 1
      real_readLines(...)
    }
  )
  page <- reader(.ipt_file_url(page_file))
  expect_equal(n_reads, 2)
  expect_equal(page, c("<html>", "<body>Resource not found</body>", "</html>"))
})

test_that(".read_ipt_resource_page uses the real reader by default", {
  page <- .read_ipt_resource_page(.ipt_file_url(.ipt_fixture("resource_hatm.html")))
  expect_true(any(grepl("'2026-11-01 12:04:45',", page, fixed = TRUE)))
})


# .get_ipt_info_rb --------------------------------------------------------------

test_that(".get_ipt_info_rb keeps only the RB collection of the jbrj IPT", {
  calls <- .mock_ipt_dcat()
  rb <- .get_ipt_info_rb()

  expect_equal(calls$urls, "https://ipt.jbrj.gov.br/jbrj/dcat")
  expect_length(rb, 3)
  expect_length(rb[[1]], 1)
  expect_equal(rb[[2]], "jbrj_rb")
  expect_equal(rb[[3]], "RB")

  meta <- rb[[1]][[1]]
  expect_equal(meta[1], "a dcat:Dataset ;")
  expect_equal(meta[2],
               "dct:title \"RB - Coleção do Herbário do Jardim Botânico do Rio de Janeiro\" ;")
  expect_equal(meta[length(meta)], "dcat:mediaType \"application/zip\" ;")
  expect_true(any(grepl("mailto:rafaela@jbrj.gov.br", meta, fixed = TRUE)))
})

test_that(".get_ipt_info_rb returns no metadata when RB is absent from the feed", {
  lines <- readLines(.ipt_fixture("jbrj_dcat.ttl"), encoding = "UTF-8")
  rb_start <- grep("r=jbrj_rb#Dataset>", lines, fixed = TRUE)
  no_rb <- withr::local_tempfile(fileext = ".ttl")
  writeLines(lines[seq_len(rb_start - 1)], no_rb, useBytes = TRUE)

  .mock_ipt_dcat(jbrj = no_rb)
  rb <- .get_ipt_info_rb()

  expect_length(rb[[1]], 0)
  expect_equal(rb[[2]], character())
  # herb_code is hard-coded, so it stays "RB" even with no match
  expect_equal(rb[[3]], "RB")
})


# .get_ipt_info -----------------------------------------------------------------

test_that(".get_ipt_info parses all JABOT collections and appends RB", {
  calls <- .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = NULL)

  expect_equal(calls$urls, c("https://ipt.jbrj.gov.br/jabot/dcat",
                             "https://ipt.jbrj.gov.br/jbrj/dcat"))
  expect_equal(info[[2]], c("hatm", "palm", "hdjf", "jbrj_rb"))
  expect_equal(info[[3]], c("HATM", "PALM", "HDJF", "RB"))
  expect_length(info[[1]], 4)

  # each metadata block runs from the dataset header to the zip media type
  firsts <- vapply(info[[1]], `[`, character(1), 1)
  lasts <- vapply(info[[1]], function(x) x[length(x)], character(1))
  expect_true(all(firsts == "a dcat:Dataset ;"))
  expect_true(all(lasts == "dcat:mediaType \"application/zip\" ;"))
  expect_match(info[[1]][[2]][2], "^dct:title \"PALM - ")
  expect_match(info[[1]][[3]][2], "^dct:title \"HDJF - ")
})

test_that(".get_ipt_info filters by herbarium code, including RB", {
  .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = c("PALM", "RB"))

  expect_equal(info[[2]], c("palm", "jbrj_rb"))
  expect_equal(info[[3]], c("PALM", "RB"))
  expect_length(info[[1]], 2)
  expect_match(info[[1]][[1]][2], "^dct:title \"PALM - ")
  expect_match(info[[1]][[2]][2], "^dct:title \"RB - ")
})

test_that(".get_ipt_info returns empty components when no herbarium matches", {
  .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = "XYZ")

  expect_length(info[[1]], 0)
  expect_equal(info[[2]], character())
  expect_equal(info[[3]], character())
})

test_that(".get_ipt_info handles a feed without datasets", {
  empty_feed <- withr::local_tempfile(fileext = ".ttl")
  writeLines(c("@prefix dcat: <http://www.w3.org/ns/dcat#> .", "",
               "<https://ipt.jbrj.gov.br/jabot#Catalog>", " a dcat:Catalog ."),
             empty_feed)
  .mock_ipt_dcat(jabot = empty_feed)
  info <- .get_ipt_info(herbarium = NULL)

  # only the RB collection from the jbrj IPT remains
  expect_equal(info[[2]], "jbrj_rb")
  expect_equal(info[[3]], "RB")
  expect_length(info[[1]], 1)
})

test_that(".get_ipt_info propagates the IPT access error", {
  testthat::local_mocked_bindings(
    .read_ipt_dcat = function(url) stop("Unable to access the JBRJ IPT service at:\n", url,
                                        call. = FALSE),
    .package = "jabotR"
  )
  expect_error(.get_ipt_info(NULL), "ipt.jbrj.gov.br/jabot/dcat", fixed = TRUE)
})


# .get_herb_info ----------------------------------------------------------------

.mock_resource_page <- function(file = .ipt_fixture("resource_hatm.html"),
                                env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$urls <- character()
  real_reader <- jabotR:::.read_ipt_resource_page
  testthat::local_mocked_bindings(
    .read_ipt_resource_page = function(url) {
      calls$urls <- c(calls$urls, url)
      real_reader(.ipt_file_url(file))
    },
    .package = "jabotR",
    .env = env
  )
  calls
}

test_that(".get_herb_info parses version, contact and rights holder of a JABOT collection", {
  .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = NULL)
  calls <- .mock_resource_page()

  herb <- .get_herb_info(info[[2]], info[[1]], 1)

  expect_equal(calls$urls, "https://ipt.jbrj.gov.br/jabot/resource?r=hatm")
  expect_length(herb, 5)
  expect_equal(herb[[1]], c("1.69", "2026-11-01 12:04:45", "3,665"))
  # only the first contact point is used
  expect_equal(herb[[2]], "Daniela Santana Nunes")
  expect_equal(herb[[3]], "hatm@ufpa.br")
  expect_equal(herb[[4]], "Herbário Padre José Maria de Albuquerque")
  expect_equal(herb[[5]], "https://ipt.jbrj.gov.br/jabot/resource?r=hatm")
})

test_that(".get_herb_info strips institution suffixes from the rights holder", {
  .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = NULL)
  .mock_resource_page()

  palm <- .get_herb_info(info[[2]], info[[1]], 2)
  expect_equal(palm[[2]], "Patrícia Jungbluth")
  expect_equal(palm[[3]], "herbario_palm@ufsm.br")
  expect_equal(palm[[4]], "Herbário do Campus Palmeira das Missões da UFSM")

  hdjf <- .get_herb_info(info[[2]], info[[1]], 3)
  expect_equal(hdjf[[2]], "Evandro Luiz Mendonça Machado")
  expect_equal(hdjf[[4]], "Herbário Dendrológico Jeanine Felfili")
})

test_that(".get_herb_info uses the jbrj IPT resource URL for RB", {
  .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = "RB")
  calls <- .mock_resource_page()

  rb <- .get_herb_info(info[[2]], info[[1]], 1)

  expect_equal(calls$urls, "https://ipt.jbrj.gov.br/jbrj/resource?r=jbrj_rb")
  expect_equal(rb[[5]], "https://ipt.jbrj.gov.br/jbrj/resource?r=jbrj_rb")
  expect_equal(rb[[2]], "Rafaela Forzza")
  expect_equal(rb[[3]], "rafaela@jbrj.gov.br")
  expect_equal(rb[[4]],
               "Coleção do Herbário do Jardim Botânico do Rio de Janeiro")
})

test_that(".get_herb_info removes a 'Flores - ' prefix from the rights holder", {
  meta <- list(c("a dcat:Dataset ;",
                 "dct:title \"XYZ - Flores -  Herbário Teste. Jabot - Banco de Dados da Flora Brasileira\" ;",
                 "dcat:contactPoint [ a vcard:Individual ; vcard:fn \"Ana Teste\"; vcard:hasEmail <mailto:ana@example.org> ] ;",
                 "dcat:mediaType \"application/zip\" ;"))
  .mock_resource_page()
  herb <- .get_herb_info("xyz", meta, 1)
  expect_equal(herb[[4]], "Herbário Teste")
  expect_equal(herb[[2]], "Ana Teste")
  expect_equal(herb[[3]], "ana@example.org")
})

test_that(".get_herb_info returns empty contact fields when no contact point exists", {
  meta <- list(c("a dcat:Dataset ;",
                 "dct:title \"XYZ - Herbário Sem Contato. Jabot - Banco de Dados\" ;",
                 "dcat:mediaType \"application/zip\" ;"))
  .mock_resource_page()
  herb <- .get_herb_info("xyz", meta, 1)
  expect_equal(herb[[1]], c("1.69", "2026-11-01 12:04:45", "3,665"))
  expect_length(herb[[2]], 0)
  expect_length(herb[[3]], 0)
  expect_equal(herb[[4]], "Herbário Sem Contato")
})

test_that(".get_herb_info errors on a resource page without a version table", {
  page_file <- withr::local_tempfile(fileext = ".html")
  writeLines(c("<html>", "<body>Resource not found</body>", "</html>"), page_file)
  .mock_ipt_dcat()
  info <- .get_ipt_info(herbarium = NULL)
  .mock_resource_page(file = page_file)

  expect_error(.get_herb_info(info[[2]], info[[1]], 1))
})


# .check_report_pkgs ------------------------------------------------------------

test_that(".check_report_pkgs passes when plotly is available", {
  checker <- .with_overrides(.check_report_pkgs,
                             requireNamespace = function(...) TRUE)
  expect_invisible(checker())
  expect_true(checker())
})

test_that(".check_report_pkgs errors with install advice when plotly is missing", {
  requested <- NULL
  checker <- .with_overrides(
    .check_report_pkgs,
    requireNamespace = function(package, ...) {
      requested <<- package
      FALSE
    }
  )
  expect_error(checker(), "The package 'plotly' is required to build the HTML report.",
               fixed = TRUE)
  expect_error(checker(), "install.packages('plotly')", fixed = TRUE)
  expect_equal(requested, "plotly")
})
