.coverage_test_fixture <- function() {
  taxon <- data.frame(
    id = as.character(1:6),
    taxonomicStatus = c(rep("NOME_ACEITO", 5), "SINONIMO"),
    taxonRank = rep("ESPECIE", 6),
    group = rep("Angiosperms", 6),
    kingdom = rep("Plantae", 6),
    phylum = rep("Tracheophyta", 6),
    class = rep("Magnoliopsida", 6),
    order = c("Fabales", "Fabales", "Fabales", "Asterales", "Asterales", "Fabales"),
    family = c("Fabaceae", "Fabaceae", "Fabaceae", "Asteraceae", "Asteraceae", "Fabaceae"),
    genus = c("Inga", "Inga", "Mimosa", "Aster", "Aster", "Inga"),
    taxonName = c("Inga edulis", "Inga vera", "Mimosa pudica",
                  "Aster alba", "Aster beta", "Inga synonym"),
    scientificNameAuthorship = c("Mart.", "Willd.", "L.", "A.", "B.", "Syn."),
    references = paste0("ref", 1:6),
    stringsAsFactors = FALSE
  )

  distribution <- data.frame(
    id = c("1", "1", "2", "3", "4", "5"),
    locationID = c("BR-BA", "BR-MG", "BR-BA", "BR-SP", "BR-AM", "BR-PA"),
    phytogeographicDomain = c("Caatinga", "Mata Atlantica", "Caatinga",
                              "Cerrado", "Amazonia", "Amazonia, Cerrado"),
    endemism = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  herb <- data.frame(
    genus = c("Inga", "Mimosa", "Mimosa"),
    species = c("synonym", "pudica", "pudica"),
    taxonName = c("Inga synonym", "Mimosa pudica", "Mimosa pudica"),
    division = rep("Tracheophyta", 3),
    typeStatus = c("HOLOTYPE", NA_character_, "ISOTYPE"),
    stringsAsFactors = FALSE
  )

  list(
    ffb = list(
      taxon = taxon,
      distribution = distribution,
      speciesprofile = data.frame(),
      typesandspecimen = data.frame()
    ),
    herb = herb
  )
}


.mock_coverage_report_render <- function(input, output_file, params, envir, quiet) {
  writeLines("<html><body>mock report</body></html>", output_file)
  invisible(output_file)
}


test_that("jabot_coverage validates required arguments", {
  expect_error(
    jabot_coverage(herbarium = NULL),
    "'herbarium' must be provided"
  )

  expect_error(
    jabot_coverage(herbarium = "RB", format = "svg"),
    "'format' must be 'pdf', 'png', or both"
  )
})


test_that("jabot_coverage computes representativeness with deterministic input data", {
  fixture <- .coverage_test_fixture()
  tmp <- tempfile("jabot_coverage_")
  render_calls <- new.env(parent = emptyenv())
  render_calls$n <- 0L
  save_calls <- new.env(parent = emptyenv())
  save_calls$format <- NULL
  save_calls$fig_dir <- NULL

  testthat::local_mocked_bindings(
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .make_rep_plots = function(...) list(mock_plot = TRUE),
    .save_rep_figures = function(plots, fig_dir, format, width, height, verbose) {
      save_calls$format <- format
      save_calls$fig_dir <- fig_dir
      invisible(NULL)
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    flora_search = function(x, progress_bar, rm_flora_database) {
      data.frame(
        Search = x,
        Accepted.taxon.Name = rep("Inga vera", length(x)),
        family = rep("Fabaceae", length(x)),
        stringsAsFactors = FALSE
      )
    },
    .package = "floraR"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      render_calls$n <- render_calls$n + 1L
      .mock_coverage_report_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  result <- suppressWarnings(jabot_coverage(
    herbarium = " rb ",
    format = c("PDF", " png "),
    open_report = FALSE,
    verbose = FALSE,
    dir = tmp
  ))

  expect_type(result, "list")
  expect_equal(result$herbarium, "RB")
  expect_equal(result$n_ffb_total, 5)
  expect_equal(result$n_in_herb, 2)
  expect_equal(result$n_missing, 3)
  expect_equal(result$coverage_pct, 40)
  expect_equal(result$n_specimens, 3)
  expect_equal(result$n_families, 1)
  expect_equal(result$n_genera, 2)

  expect_equal(
    result$family_rep$coverage_pct[result$family_rep$family == "Fabaceae"],
    66.7
  )
  expect_equal(
    result$family_rep$coverage_pct[result$family_rep$family == "Asteraceae"],
    0
  )
  expect_equal(
    result$state_rep$coverage_pct[result$state_rep$abbrev_state == "SP"],
    100
  )
  expect_equal(
    result$state_rep$coverage_pct[result$state_rep$abbrev_state == "BA"],
    50
  )
  expect_equal(result$n_endemic_ffb, 2)
  expect_equal(result$n_endemic_in_herb, 1)
  expect_equal(result$endemic_coverage_pct, 50)
  expect_equal(result$non_endemic_coverage_pct, 33.3)
  expect_equal(result$n_type_specimens, 2)
  expect_equal(result$n_type_taxa, 2)
  expect_equal(result$type_group_rep$pct_type_species, 100)

  expect_true(dir.exists(tmp))
  expect_true(dir.exists(file.path(tmp, "figures")))
  expect_true(file.exists(file.path(tmp, "RB_representativeness.html")))
  expect_equal(save_calls$format, c("pdf", "png"))
  expect_equal(save_calls$fig_dir, file.path(tmp, "figures"))
  expect_equal(render_calls$n, 1L)

  unlink(tmp, recursive = TRUE)
})


test_that(".make_rep_plots returns ggplot objects and tolerates map failure", {
  testthat::local_mocked_bindings(
    read_state = function(...) stop("offline fixture"),
    .package = "geobr"
  )

  group_rep <- data.frame(
    group = c("Angiosperms", "Ferns"),
    n_in = c(8, 2),
    n_ffb = c(10, 5),
    coverage_pct = c(80, 40)
  )
  family_rep <- data.frame(
    family = c("Fabaceae", "Asteraceae"),
    n_in = c(8, 2),
    n_ffb = c(10, 20),
    coverage_pct = c(80, 10)
  )
  domain_rep <- data.frame(
    phytogeographicDomain = c("Caatinga", "Amazonia"),
    n_in = c(5, 2),
    n_ffb = c(10, 10),
    coverage_pct = c(50, 20)
  )
  state_rep <- data.frame(
    abbrev_state = c("BA", "AM"),
    n_in = c(5, 2),
    n_ffb = c(10, 10),
    coverage_pct = c(50, 20)
  )
  type_group_rep <- data.frame(
    group = c("Angiosperms", "Ferns"),
    n_species = c(10, 5),
    n_type_species = c(2, 1),
    pct_type_species = c(20, 20)
  )
  type_family_rep <- data.frame(
    family = c("Fabaceae", "Asteraceae"),
    n_type_taxa = c(4, 2)
  )

  expect_message(
    suppressWarnings(plots <- .make_rep_plots(
      herbarium = "RB",
      group_rep = group_rep,
      family_rep = family_rep,
      domain_rep = domain_rep,
      state_rep = state_rep,
      type_group_rep = type_group_rep,
      type_family_rep = type_family_rep,
      endemic_coverage_pct = 50,
      non_endemic_coverage_pct = 40,
      coverage_pct = 45,
      n_ffb_total = 100,
      n_in_herb = 45
    )),
    "state map skipped"
  )

  expect_named(
    plots,
    c("group", "family_best", "family_worst", "domain", "endemic",
      "state", "types", "type_families")
  )
  expect_true(all(vapply(
    plots[c("group", "family_best", "family_worst", "domain",
            "endemic", "types", "type_families")],
    inherits,
    logical(1),
    what = "ggplot"
  )))
  expect_null(plots$state)
})


test_that(".save_rep_figures saves requested formats and skips NULL plots", {
  tmp <- tempfile("jabot_rep_figures_")
  dir.create(tmp)

  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  expect_message(
    .save_rep_figures(
      plots = list(group = p, state = NULL),
      fig_dir = tmp,
      format = "pdf",
      width = 4,
      height = 3,
      verbose = TRUE
    ),
    "Saved:"
  )

  expect_true(file.exists(file.path(tmp, "01_rep_group.pdf")))
  expect_false(file.exists(file.path(tmp, "06_rep_state_map.pdf")))

  unlink(tmp, recursive = TRUE)
})


# ---- Additional coverage: verbose run, report options, state map -----------

# Mocks base::system.file (or the pkgload shim under devtools::load_all()) so
# that the bundled Rmd template appears to be missing.
.coverage_local_missing_template <- function(env = parent.frame()) {
  real <- base::system.file
  fake <- function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
    if (any(grepl("^rmd/", c(...)))) return("")
    real(..., package = package, lib.loc = lib.loc, mustWork = mustWork)
  }
  imports <- parent.env(asNamespace("jabotR"))
  if (exists("system.file", envir = imports, inherits = FALSE)) {
    testthat::local_mocked_bindings(system.file = fake, .package = "jabotR",
                                    .env = env)
  } else {
    testthat::local_mocked_bindings(system.file = fake, .package = "base",
                                    .env = env)
  }
}


.coverage_local_mocks <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .make_rep_plots = function(...) list(mock_plot = TRUE),
    .package = "jabotR",
    .env = env
  )
  testthat::local_mocked_bindings(
    flora_search = function(x, progress_bar, rm_flora_database) {
      data.frame(Search = x, Accepted.taxon.Name = rep("Inga vera", length(x)),
                 family = rep("Fabaceae", length(x)), stringsAsFactors = FALSE)
    },
    .package = "floraR",
    .env = env
  )
}


test_that("jabot_coverage verbose run writes to the default directory and opens the report", {
  fixture <- .coverage_test_fixture()
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  calls <- new.env(parent = emptyenv())

  .coverage_local_mocks(fixture)
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      calls$quiet <- quiet
      calls$params <- params
      .mock_coverage_report_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) calls$url <- url,
    .package = "utils"
  )

  msgs <- testthat::capture_messages(
    result <- suppressWarnings(testthat::with_mocked_bindings(
      jabot_coverage(herbarium = "rb", open_report = TRUE, verbose = TRUE),
      interactive = function() TRUE,
      .package = "base"
    ))
  )
  msgs <- paste(msgs, collapse = "")

  for (step in c("[1/5] Loading Flora",
                 "[2/5] Loading JABOT records for herbarium 'RB'",
                 "[3/5] Resolving synonyms",
                 "[4/5] Computing representativeness metrics",
                 "[5/5] Building figures", "Rendering HTML report",
                 "Report saved:")) {
    expect_match(msgs, step, fixed = TRUE)
  }

  html <- file.path(wd, "RB_representativeness", "RB_representativeness.html")
  expect_true(file.exists(html))
  expect_true(file.exists(file.path(wd, "RB_representativeness", "figures",
                                    "jabotr_hex_sticker.png")))
  expect_false(calls$quiet)
  expect_equal(calls$params$herbarium, "RB")
  # The temporary data file handed to the template is removed after rendering
  expect_false(file.exists(calls$params$data_path))
  expect_equal(result$coverage_pct, 40)

  # Byte-compiled (installed) package code inlines interactive(), so the
  # mock above only reaches it under devtools::load_all()
  if (is.null(calls$url)) skip("interactive() cannot be mocked in byte-compiled code")
  expect_equal(normalizePath(calls$url), normalizePath(html))
})


test_that("jabot_coverage does not open the report when open_report = FALSE", {
  fixture <- .coverage_test_fixture()
  tmp <- withr::local_tempdir()
  opened <- new.env(parent = emptyenv())
  opened$n <- 0L

  .coverage_local_mocks(fixture)
  testthat::local_mocked_bindings(render = .mock_coverage_report_render,
                                  .package = "rmarkdown")
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) opened$n <- opened$n + 1L,
    .package = "utils"
  )

  suppressWarnings(testthat::with_mocked_bindings(
    jabot_coverage(herbarium = "RB", open_report = FALSE, verbose = FALSE,
                   dir = tmp),
    interactive = function() TRUE,
    .package = "base"
  ))
  expect_equal(opened$n, 0L)
})


test_that("jabot_coverage errors when the Rmd template is missing", {
  fixture <- .coverage_test_fixture()
  tmp <- withr::local_tempdir()

  .coverage_local_mocks(fixture)
  .coverage_local_missing_template()

  expect_error(
    suppressWarnings(jabot_coverage(herbarium = "RB", verbose = FALSE,
                                    dir = tmp)),
    "Rmd template not found"
  )
  expect_false(file.exists(file.path(tmp, "RB_representativeness.html")))
})


test_that(".make_rep_plots builds the state coverage choropleth", {
  square <- function(x0) {
    sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1),
                              c(x0, 1), c(x0, 0))))
  }
  states <- sf::st_sf(
    abbrev_state = c("BA", "AM", "PE"),
    name_state = c("Bahia", "Amazonas", "Pernambuco"),
    geometry = sf::st_sfc(square(0), square(2), square(4))
  )
  testthat::local_mocked_bindings(
    read_state = function(year, showProgress) states,
    .package = "geobr"
  )

  # geom_sf() warns that the plotly "text" aesthetic is unknown to ggplot2
  plots <- suppressWarnings(.make_rep_plots(
    herbarium = "RB",
    group_rep = data.frame(group = "Angiosperms", n_in = 8, n_ffb = 10,
                           coverage_pct = 80),
    family_rep = data.frame(family = c("Fabaceae", "Asteraceae"),
                            n_in = c(8, 2), n_ffb = c(10, 20),
                            coverage_pct = c(80, 10)),
    domain_rep = data.frame(phytogeographicDomain = "Caatinga", n_in = 5,
                            n_ffb = 10, coverage_pct = 50),
    state_rep = data.frame(abbrev_state = c("BA", "AM"), n_in = c(1500, 2),
                           n_ffb = c(3000, 10), coverage_pct = c(50, 20)),
    type_group_rep = data.frame(group = "Angiosperms", n_species = 10,
                                n_type_species = 2, pct_type_species = 20),
    type_family_rep = data.frame(family = "Fabaceae", n_type_taxa = 4),
    endemic_coverage_pct = 50,
    non_endemic_coverage_pct = 40,
    coverage_pct = 45,
    n_ffb_total = 100,
    n_in_herb = 45
  ))

  expect_s3_class(plots$state, "ggplot")
  sd <- plots$state$data
  expect_equal(sd$coverage_pct[sd$abbrev_state == "BA"], 50)
  # States absent from state_rep get 0% coverage instead of NA
  expect_equal(sd$coverage_pct[sd$abbrev_state == "PE"], 0)
  expect_equal(
    sd$tooltip[sd$abbrev_state == "BA"],
    "<b>Bahia</b><br>Coverage: 50%<br>Species represented: 1,500<br>FFB total: 3,000"
  )
  expect_match(plots$state$labels$title,
               "Herbarium coverage by Brazilian state .* RB")
})
