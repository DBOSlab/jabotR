.gaps_test_fixture <- function() {
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


.mock_report_render <- function(input, output_file, params, envir, quiet) {
  writeLines("<html><body>mock report</body></html>", output_file)
  invisible(output_file)
}


test_that("jabot_gaps validates required arguments", {
  expect_error(
    jabot_gaps(herbarium = NULL),
    "'herbarium' must be provided"
  )

  expect_error(
    jabot_gaps(herbarium = "RB", format = "svg"),
    "'format' must be 'pdf', 'png', or both"
  )
})


test_that("jabot_gaps computes taxonomic gaps with deterministic input data", {
  fixture <- .gaps_test_fixture()
  tmp <- tempfile("jabot_gaps_")
  render_calls <- new.env(parent = emptyenv())
  render_calls$n <- 0L
  save_calls <- new.env(parent = emptyenv())
  save_calls$format <- NULL
  save_calls$fig_dir <- NULL

  testthat::local_mocked_bindings(
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .make_gap_plots = function(...) list(mock_plot = TRUE),
    .save_gap_figures = function(plots, fig_dir, format, width, height, verbose) {
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
      .mock_report_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  result <- suppressWarnings(jabot_gaps(
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
  expect_equal(result$pct_present, 40)
  expect_equal(result$pct_missing, 60)
  expect_equal(result$n_endemic_missing, 1)
  expect_equal(result$n_genera_absent_all, 1)

  expect_setequal(result$taxon_not_in_herb$taxonName,
                  c("Inga edulis", "Aster alba", "Aster beta"))
  expect_equal(
    result$family_gap$n_missing[result$family_gap$family == "Asteraceae"],
    2
  )
  expect_equal(
    result$genus_gap$in_herb[result$genus_gap$genus == "Aster"],
    FALSE
  )
  expect_equal(
    result$domain_gap$n_missing[result$domain_gap$phytogeographicDomain == "Amazonia"],
    2
  )

  expect_true(dir.exists(tmp))
  expect_true(dir.exists(file.path(tmp, "figures")))
  expect_true(file.exists(file.path(tmp, "RB_gap_analysis.html")))
  expect_equal(save_calls$format, c("pdf", "png"))
  expect_equal(save_calls$fig_dir, file.path(tmp, "figures"))
  expect_equal(render_calls$n, 1L)

  unlink(tmp, recursive = TRUE)
})


test_that(".make_gap_plots returns ggplot objects and tolerates map failure", {
  testthat::local_mocked_bindings(
    read_state = function(...) stop("offline fixture"),
    .package = "geobr"
  )

  group_gap <- data.frame(
    group = c("Angiosperms", "Ferns"),
    n_missing = c(3, 1)
  )
  family_gap <- data.frame(
    family = c("Fabaceae", "Asteraceae"),
    n_missing = c(2, 1)
  )
  genus_gap <- data.frame(
    genus = c("Aster", "Inga"),
    family = c("Asteraceae", "Fabaceae"),
    n_missing = c(2, 1),
    in_herb = c(FALSE, TRUE)
  )
  domain_gap <- data.frame(
    phytogeographicDomain = c("Amazonia", "Caatinga"),
    n_missing = c(2, 1)
  )
  endemism_gap <- data.frame(
    endemism_label = c("Brazilian endemic", "Non-endemic"),
    n_missing = c(1, 2)
  )
  state_gap <- data.frame(
    abbrev_state = c("BA", "AM"),
    n_missing = c(1, 2)
  )

  expect_message(
    suppressWarnings(plots <- .make_gap_plots(
      herbarium = "RB",
      group_gap = group_gap,
      family_gap = family_gap,
      genus_gap = genus_gap,
      domain_gap = domain_gap,
      endemism_gap = endemism_gap,
      state_gap = state_gap,
      n_missing = 3
    )),
    "state map skipped"
  )

  expect_named(plots, c("group", "genus", "domain", "endemism", "state", "priority_map"))
  expect_true(all(vapply(plots[c("group", "genus", "domain", "endemism")],
                         inherits, logical(1), what = "ggplot")))
  expect_null(plots$state)
  expect_null(plots$priority_map)
})


test_that(".save_gap_figures saves requested formats and skips NULL plots", {
  tmp <- tempfile("jabot_gap_figures_")
  dir.create(tmp)

  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  expect_message(
    .save_gap_figures(
      plots = list(group = p, state = NULL),
      fig_dir = tmp,
      format = "pdf",
      width = 4,
      height = 3,
      verbose = TRUE
    ),
    "Saved:"
  )

  expect_true(file.exists(file.path(tmp, "01_gap_group.pdf")))
  expect_false(file.exists(file.path(tmp, "06_gap_state_map.pdf")))

  unlink(tmp, recursive = TRUE)
})


# ---- Additional coverage: verbose run, priority map, report options --------

# Mocks base::system.file (or the pkgload shim under devtools::load_all()) so
# that the bundled Rmd template appears to be missing.
.gaps_local_missing_template <- function(env = parent.frame()) {
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


.gaps_square <- function(x0, y0) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + 1, y0), c(x0 + 1, y0 + 1),
                            c(x0, y0 + 1), c(x0, y0))))
}


.gaps_pool_fixture <- function() {
  data.frame(
    genus = c("Inga", "Aster", "Inga", "Mimosa", NA),
    species = c("edulis", "alba", "edulis", "pudica", "sp"),
    taxonName = c("Inga edulis", "Aster alba", "Inga edulis", "Mimosa pudica",
                  NA),
    collectionCode = c("ALCB", "HUEFS", "RB", "ALCB", "ALCB"),
    municipality = c("Salvador", "Manaus", "Ilh\u00e9us", "Salvador", "Recife"),
    stateProvince = c("Bahia", "Amazonas", "Bahia", "Bahia", "Pernambuco"),
    stringsAsFactors = FALSE
  )
}


.gaps_mock_flora_search <- function(x, progress_bar, rm_flora_database) {
  data.frame(Search = x, Accepted.taxon.Name = rep("Inga vera", length(x)),
             family = rep("Fabaceae", length(x)), stringsAsFactors = FALSE)
}


test_that("jabot_gaps verbose run builds the nationwide priority map", {
  fixture <- .gaps_test_fixture()
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  calls <- new.env(parent = emptyenv())

  muni <- sf::st_sf(
    muni_key = c("SALVADOR", "MANAUS", "RECIFE"),
    abbrev_state = c("BA", "AM", "PE"),
    geometry = sf::st_sfc(.gaps_square(0, 0), .gaps_square(2, 0),
                          .gaps_square(4, 0))
  )

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) {
      calls$checked <- x
      invisible(TRUE)
    },
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .load_pool_data = function(network_herbaria, jabot_path, verbose) {
      calls$network_herbaria <- network_herbaria
      .gaps_pool_fixture()
    },
    .load_municipality_sf = function(state_abbrevs, verbose) {
      calls$state_abbrevs <- state_abbrevs
      muni
    },
    .make_gap_plots = function(..., muni_sf = NULL) {
      calls$muni_sf <- muni_sf
      list(mock_plot = TRUE)
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    flora_search = function(x, progress_bar, rm_flora_database) {
      calls$progress_bar <- progress_bar
      .gaps_mock_flora_search(x, progress_bar, rm_flora_database)
    },
    .package = "floraR"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      calls$quiet <- quiet
      .mock_report_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  msgs <- testthat::capture_messages(
    result <- suppressWarnings(jabot_gaps(
      herbarium = "rb",
      priority_map = TRUE,
      network_herbaria = c(" alcb ", "huefs"),
      verbose = TRUE
    ))
  )
  msgs <- paste(msgs, collapse = "")

  for (step in c("[1/6] Loading Flora",
                 "[2/6] Loading JABOT records for herbarium 'RB'",
                 "[3/6] Resolving synonyms", "Resolved 1 synonym(s)",
                 "[4/6] Computing taxonomic gaps",
                 "[5/6] Computing municipality collecting priority",
                 "[6/6] Building figures", "Rendering HTML report",
                 "Report saved:")) {
    expect_match(msgs, step, fixed = TRUE)
  }

  # Default output directory, relative to the working directory
  out_dir <- file.path(wd, "RB_gap_analysis")
  expect_true(file.exists(file.path(out_dir, "RB_gap_analysis.html")))
  expect_true(file.exists(file.path(out_dir, "figures", "jabotr_hex_sticker.png")))

  # network_herbaria normalised, validated and forwarded to the pool loader
  expect_equal(calls$checked, c("ALCB", "HUEFS"))
  expect_equal(calls$network_herbaria, c("ALCB", "HUEFS"))
  expect_equal(calls$state_abbrevs, "all")
  expect_true(calls$progress_bar)
  expect_false(calls$quiet)

  # Priority: vouchers of missing species held by other herbaria only;
  # Mimosa pudica is present in RB and the undetermined record is dropped
  mp <- result$municipality_priority
  expect_setequal(mp$municipality, c("Salvador", "Manaus"))
  expect_equal(mp$n_missing_species[mp$municipality == "Salvador"], 1)
  expect_equal(mp$n_records[mp$municipality == "Salvador"], 1)
  expect_false("ILHEUS" %in% mp$muni_key)

  # Municipalities without vouchers are kept with zero counts
  ms <- result$muni_sf
  expect_s3_class(ms, "sf")
  expect_identical(calls$muni_sf, ms)
  expect_equal(ms$n_missing_species[ms$muni_key == "RECIFE"], 0)
  expect_equal(ms$n_records[ms$muni_key == "RECIFE"], 0)
  expect_equal(ms$n_missing_species[ms$muni_key == "MANAUS"], 1)
})


test_that("jabot_gaps skips the priority map when polygons fail and opens the report", {
  fixture <- .gaps_test_fixture()
  tmp <- withr::local_tempdir()
  opened <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .load_pool_data = function(network_herbaria, jabot_path, verbose) {
      .gaps_pool_fixture()
    },
    .load_municipality_sf = function(state_abbrevs, verbose) stop("no polygons"),
    .make_gap_plots = function(...) list(mock_plot = TRUE),
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(flora_search = .gaps_mock_flora_search,
                                  .package = "floraR")
  testthat::local_mocked_bindings(render = .mock_report_render,
                                  .package = "rmarkdown")
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) opened$url <- url,
    .package = "utils"
  )

  msgs <- testthat::capture_messages(
    result <- suppressWarnings(testthat::with_mocked_bindings(
      jabot_gaps(herbarium = "RB", priority_map = TRUE, open_report = TRUE,
                 verbose = TRUE, dir = tmp),
      .is_interactive = function() TRUE,
      .package = "jabotR"
    ))
  )

  expect_match(paste(msgs, collapse = ""),
               "municipality polygons could not be loaded.*no polygons")
  expect_null(result$muni_sf)
  expect_s3_class(result$municipality_priority, "data.frame")
  expect_equal(normalizePath(opened$url),
               normalizePath(file.path(tmp, "RB_gap_analysis.html")))
})


test_that("jabot_gaps reports the skipped priority map and a missing template", {
  fixture <- .gaps_test_fixture()
  tmp <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .make_gap_plots = function(...) list(mock_plot = TRUE),
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(flora_search = .gaps_mock_flora_search,
                                  .package = "floraR")
  .gaps_local_missing_template()

  msgs <- testthat::capture_messages(
    err <- tryCatch(
      suppressWarnings(jabot_gaps(herbarium = "RB", verbose = TRUE, dir = tmp)),
      error = function(e) e
    )
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Rmd template not found")
  expect_match(paste(msgs, collapse = ""),
               "Skipping municipality collecting priority", fixed = TRUE)
  expect_false(file.exists(file.path(tmp, "RB_gap_analysis.html")))
})


test_that(".make_gap_plots builds the state and priority choropleths", {
  states <- sf::st_sf(
    abbrev_state = c("BA", "AM", "PE"),
    name_state = c("Bahia", "Amazonas", "Pernambuco"),
    geometry = sf::st_sfc(.gaps_square(0, 0), .gaps_square(2, 0),
                          .gaps_square(4, 0))
  )
  read_args <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    read_state = function(year, showProgress) {
      read_args$year <- year
      read_args$showProgress <- showProgress
      states
    },
    .package = "geobr"
  )

  muni_sf <- sf::st_sf(
    muni_key = c("SALVADOR", "MANAUS"),
    n_missing_species = c(2, 0),
    geometry = sf::st_sfc(.gaps_square(0, 0), .gaps_square(2, 0))
  )

  plots <- .make_gap_plots(
    herbarium = "RB",
    group_gap = data.frame(group = "Angiosperms", n_missing = 3),
    family_gap = data.frame(family = "Fabaceae", n_missing = 3),
    genus_gap = data.frame(genus = c("Aster", "Inga"),
                           family = c("Asteraceae", "Fabaceae"),
                           n_missing = c(2, 1), in_herb = c(FALSE, TRUE)),
    domain_gap = data.frame(phytogeographicDomain = "Amazonia", n_missing = 2),
    endemism_gap = data.frame(endemism_label = "Non-endemic", n_missing = 3),
    state_gap = data.frame(abbrev_state = c("BA", "AM"), n_missing = c(1, 2)),
    muni_sf = muni_sf,
    n_missing = 3
  )

  expect_equal(read_args$year, 2020)
  expect_false(read_args$showProgress)

  expect_s3_class(plots$state, "ggplot")
  sd <- plots$state$data
  expect_equal(sd$n_missing[sd$abbrev_state == "BA"], 1)
  expect_equal(sd$n_missing[sd$abbrev_state == "AM"], 2)
  # States without missing taxa are filled with zero instead of NA
  expect_equal(sd$n_missing[sd$abbrev_state == "PE"], 0)
  expect_match(plots$state$labels$title, "Missing taxa by Brazilian state")

  expect_s3_class(plots$priority_map, "ggplot")
  expect_identical(plots$priority_map$data, muni_sf)
  expect_match(plots$priority_map$labels$title,
               "Priority municipalities for collecting expeditions .* RB")
  expect_match(plots$group$labels$subtitle, "Total missing from FFB: 3",
               fixed = TRUE)
})
