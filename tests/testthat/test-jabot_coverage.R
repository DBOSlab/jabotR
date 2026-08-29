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
