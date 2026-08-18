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

  expect_named(plots, c("group", "genus", "domain", "endemism", "state"))
  expect_true(all(vapply(plots[c("group", "genus", "domain", "endemism")],
                         inherits, logical(1), what = "ggplot")))
  expect_null(plots$state)
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
