.missing_test_fixture <- function() {
  taxon <- data.frame(
    id = as.character(1:7),
    taxonomicStatus = c(rep("NOME_ACEITO", 6), "SINONIMO"),
    taxonRank = rep("ESPECIE", 7),
    group = rep("Angiosperms", 7),
    kingdom = rep("Plantae", 7),
    phylum = rep("Tracheophyta", 7),
    class = rep("Magnoliopsida", 7),
    order = c("Fabales", "Fabales", "Fabales", "Fabales", "Asterales",
              "Asterales", "Fabales"),
    family = c("Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae", "Asteraceae",
               "Asteraceae", "Fabaceae"),
    genus = c("Inga", "Inga", "Mimosa", "Mimosa", "Baccharis", "Aster", "Inga"),
    taxonName = c("Inga edulis", "Inga vera", "Mimosa pudica", "Mimosa sensitiva",
                  "Baccharis dracunculifolia", "Aster alba", "Inga synonym"),
    scientificNameAuthorship = c("Mart.", "Willd.", "L.", "L.", "DC.", "L.", "Syn."),
    references = paste0("ref", 1:7),
    stringsAsFactors = FALSE
  )

  distribution <- data.frame(
    id = c("1", "2", "3", "4", "5", "6"),
    locationID = c("BR-BA", "BR-BA", "BR-BA", "BR-BA", "BR-BA", "BR-SP"),
    phytogeographicDomain = c("Mata Atlantica", "Mata Atlantica", "Caatinga",
                              "Caatinga", "Cerrado", "Mata Atlantica"),
    endemism = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  herb <- data.frame(
    genus = c("Inga", "Mimosa"),
    species = c("synonym", "pudica"),
    taxonName = c("Inga synonym", "Mimosa pudica"),
    stringsAsFactors = FALSE
  )

  pool <- data.frame(
    occurrenceID = c("occ-1", "occ-2", "occ-3", "occ-4"),
    genus = c("Inga", "Baccharis", "Aster", "Inga"),
    species = c("edulis", "dracunculifolia", "alba", "edulis"),
    taxonName = c("Inga edulis", "Baccharis dracunculifolia", "Aster alba",
                  "Inga edulis"),
    family = c("Fabaceae", "Asteraceae", "Asteraceae", "Fabaceae"),
    collectionCode = c("ALCB", "ALCB", "SP", "RB"),
    stateProvince = c("Bahia", "Bahia", "São Paulo", "Bahia"),
    municipality = c("Feira de Santana", "Salvador", "Campinas", "Ilhéus"),
    decimalLatitude = c(-12.25, -12.97, -22.90, -14.79),
    decimalLongitude = c(-38.96, -38.50, -47.06, -39.04),
    recordedBy = c("Silva, J.", "Costa, M.", "Souza, T.", "Cardoso, D."),
    recordNumber = c("100", "200", "300", "400"),
    eventDate = c("2020-01-01", "2021-05-01", "2019-01-01", "2022-01-01"),
    stringsAsFactors = FALSE
  )

  multimedia_dwca <- list(
    ALCB_v1 = list(
      data = list(
        multimedia.txt = data.frame(
          id = "occ-1",
          references = "https://example.org/specimen/occ-1",
          identifier = "example.org/img/occ-1.jpg",
          stringsAsFactors = FALSE
        )
      )
    )
  )

  list(
    ffb = list(
      taxon = taxon,
      distribution = distribution,
      speciesprofile = data.frame(),
      typesandspecimen = data.frame()
    ),
    herb = herb,
    pool = pool,
    multimedia_dwca = multimedia_dwca
  )
}


.mock_missing_report_render <- function(input, output_file, params, envir, quiet) {
  writeLines("<html><body>mock report</body></html>", output_file)
  invisible(output_file)
}


test_that("jabot_missing validates required arguments", {
  expect_error(
    jabot_missing(herbarium = NULL, geo = "Bahia"),
    "'herbarium' must be provided"
  )

  expect_error(
    jabot_missing(herbarium = c("RB", "ALCB"), geo = "Bahia"),
    "'herbarium' must be a single herbarium acronym"
  )

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .package = "jabotR"
  )

  expect_error(
    jabot_missing(herbarium = "RB", geo = NULL),
    "'geo' must be provided"
  )

  expect_error(
    jabot_missing(herbarium = "RB", geo = "Bahia", format = "svg"),
    "'format' must be 'pdf', 'png', or both"
  )
})


test_that("jabot_missing computes missing species with deterministic input data", {
  fixture <- .missing_test_fixture()
  tmp <- tempfile("jabot_missing_")
  render_calls <- new.env(parent = emptyenv())
  render_calls$n <- 0L
  save_calls <- new.env(parent = emptyenv())
  save_calls$format <- NULL
  save_calls$fig_dir <- NULL
  parse_calls <- new.env(parent = emptyenv())
  parse_calls$herbarium <- NULL
  pool_calls <- new.env(parent = emptyenv())
  pool_calls$network_herbaria <- NULL

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .load_pool_data = function(network_herbaria, jabot_path, verbose) {
      pool_calls$network_herbaria <- network_herbaria
      fixture$pool
    },
    .load_municipality_sf = function(state_abbrevs, verbose) stop("offline fixture"),
    .make_missing_plots = function(...) list(mock_plot = TRUE),
    .save_missing_figures = function(plots, fig_dir, format, width, height, verbose) {
      save_calls$format <- format
      save_calls$fig_dir <- fig_dir
      invisible(NULL)
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    jabot_parse = function(path, herbarium, verbose) {
      parse_calls$herbarium <- herbarium
      fixture$multimedia_dwca
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
      .mock_missing_report_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  result <- suppressWarnings(jabot_missing(
    herbarium = " rb ",
    geo = "Bahia",
    network_herbaria = c(" alcb ", "rb"),
    format = c("PDF", " png "),
    open_report = FALSE,
    verbose = FALSE,
    dir = tmp
  ))

  expect_type(result, "list")
  expect_equal(result$herbarium, "RB")
  expect_equal(result$region_label, "Bahia")
  expect_equal(result$n_ffb_region, 5)
  expect_equal(result$n_in_herb, 2)
  expect_equal(result$n_missing, 3)
  expect_equal(result$pct_missing, 60)
  expect_equal(result$n_missing_locatable, 2)

  expect_setequal(
    result$taxon_missing$taxonName,
    c("Inga edulis", "Mimosa sensitiva", "Baccharis dracunculifolia")
  )
  expect_equal(
    result$taxon_missing$n_records[result$taxon_missing$taxonName == "Inga edulis"],
    1
  )
  expect_equal(
    result$taxon_missing$herbaria[result$taxon_missing$taxonName == "Inga edulis"],
    "ALCB"
  )
  expect_false(
    result$taxon_missing$locatable[result$taxon_missing$taxonName == "Mimosa sensitiva"]
  )

  expect_equal(
    result$family_gap$n_missing[result$family_gap$family == "Fabaceae"], 2
  )
  expect_equal(
    result$family_gap$n_missing[result$family_gap$family == "Asteraceae"], 1
  )

  expect_equal(
    result$evidence_gap$n[result$evidence_gap$status == "Locatable via network pool"],
    2
  )

  # Records/herbaria not matching the target region or belonging to the
  # target herbarium itself must be excluded from the candidate pool
  expect_false("Aster alba" %in% result$missing_pool_points$taxonName)
  expect_false("RB" %in% result$missing_pool_records$herbarium)

  # Municipality-level collecting priority
  expect_setequal(result$municipality_priority$municipality,
                  c("Feira de Santana", "Salvador"))
  expect_equal(
    result$municipality_priority$n_missing_species[
      result$municipality_priority$municipality == "Feira de Santana"],
    1
  )
  expect_null(result$muni_sf)

  # Specimen links: populated from the multimedia extension when available,
  # NA when the source herbarium provides none
  expect_equal(nrow(result$missing_pool_records), 2)
  expect_equal(
    result$missing_pool_records$specimenLink[
      result$missing_pool_records$taxonName == "Inga edulis"],
    "https://example.org/specimen/occ-1"
  )
  expect_true(
    is.na(result$missing_pool_records$specimenLink[
      result$missing_pool_records$taxonName == "Baccharis dracunculifolia"])
  )

  # jabot_parse() for multimedia links must be restricted to the herbaria
  # actually present in the region-filtered pool (not the whole network)
  expect_equal(parse_calls$herbarium, "ALCB")

  # network_herbaria must be normalised and forwarded to the pool loader
  expect_equal(pool_calls$network_herbaria, c("ALCB", "RB"))

  expect_true(dir.exists(tmp))
  expect_true(dir.exists(file.path(tmp, "figures")))
  expect_true(file.exists(file.path(tmp, "RB_missing_Bahia.html")))
  expect_equal(save_calls$format, c("pdf", "png"))
  expect_equal(save_calls$fig_dir, file.path(tmp, "figures"))
  expect_equal(render_calls$n, 1L)

  unlink(tmp, recursive = TRUE)
})


test_that("jabot_missing errors when 'geo' matches no pooled records", {
  fixture <- .missing_test_fixture()
  tmp <- tempfile("jabot_missing_")

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .load_pool_data = function(network_herbaria, jabot_path, verbose) fixture$pool,
    .package = "jabotR"
  )

  expect_error(
    suppressWarnings(jabot_missing(
      herbarium = "RB",
      geo = "Roraima",
      open_report = FALSE,
      verbose = FALSE,
      dir = tmp
    )),
    "No JABOT records were found for the given 'geo'"
  )

  unlink(tmp, recursive = TRUE)
})


test_that("jabot_missing warns (verbose) when municipality polygons fail to load", {
  fixture <- .missing_test_fixture()
  tmp <- tempfile("jabot_missing_")

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .load_ffb_data = function(verbose) fixture$ffb,
    .load_herb_data = function(herbarium, jabot_path, verbose) fixture$herb,
    .load_pool_data = function(network_herbaria, jabot_path, verbose) fixture$pool,
    .load_municipality_sf = function(state_abbrevs, verbose) stop("offline fixture"),
    .make_missing_plots = function(...) list(mock_plot = TRUE),
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    jabot_parse = function(path, herbarium, verbose) list(),
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    flora_search = function(x, progress_bar, rm_flora_database) {
      data.frame(Search = x, Accepted.taxon.Name = rep("Inga vera", length(x)),
                family = rep("Fabaceae", length(x)), stringsAsFactors = FALSE)
    },
    .package = "floraR"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      .mock_missing_report_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  expect_message(
    suppressWarnings(jabot_missing(
      herbarium = "RB",
      geo = "Bahia",
      open_report = FALSE,
      verbose = TRUE,
      dir = tmp
    )),
    "municipality polygons could not be loaded"
  )

  unlink(tmp, recursive = TRUE)
})


test_that(".classify_geo splits states and municipalities correctly", {
  result <- .classify_geo(c("Bahia", "sp", "Feira de Santana", "sao paulo"))

  expect_setequal(result$state_full, c("Bahia", "São Paulo"))
  expect_setequal(result$state_abbrev, c("BA", "SP"))
  expect_equal(result$municipalities, "Feira de Santana")
})


test_that(".classify_geo returns empty state vectors when no state is recognised", {
  result <- .classify_geo(c("Feira de Santana", "Ilheus"))

  expect_length(result$state_full, 0)
  expect_length(result$state_abbrev, 0)
  expect_equal(result$municipalities, c("Feira de Santana", "Ilheus"))
})


test_that(".br_states returns the 27 Brazilian federative units", {
  states <- .br_states()

  expect_length(states, 27)
  expect_equal(unname(states["Bahia"]), "BA")
  expect_equal(unname(states["São Paulo"]), "SP")
  expect_equal(unname(states["Distrito Federal"]), "DF")
})


test_that(".compute_municipality_priority aggregates missing-species vouchers by municipality", {
  pool_df <- data.frame(
    taxonName = c("Inga edulis", "Inga edulis", "Mimosa pudica", "Inga edulis"),
    collectionCode = c("ALCB", "HUEFS", "ALCB", "RB"),
    municipality = c("Feira de Santana", "Feira de Santana", "Salvador", "Ilheus"),
    stateProvince = c("Bahia", "Bahia", "Bahia", "Bahia"),
    stringsAsFactors = FALSE
  )

  result <- .compute_municipality_priority(
    pool_df, missing_taxonNames = "Inga edulis", herbarium = "RB"
  )

  # RB's own record (Ilheus) must be excluded, and "Mimosa pudica" is not a
  # missing species so its Salvador record must not appear
  expect_setequal(result$municipality, "Feira de Santana")
  expect_equal(result$n_records[result$municipality == "Feira de Santana"], 2)
  expect_equal(result$n_missing_species[result$municipality == "Feira de Santana"], 1)
  expect_equal(result$abbrev_state[result$municipality == "Feira de Santana"], "BA")
})


test_that(".compute_municipality_priority returns an empty data.frame when nothing matches", {
  pool_df <- data.frame(
    taxonName = "Mimosa pudica",
    collectionCode = "ALCB",
    municipality = "Salvador",
    stateProvince = "Bahia",
    stringsAsFactors = FALSE
  )

  result <- .compute_municipality_priority(
    pool_df, missing_taxonNames = "Inga edulis", herbarium = "RB"
  )

  expect_equal(nrow(result), 0)
})


test_that(".merge_multimedia_txt prefers references and falls back to identifier", {
  dwca_files <- list(
    herb_a = list(data = list(
      multimedia.txt = data.frame(
        id = c("occ-1", "occ-2"),
        references = c("https://example.org/view/occ-1", NA_character_),
        identifier = c("example.org/img/occ-1.jpg", "example.org/img/occ-2.jpg"),
        stringsAsFactors = FALSE
      )
    )),
    herb_b = list(data = list(
      multimedia.txt = data.frame(
        id = "occ-3",
        references = "http://already-has-scheme.org/occ-3",
        identifier = NA_character_,
        stringsAsFactors = FALSE
      )
    )),
    herb_c_no_media = list(data = list()),
    herb_d_empty = list(data = list(
      multimedia.txt = data.frame(id = character(0), references = character(0),
                                  identifier = character(0))
    ))
  )

  result <- .merge_multimedia_txt(dwca_files)

  expect_setequal(result$occurrenceID, c("occ-1", "occ-2", "occ-3"))
  expect_equal(
    result$specimenLink[result$occurrenceID == "occ-1"],
    "https://example.org/view/occ-1"
  )
  expect_equal(
    result$specimenLink[result$occurrenceID == "occ-2"],
    "https://example.org/img/occ-2.jpg"
  )
  expect_equal(
    result$specimenLink[result$occurrenceID == "occ-3"],
    "http://already-has-scheme.org/occ-3"
  )
})


test_that(".merge_multimedia_txt de-duplicates repeated occurrenceIDs", {
  dwca_files <- list(
    herb_a = list(data = list(
      multimedia.txt = data.frame(
        id = c("occ-1", "occ-1"),
        references = c("https://example.org/first", "https://example.org/second"),
        stringsAsFactors = FALSE
      )
    ))
  )

  result <- .merge_multimedia_txt(dwca_files)
  expect_equal(nrow(result), 1)
})


test_that(".merge_multimedia_txt returns an empty data.frame with no multimedia data", {
  result <- .merge_multimedia_txt(list(herb_a = list(data = list())))
  expect_equal(nrow(result), 0)
  expect_named(result, c("occurrenceID", "specimenLink"))
})


test_that(".load_municipality_sf loads and binds municipalities for multiple states", {
  testthat::local_mocked_bindings(
    read_municipality = function(code_muni, year, showProgress) {
      data.frame(
        code_muni = 1,
        name_muni = paste0("Muni of ", code_muni),
        abbrev_state = code_muni,
        stringsAsFactors = FALSE
      )
    },
    .package = "geobr"
  )

  result <- .load_municipality_sf(c("BA", "SE"), verbose = FALSE)

  expect_equal(nrow(result), 2)
  expect_setequal(result$abbrev_state, c("BA", "SE"))
  expect_true("muni_key" %in% names(result))
  expect_equal(result$muni_key[result$abbrev_state == "BA"], "MUNI OF BA")
})


test_that(".load_municipality_sf requests the whole country when state_abbrevs is NULL or 'all'", {
  calls <- new.env(parent = emptyenv())
  calls$code_muni <- list()

  testthat::local_mocked_bindings(
    read_municipality = function(code_muni, year, showProgress) {
      calls$code_muni <- c(calls$code_muni, code_muni)
      data.frame(code_muni = 1, name_muni = "Some Municipality",
                stringsAsFactors = FALSE)
    },
    .package = "geobr"
  )

  .load_municipality_sf(NULL, verbose = FALSE)
  .load_municipality_sf("all", verbose = FALSE)

  expect_equal(unlist(calls$code_muni), c("all", "all"))
})


test_that(".make_missing_plots returns ggplot objects and skips the priority map when muni_sf is NULL", {
  family_gap <- data.frame(family = c("Fabaceae", "Asteraceae"), n_missing = c(2, 1))
  evidence_gap <- data.frame(
    status = c("Locatable via network pool", "No known locality in pool"),
    n = c(2, 1)
  )

  plots <- .make_missing_plots(
    herbarium = "RB",
    region_label = "Bahia",
    family_gap = family_gap,
    evidence_gap = evidence_gap,
    muni_sf = NULL,
    n_missing = 3
  )

  expect_named(plots, c("family", "evidence", "priority_map"))
  expect_true(inherits(plots$family, "ggplot"))
  expect_true(inherits(plots$evidence, "ggplot"))
  expect_null(plots$priority_map)
})


test_that(".make_missing_plots builds the priority choropleth when muni_sf is provided", {
  family_gap <- data.frame(family = "Fabaceae", n_missing = 2)
  evidence_gap <- data.frame(
    status = c("Locatable via network pool", "No known locality in pool"),
    n = c(2, 1)
  )

  square <- sf::st_polygon(list(rbind(
    c(-39.1, -12.4), c(-38.9, -12.4), c(-38.9, -12.1), c(-39.1, -12.1), c(-39.1, -12.4)
  )))
  muni_sf <- sf::st_sf(
    name_muni = "Feira de Santana",
    abbrev_state = "BA",
    n_missing_species = 1,
    geometry = sf::st_sfc(square)
  )

  plots <- .make_missing_plots(
    herbarium = "RB",
    region_label = "Bahia",
    family_gap = family_gap,
    evidence_gap = evidence_gap,
    muni_sf = muni_sf,
    n_missing = 2
  )

  expect_true(inherits(plots$priority_map, "ggplot"))
})


test_that(".save_missing_figures saves requested formats and skips NULL plots", {
  tmp <- tempfile("jabot_missing_figures_")
  dir.create(tmp)

  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  expect_message(
    .save_missing_figures(
      plots = list(family = p, priority_map = NULL),
      fig_dir = tmp,
      format = "pdf",
      width = 4,
      height = 3,
      verbose = TRUE
    ),
    "Saved:"
  )

  expect_true(file.exists(file.path(tmp, "01_missing_family.pdf")))
  expect_false(file.exists(file.path(tmp, "03_priority_municipality_map.pdf")))

  unlink(tmp, recursive = TRUE)
})
