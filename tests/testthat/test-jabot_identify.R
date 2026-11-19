.identify_record <- function(id, herb, catalog, family = NA, genus = NA,
                             epithet = NA, collector = NA, number = NA,
                             identifiedBy = NA, source = "JABOT", link = NA) {
  species <- if (!is.na(genus) && !is.na(epithet)) paste(genus, epithet) else NA
  data.frame(
    occurrenceID = id,
    institutionCode = herb,
    collectionCode = herb,
    catalogNumber = catalog,
    taxonRank = NA_character_,
    kingdom = "Plantae",
    division = NA_character_,
    class = NA_character_,
    order = NA_character_,
    family = family,
    genus = genus,
    specificEpithet = epithet,
    species = species,
    infraspecificEpithet = NA_character_,
    taxonName = species,
    scientificNameAuthorship = NA_character_,
    scientificName = species,
    recordedBy = collector,
    recordNumber = number,
    eventDate = NA_character_,
    year = "2010",
    month = "5",
    day = "12",
    fieldNotes = NA_character_,
    occurrenceRemarks = NA_character_,
    country = "Brasil",
    countryCode = "BR",
    stateProvince = "Bahia",
    municipality = "Salvador",
    locality = NA_character_,
    minimumElevationInMeters = NA_character_,
    maximumElevationInMeters = NA_character_,
    decimalLatitude = NA_real_,
    decimalLongitude = NA_real_,
    verbatimLatitude = NA_character_,
    verbatimLongitude = NA_character_,
    identificationQualifier = NA_character_,
    typeStatus = NA_character_,
    identifiedBy = identifiedBy,
    dateIdentified = NA_character_,
    basisOfRecord = "PreservedSpecimen",
    .jabot_source = source,
    .jabot_link = as.character(link),
    stringsAsFactors = FALSE
  )
}

.identify_test_fixture <- function() {
  rec <- .identify_record
  occ <- rbind(
    # Indeterminate specimens of RB
    rec("rb1", "RB", "RB1", "Fabaceae", "Inga", NA, "Silva, J.", "100"),
    rec("rb2", "RB", "RB2", "Fabaceae", NA, NA, "Costa, M.", "200"),
    rec("rb3", "RB", "RB3", NA, NA, NA, "Souza, T.", "300"),
    rec("rb4", "RB", "RB4", "Fabaceae", "Inga", NA, "Lima, A.", "400"),
    rec("rb5", "RB", "RB5", "Fabaceae", "Inga", NA, "Reis, P.", "500"),
    rec("rb6", "RB", "RB6", "Fabaceae", "Inga", NA, "Nunes, F.", "600"),
    rec("rb7", "RB", "RB7", "Fabaceae", "Inga", NA, NA, NA),
    # Identified specimen of RB: not indeterminate
    rec("rb8", "RB", "RB8", "Fabaceae", "Inga", "edulis", "Alves, C.", "800"),
    # Duplicates in other herbaria
    rec("alcb1", "ALCB", "ALCB1", "Fabaceae", "Inga", "edulis", "Silva, J.", "100",
        identifiedBy = "Cardoso, D."),
    rec("huefs2", "HUEFS", "HUEFS2", "Fabaceae", "Mimosa", NA, "Costa, M.", "200"),
    rec("alcb3", "ALCB", "ALCB3", "Fabaceae", NA, NA, "Souza, T.", "300"),
    rec("alcb4", "ALCB", "ALCB4", "Fabaceae", "Mimosa", "pudica", "Lima, A.", "400"),
    rec("sp5", "SP", "SP5", "Fabaceae", "Inga", NA, "Reis, P.", "500"),
    # Same number, different collector: not a duplicate
    rec("alcb9", "ALCB", "ALCB9", "Fabaceae", "Inga", "vera", "Pereira, X.", "100")
  )
  occ
}

.mock_identify_render <- function(input, output_file, params, envir, quiet) {
  writeLines("<html><body>mock report</body></html>", output_file)
  invisible(output_file)
}


test_that("jabot_identify validates its arguments", {
  expect_error(
    jabot_identify(herbarium = NULL),
    "'herbarium' must be provided"
  )

  expect_error(
    jabot_identify(herbarium = c("RB", "ALCB")),
    "'herbarium' must be a single herbarium acronym"
  )

  expect_error(
    jabot_identify(herbarium = "RB", format = "svg"),
    "'format' must be 'pdf', 'png', or both"
  )

  expect_error(
    jabot_identify(herbarium = "RB", reflora = NA),
    "'reflora' must be TRUE or FALSE"
  )

  expect_error(
    jabot_identify(herbarium = "RB", jabot = FALSE, reflora = FALSE),
    "At least one of 'jabot' and 'reflora' must be TRUE"
  )
})


test_that(".resolve_identify_source looks in JABOT first, then in REFLORA", {
  testthat::local_mocked_bindings(
    .jabot_codes = function() c("RB", "MBM"),
    .reflora_codes = function() c("RB", "NY", "HUEFS"),
    .package = "jabotR"
  )

  expect_equal(.resolve_identify_source("RB"), "JABOT")
  expect_equal(.resolve_identify_source("HUEFS"), "REFLORA")
  expect_error(.resolve_identify_source("ZZZ"),
               "not found in JABOT nor in REFLORA")
})


test_that(".first_url, .std_occ_types and .merge_reflora_occur handle REFLORA records", {
  expect_equal(
    .first_url(c("https://a.org/1", NA, "https://b.org/2 | https://c.org/3", "")),
    c("https://a.org/1", NA, "https://b.org/2", NA)
  )

  occ <- data.frame(
    occurrenceID = c(1, 2), recordNumber = c(10, NA), year = c(2000L, 2001L),
    decimalLatitude = c("-12.5", "x"), division = c(NA, NA),
    associatedMedia = c("https://img.org/1.dzi", NA),
    stringsAsFactors = FALSE
  )
  merged <- .merge_reflora_occur(list(a = list(data = list(occurrence.txt = occ))))
  expect_equal(unique(merged$.jabot_source), "REFLORA")
  expect_equal(merged$.jabot_link, c("https://img.org/1.dzi", NA))
  expect_type(merged$recordNumber, "character")
  expect_type(merged$division, "character")
  expect_type(merged$decimalLatitude, "double")
})


test_that("herbaria with different column types can be combined", {
  # a logical all-NA column in one herbarium and a character one in another
  one <- data.frame(occurrenceID = "a", division = NA, year = 2000L,
                    stringsAsFactors = FALSE)
  two <- data.frame(occurrenceID = "b", division = "Magnoliophyta", year = "2001",
                    stringsAsFactors = FALSE)
  merged <- dplyr::bind_rows(.std_occ_types(one), .std_occ_types(two))
  expect_equal(merged$division, c(NA, "Magnoliophyta"))
  expect_equal(merged$year, c("2000", "2001"))
})


test_that(".dwca_codes reads the herbarium acronyms of the DwC-A folders", {
  dir <- tempfile("dwca_")
  dir.create(dir)
  for (f in c("dwca_mbm_v1_43", "dwca_jbrj_rb_v84_280", "dwca_hb_herbarium_v1_84",
              "dwca-furb-herbarium-v1_301", "readme"))
    dir.create(file.path(dir, f))

  expect_setequal(.dwca_codes(dir), c("MBM", "JBRJ", "HB", "FURB"))
  expect_setequal(.dwca_codes(dir, jabot = TRUE), c("MBM", "RB", "HB", "FURB"))
  expect_equal(.dwca_codes(file.path(dir, "none")), character(0))
  expect_equal(.dwca_codes(NULL), character(0))
})


test_that(".prepare_identify_databases downloads what is missing and skips REFLORA herbaria already in JABOT", {
  downloads <- new.env(parent = emptyenv())
  downloads$jabot <- "unset"
  downloads$reflora <- "unset"

  testthat::local_mocked_bindings(
    jabot_download = function(herbarium, verbose, dir) {
      downloads$jabot <- herbarium
      downloads$jabot_dir <- dir
      invisible(NULL)
    },
    .dwca_codes = function(path, jabot = FALSE) {
      if (jabot) c("MBM", "RB", "UPCB", "FURB") else
        c("NY", "K", "HUEFS", "FURB")
    },
    .reflora_codes = function() c("NY", "K", "HUEFS", "FURB", "MBM", "RB"),
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    reflora_download = function(herbarium, verbose, dir) {
      downloads$reflora <- herbarium
      downloads$reflora_dir <- dir
      invisible(NULL)
    },
    .package = "refloraR"
  )

  # JABOT herbarium, everything searched with the default folders
  db <- .prepare_identify_databases(
    "RB", "JABOT", jabot = TRUE, network_herbaria = NULL, reflora = TRUE,
    reflora_herbaria = NULL, jabot_path = NULL, reflora_path = NULL,
    verbose = FALSE)

  expect_null(downloads$jabot)
  expect_equal(downloads$jabot_dir, "jabot_download")
  expect_equal(downloads$reflora_dir, "reflora_download")
  expect_equal(db$jabot_codes, c("MBM", "UPCB", "FURB"))
  # MBM, UPCB, FURB (in JABOT) and RB (the herbarium) are not taken from REFLORA
  expect_setequal(downloads$reflora, c("NY", "K", "HUEFS"))
  expect_setequal(db$reflora_codes, c("NY", "K", "HUEFS"))

  # REFLORA herbarium, only REFLORA as the source of duplicates
  downloads$jabot <- "unset"
  db2 <- .prepare_identify_databases(
    "HUEFS", "REFLORA", jabot = FALSE, network_herbaria = NULL, reflora = TRUE,
    reflora_herbaria = c("NY", "K"), jabot_path = NULL, reflora_path = NULL,
    verbose = FALSE)
  expect_equal(downloads$jabot, "unset")
  expect_setequal(downloads$reflora, c("NY", "K", "HUEFS"))
  expect_equal(db2$jabot_codes, character(0))
  expect_setequal(db2$reflora_codes, c("NY", "K"))

  # paths given: nothing is downloaded
  downloads$jabot <- "unset"
  downloads$reflora <- "unset"
  db3 <- .prepare_identify_databases(
    "RB", "JABOT", jabot = TRUE, network_herbaria = c("MBM", "ZZZ"),
    reflora = TRUE, reflora_herbaria = c("NY", "MBM"), jabot_path = "jp",
    reflora_path = "rp", verbose = FALSE)
  expect_equal(downloads$jabot, "unset")
  expect_equal(downloads$reflora, "unset")
  expect_equal(db3$jabot_codes, "MBM")
  expect_equal(db3$reflora_codes, "NY")
})


test_that(".load_identify_candidates reads the herbaria of both sources", {
  target <- .identify_record("t1", "RB", "RB1", "Fabaceae", "Inga", NA,
                             "Silva, J.", "100")
  data <- list(
    JABOT = list(
      MBM = .identify_record("m1", "MBM", "MBM1", "Fabaceae", "Inga", "edulis",
                             "Silva, J.", "100"),
      FURB = .identify_record("f1", "FURB", "FURB1", "Fabaceae", "Inga", "vera",
                              "Souza, T.", "100")
    ),
    REFLORA = list(
      NY = .identify_record("n1", "NY", "NY0001", "Fabaceae", "Inga", "edulis",
                            "J. Silva", "100", source = "REFLORA",
                            link = "https://img.org/n1")
    )
  )
  seen <- new.env(parent = emptyenv())
  seen$jabot <- character(0)
  seen$reflora <- character(0)

  testthat::local_mocked_bindings(
    .read_jabot_herb = function(path, herbarium) {
      seen$jabot <- c(seen$jabot, herbarium)
      data$JABOT[[herbarium]]
    },
    .read_reflora_herb = function(path, herbarium) {
      seen$reflora <- c(seen$reflora, herbarium)
      data$REFLORA[[herbarium]]
    },
    .package = "jabotR"
  )

  out <- .load_identify_candidates(target, "jp", c("MBM", "FURB"), "rp", "NY",
                                   verbose = FALSE)

  expect_equal(seen$jabot, c("MBM", "FURB"))
  expect_equal(seen$reflora, "NY")
  # FURB has another collector, so it is not a plausible duplicate
  expect_equal(out$occurrenceID, c("m1", "n1"))
  expect_equal(out$.jabot_source, c("JABOT", "REFLORA"))
  expect_equal(out$.jabot_link[out$occurrenceID == "n1"], "https://img.org/n1")

  none <- .load_identify_candidates(target, "jp", character(0), "rp",
                                    character(0), verbose = FALSE)
  expect_equal(nrow(none), 0)
})


test_that(".rank_missing and .is_indet detect indeterminate records", {
  expect_equal(
    .rank_missing(c("Inga", NA, "", "sp.", "cf.", "edulis")),
    c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )

  df <- data.frame(
    family = c("Fabaceae", "Fabaceae", NA, "Fabaceae"),
    genus = c("Inga", "Inga", "Inga", NA),
    specificEpithet = c("edulis", NA, "edulis", "edulis"),
    stringsAsFactors = FALSE
  )
  expect_equal(.is_indet(df), c(FALSE, TRUE, TRUE, TRUE))
})


test_that(".select_candidate_duplicates keeps only plausible duplicates", {
  target <- .identify_record("t1", "RB", "RB1", "Fabaceae", "Inga", NA,
                             "Silva, J.", "0100")
  pool <- rbind(
    .identify_record("p1", "ALCB", "A1", "Fabaceae", "Inga", "edulis",
                     "J. Silva", "100"),
    .identify_record("p2", "ALCB", "A2", "Fabaceae", "Inga", "edulis",
                     "Pereira, X.", "100"),
    .identify_record("p3", "ALCB", "A3", "Fabaceae", "Inga", "edulis",
                     "Silva, J.", "101")
  )

  cand <- .select_candidate_duplicates(target, pool)
  expect_equal(cand$occurrenceID, "p1")

  expect_equal(nrow(.select_candidate_duplicates(target, pool[0, ])), 0)
})


test_that(".compute_identify classifies the outcome of each indeterminate specimen", {
  std <- data.frame(
    .jabot_row = 1:6,
    .jabot_target = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    occurrenceID = c("a", "b", "c", "d", "e", "f"),
    collectionCode = c("RB", "RB", "RB", "RB", "ALCB", "SP"),
    catalogNumber = c("RB1", "RB2", "RB3", "RB4", "ALCB1", "SP1"),
    family = c("Fabaceae", "Fabaceae", NA, "Fabaceae", "Fabaceae", "Fabaceae"),
    genus = c("Inga", NA, NA, "Inga", "Inga", "Mimosa"),
    specificEpithet = c(NA, NA, NA, NA, "edulis", "pudica"),
    recordedBy = c("J. Silva", "M. Costa", "Unknown", "A. Lima", "J. Silva",
                   "A. Lima"),
    recordNumber = c("1", "2", NA, "4", "1", "4"),
    year = "2010", month = "5", day = "12",
    duplicateGroup = c(1L, NA, NA, 2L, 1L, 2L),
    identifiedBy = c(NA, NA, NA, NA, "D. Cardoso", NA),
    dateIdentified = NA_character_,
    .jabot_source = c("JABOT", "JABOT", "JABOT", "JABOT", "REFLORA", "JABOT"),
    .jabot_link = c("https://t/rb1", NA, NA, NA, "https://r/alcb1", NA),
    stringsAsFactors = FALSE
  )

  res <- .compute_identify(std)
  oc <- .identify_outcomes()
  ind <- res$indets
  by_cat <- setNames(ind$outcome, ind$catalogNumber)

  expect_equal(nrow(ind), 4)
  expect_equal(unname(by_cat["RB1"]), oc[["species"]])
  expect_equal(unname(by_cat["RB2"]), oc[["no_dups"]])
  expect_equal(
    names(ind)[match("recordNumber", names(ind)) + 1:2],
    c("identifiedBy", "dateIdentified")
  )
  expect_equal(
    names(res$suggestions)[match("recordNumber", names(res$suggestions)) + 1:2],
    c("identifiedBy", "dateIdentified")
  )
  expect_equal(unname(by_cat["RB3"]), oc[["unmatchable"]])
  expect_equal(unname(by_cat["RB4"]), oc[["conflict"]])

  rb1 <- ind[ind$catalogNumber == "RB1", ]
  expect_equal(rb1$suggestedIdentification, "Inga edulis")
  expect_equal(rb1$suggestedFrom, "ALCB ALCB1")
  expect_equal(rb1$suggestedIdentifiedBy, "D. Cardoso")
  expect_equal(rb1$n_useful, 1)
  expect_equal(rb1$source, "JABOT")
  expect_equal(rb1$suggestedSource, "REFLORA")
  expect_equal(rb1$specimenLink, "https://t/rb1")
  expect_equal(rb1$suggestedSpecimenLink, "https://r/alcb1")

  expect_equal(nrow(res$suggestions), 2)
  conflict <- res$suggestions[res$suggestions$catalogNumber == "RB4", ]
  expect_true(conflict$fills_missing_rank)
  expect_false(conflict$compatible)

  src <- .identify_source_summary(res$suggestions)
  expect_equal(src$herbarium, "ALCB")
  expect_equal(src$n_specimens, 1)
})


test_that("jabot_identify suggests identifications with barRoso and builds the report", {
  skip_if_not_installed("barRoso")

  occ <- .identify_test_fixture()
  tmp <- tempfile("jabot_identify_")
  render_calls <- new.env(parent = emptyenv())
  render_calls$n <- 0L
  load_calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .jabot_codes = function() c("RB", "ALCB", "HUEFS", "SP"),
    .reflora_codes = function() c("RB", "NY", "K"),
    .prepare_identify_databases = function(herbarium, source, jabot,
                                           network_herbaria, reflora,
                                           reflora_herbaria, jabot_path,
                                           reflora_path, verbose) {
      load_calls$source <- source
      load_calls$jabot <- jabot
      load_calls$network_herbaria <- network_herbaria
      load_calls$reflora <- reflora
      load_calls$reflora_herbaria <- reflora_herbaria
      list(jabot_dir = "jp", reflora_dir = "rp",
           jabot_codes = c("ALCB", "HUEFS", "SP"),
           reflora_codes = c("NY", "K"))
    },
    .load_identify_target = function(herbarium, source, jabot_dir, reflora_dir) {
      occ[occ$collectionCode %in% herbarium, , drop = FALSE]
    },
    .load_identify_candidates = function(target, jabot_dir, jabot_codes,
                                         reflora_dir, reflora_codes, verbose) {
      occ[!occ$collectionCode %in% "RB", , drop = FALSE]
    },
    .load_identify_links = function(herbaria, jabot_path, verbose) {
      data.frame(occurrenceID = c("rb1", "alcb1"),
                 specimenLink = c("https://example.org/rb1",
                                  "https://example.org/alcb1"),
                 stringsAsFactors = FALSE)
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      render_calls$n <- render_calls$n + 1L
      .mock_identify_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  result <- suppressWarnings(jabot_identify(
    herbarium = " rb ",
    network_herbaria = c(" alcb ", "huefs", "sp"),
    reflora_herbaria = c("ny", "K"),
    format = "png",
    open_report = FALSE,
    verbose = FALSE,
    dir = tmp
  ))

  oc <- .identify_outcomes()
  ind <- result$indets
  by_cat <- setNames(as.character(ind$outcome), ind$catalogNumber)

  expect_equal(result$herbarium, "RB")
  expect_equal(result$herbarium_source, "JABOT")
  expect_equal(result$sources, c("JABOT", "REFLORA"))
  expect_equal(load_calls$source, "JABOT")
  expect_true(load_calls$jabot)
  expect_true(load_calls$reflora)
  expect_equal(load_calls$network_herbaria, c("ALCB", "HUEFS", "SP"))
  expect_equal(load_calls$reflora_herbaria, c("NY", "K"))
  expect_equal(result$n_indets, 7)
  expect_false("RB8" %in% ind$catalogNumber)

  expect_equal(unname(by_cat["RB1"]), oc[["species"]])
  expect_equal(unname(by_cat["RB2"]), oc[["genus"]])
  expect_equal(unname(by_cat["RB3"]), oc[["family"]])
  expect_equal(unname(by_cat["RB4"]), oc[["conflict"]])
  expect_equal(unname(by_cat["RB5"]), oc[["none_better"]])
  expect_equal(unname(by_cat["RB6"]), oc[["no_dups"]])
  expect_equal(unname(by_cat["RB7"]), oc[["unmatchable"]])
  expect_equal(
    names(ind)[match("recordNumber", names(ind)) + 1:2],
    c("identifiedBy", "dateIdentified")
  )

  expect_equal(result$n_resolvable, 3)
  expect_equal(result$n_species, 1)
  expect_equal(result$n_conflict, 1)
  expect_equal(result$n_matchable, 6)
  expect_equal(
    ind$suggestedIdentification[ind$catalogNumber == "RB1"],
    "Inga edulis"
  )

  # Links to both the indeterminate specimen and the one that identifies it
  expect_equal(ind$specimenLink[ind$catalogNumber == "RB1"],
               "https://example.org/rb1")
  expect_equal(ind$suggestedSpecimenLink[ind$catalogNumber == "RB1"],
               "https://example.org/alcb1")
  expect_true(is.na(ind$specimenLink[ind$catalogNumber == "RB2"]))
  sug1 <- result$suggestions[result$suggestions$dup_catalogNumber == "ALCB1", ]
  expect_equal(sug1$specimenLink, "https://example.org/rb1")
  expect_equal(sug1$dup_specimenLink, "https://example.org/alcb1")
  expect_equal(sug1$dup_source, "JABOT")
  expect_false(any(c("occurrenceID", "dup_occurrenceID") %in%
                     names(result$suggestions)))
  expect_false("suggestedOccurrenceID" %in% names(ind))
  expect_false("ALCB9" %in% result$suggestions$dup_catalogNumber)

  expect_equal(render_calls$n, 1L)
  expect_true(file.exists(file.path(tmp, "RB_identify.html")))
  expect_true(file.exists(file.path(tmp, "RB_identify_indets.csv")))
  expect_true(file.exists(file.path(tmp, "RB_identify_suggestions.csv")))
  expect_true(file.exists(file.path(tmp, "figures", "01_identify_outcome.png")))
})


test_that("jabot_identify investigates a REFLORA herbarium with REFLORA duplicates", {
  skip_if_not_installed("barRoso")

  occ <- .identify_test_fixture()
  occ$.jabot_source[occ$collectionCode == "RB"] <- "REFLORA"
  occ$.jabot_source[occ$collectionCode == "ALCB"] <- "REFLORA"
  occ$.jabot_link[occ$catalogNumber == "ALCB1"] <- "https://reflora.org/alcb1"
  load_calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .jabot_codes = function() c("SP"),
    .reflora_codes = function() c("RB", "ALCB", "HUEFS"),
    .prepare_identify_databases = function(herbarium, source, jabot, ...) {
      load_calls$source <- source
      load_calls$jabot <- jabot
      list(jabot_dir = "jp", reflora_dir = "rp", jabot_codes = character(0),
           reflora_codes = c("ALCB", "HUEFS"))
    },
    .load_identify_target = function(herbarium, source, jabot_dir, reflora_dir) {
      occ[occ$collectionCode %in% herbarium, , drop = FALSE]
    },
    .load_identify_candidates = function(target, ...) {
      occ[!occ$collectionCode %in% "RB", , drop = FALSE]
    },
    .load_identify_links = function(herbaria, jabot_path, verbose) {
      data.frame(occurrenceID = character(0), specimenLink = character(0))
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      .mock_identify_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  result <- suppressWarnings(jabot_identify(
    herbarium = "RB", jabot = FALSE, open_report = FALSE, verbose = FALSE,
    dir = tempfile("jabot_identify_")
  ))

  expect_equal(result$herbarium_source, "REFLORA")
  expect_equal(result$sources, "REFLORA")
  expect_equal(load_calls$source, "REFLORA")
  expect_false(load_calls$jabot)
  ind <- result$indets
  expect_equal(unique(ind$source), "REFLORA")
  rb1 <- ind[ind$catalogNumber == "RB1", ]
  expect_equal(rb1$suggestedSource, "REFLORA")
  expect_equal(rb1$suggestedSpecimenLink, "https://reflora.org/alcb1")

  expect_error(
    jabot_identify(herbarium = "ZZZ", open_report = FALSE, verbose = FALSE),
    "not found in JABOT nor in REFLORA"
  )
  expect_error(
    jabot_identify(herbarium = "RB", reflora_herbaria = "QQQ",
                   open_report = FALSE, verbose = FALSE),
    "not recognized by REFLORA"
  )
})


test_that("jabot_identify restricts the indeterminate specimens by taxon", {
  skip_if_not_installed("barRoso")

  occ <- .identify_test_fixture()
  occ$family[occ$catalogNumber == "RB4"] <- "Asteraceae"
  occ$genus[occ$catalogNumber == "RB4"] <- "Baccharis"

  testthat::local_mocked_bindings(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .jabot_codes = function() c("RB", "ALCB"),
    .reflora_codes = function() c("RB", "NY"),
    .prepare_identify_databases = function(...) {
      list(jabot_dir = "jp", reflora_dir = "rp", jabot_codes = "ALCB",
           reflora_codes = "NY")
    },
    .load_identify_target = function(herbarium, source, jabot_dir, reflora_dir) {
      occ[occ$collectionCode %in% herbarium, , drop = FALSE]
    },
    .load_identify_candidates = function(target, ...) {
      occ[!occ$collectionCode %in% "RB", , drop = FALSE]
    },
    .load_identify_links = function(herbaria, jabot_path, verbose) {
      data.frame(occurrenceID = character(0), specimenLink = character(0))
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      .mock_identify_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )

  by_genus <- suppressWarnings(jabot_identify(
    herbarium = "RB", taxon = "Baccharis", open_report = FALSE,
    verbose = FALSE, dir = tempfile("jabot_identify_")
  ))
  expect_equal(by_genus$indets$catalogNumber, "RB4")

  by_family <- suppressWarnings(jabot_identify(
    herbarium = "RB", taxon = "fabaceae", open_report = FALSE,
    verbose = FALSE, dir = tempfile("jabot_identify_")
  ))
  expect_false("RB4" %in% by_family$indets$catalogNumber)
  expect_equal(by_family$n_indets, 5)

  expect_error(
    jabot_identify(herbarium = "RB", taxon = "Rosaceae", open_report = FALSE,
                   verbose = FALSE, dir = tempfile("jabot_identify_")),
    "belongs to the given 'taxon'"
  )
})


# ---- Additional coverage ---------------------------------------------------

# Mocks base::system.file (or the pkgload shim under devtools::load_all()) so
# that the bundled Rmd template appears to be missing.
.identify_local_missing_template <- function(env = parent.frame()) {
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


# Mocks the database layer of jabot_identify() with the identify fixture.
.identify_local_db_mocks <- function(occ, env = parent.frame(), ...) {
  mocks <- list(
    .arg_check_herbarium = function(x, verbose) invisible(TRUE),
    .jabot_codes = function() c("RB", "ALCB", "HUEFS", "SP"),
    .reflora_codes = function() c("RB", "NY", "K"),
    .prepare_identify_databases = function(...) {
      list(jabot_dir = "jp", reflora_dir = "rp",
           jabot_codes = c("ALCB", "HUEFS", "SP"), reflora_codes = c("NY", "K"))
    },
    .load_identify_target = function(herbarium, source, jabot_dir, reflora_dir) {
      occ[occ$collectionCode %in% herbarium, , drop = FALSE]
    },
    .load_identify_candidates = function(target, ...) {
      occ[!occ$collectionCode %in% "RB", , drop = FALSE]
    },
    .load_identify_links = function(herbaria, jabot_path, verbose) {
      data.frame(occurrenceID = character(0), specimenLink = character(0))
    }
  )
  overrides <- list(...)
  mocks[names(overrides)] <- overrides
  do.call(testthat::local_mocked_bindings,
          c(mocks, list(.package = "jabotR", .env = env)))
}


test_that("jabot_identify requires barRoso and refloraR", {
  real <- base::requireNamespace
  for (pkg in c("barRoso", "refloraR")) {
    expect_error(
      testthat::with_mocked_bindings(
        jabot_identify(herbarium = "RB", verbose = FALSE),
        requireNamespace = function(package, ..., quietly = TRUE) {
          if (identical(package, pkg)) FALSE else real(package, ..., quietly = quietly)
        },
        .package = "base"
      ),
      paste0("The package '", pkg, "' is required.*",
             "remotes::install_github\\('DBOSlab/", pkg, "'\\)")
    )
  }
})


test_that("jabot_identify errors when the herbarium has no (indeterminate) records", {
  occ <- .identify_test_fixture()
  tmp <- withr::local_tempdir()

  .identify_local_db_mocks(
    occ,
    .load_identify_target = function(...) occ[0, , drop = FALSE]
  )
  expect_error(
    jabot_identify(herbarium = "RB", verbose = FALSE, dir = tmp),
    "No records were found for herbarium 'RB'"
  )

  .identify_local_db_mocks(
    occ,
    .load_identify_target = function(...) occ[occ$catalogNumber == "RB8", ]
  )
  expect_error(
    jabot_identify(herbarium = "RB", verbose = FALSE, dir = tmp),
    "Herbarium 'RB' has no indeterminate specimens"
  )
})


test_that("jabot_identify verbose run, default directory, blank taxon and report opening", {
  skip_if_not_installed("barRoso")

  occ <- .identify_test_fixture()
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  calls <- new.env(parent = emptyenv())

  .identify_local_db_mocks(occ)
  testthat::local_mocked_bindings(
    render = function(input, output_file, params, envir, quiet) {
      calls$quiet <- quiet
      .mock_identify_render(input, output_file, params, envir, quiet)
    },
    .package = "rmarkdown"
  )
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) calls$url <- url,
    .package = "utils"
  )

  msgs <- testthat::capture_messages(
    result <- suppressWarnings(testthat::with_mocked_bindings(
      jabot_identify(herbarium = "rb", taxon = c(" ", NA, ""),
                     open_report = TRUE, verbose = TRUE),
      .is_interactive = function() TRUE,
      .package = "jabotR"
    ))
  )
  msgs <- paste(msgs, collapse = "")

  for (step in c("[1/6] Checking the JABOT and REFLORA databases",
                 "[2/6] Reading the records of 'RB' (JABOT)",
                 "[3/6] Reading the other herbaria of JABOT and REFLORA",
                 "[4/6] Standardizing records", "[5/6] Comparing",
                 "[6/6] Building figures and report", "Rendering HTML report",
                 "Report saved:")) {
    expect_match(msgs, step, fixed = TRUE)
  }

  # Blank taxon values are dropped: all indeterminate specimens are used
  expect_null(result$taxon)
  expect_equal(result$taxon_label, "all indeterminate specimens")
  expect_equal(result$n_indets, 7)

  html <- file.path(wd, "RB_identify", "RB_identify.html")
  expect_true(file.exists(html))
  expect_true(file.exists(file.path(wd, "RB_identify", "RB_identify_indets.csv")))
  expect_false(calls$quiet)

  expect_equal(normalizePath(calls$url), normalizePath(html))
})


test_that("jabot_identify errors when the Rmd template is missing", {
  skip_if_not_installed("barRoso")

  occ <- .identify_test_fixture()
  tmp <- withr::local_tempdir()
  .identify_local_db_mocks(occ)
  .identify_local_missing_template()

  expect_error(
    suppressWarnings(jabot_identify(herbarium = "RB", verbose = FALSE,
                                    dir = tmp)),
    "Rmd template not found"
  )
  # The CSV tables are written before the report is rendered
  expect_true(file.exists(file.path(tmp, "RB_identify_suggestions.csv")))
  expect_false(file.exists(file.path(tmp, "RB_identify.html")))
})


test_that(".jabot_codes and .reflora_codes list the collection codes", {
  args <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    jabot_summary = function(herbarium, verbose, save) {
      args$jabot <- list(herbarium = herbarium, verbose = verbose, save = save)
      data.frame(collectionCode = c("RB", "MBM"), n = 1:2)
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    reflora_summary = function(herbarium, verbose, save) {
      args$reflora <- list(herbarium = herbarium, verbose = verbose, save = save)
      data.frame(collectionCode = c("NY", "K", "HUEFS"))
    },
    .package = "refloraR"
  )

  expect_equal(.jabot_codes(), c("RB", "MBM"))
  expect_equal(.reflora_codes(), c("NY", "K", "HUEFS"))
  expect_equal(args$jabot, list(herbarium = NULL, verbose = FALSE, save = FALSE))
  expect_equal(args$reflora, list(herbarium = NULL, verbose = FALSE, save = FALSE))
})


test_that(".read_jabot_herb, .read_reflora_herb and .load_identify_target parse one herbarium", {
  occ <- data.frame(occurrenceID = c("a", "b"), recordNumber = c(1, 2),
                    associatedMedia = c("see https://img.org/a.jpg", NA),
                    stringsAsFactors = FALSE)
  dwca <- list(x = list(data = list(occurrence.txt = occ)))
  args <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    jabot_parse = function(path, herbarium, verbose) {
      args$jabot <- list(path = path, herbarium = herbarium, verbose = verbose)
      dwca
    },
    .package = "jabotR"
  )
  testthat::local_mocked_bindings(
    reflora_parse = function(path, herbarium, verbose) {
      args$reflora <- list(path = path, herbarium = herbarium, verbose = verbose)
      dwca
    },
    .package = "refloraR"
  )

  jab <- .read_jabot_herb("jp", "RB")
  expect_equal(args$jabot, list(path = "jp", herbarium = "RB", verbose = FALSE))
  expect_equal(jab$.jabot_source, c("JABOT", "JABOT"))
  # JABOT links come from the multimedia extension, not associatedMedia
  expect_equal(jab$.jabot_link, c(NA_character_, NA_character_))
  expect_type(jab$recordNumber, "character")

  ref <- .read_reflora_herb("rp", "NY")
  expect_equal(args$reflora, list(path = "rp", herbarium = "NY", verbose = FALSE))
  expect_equal(ref$.jabot_source, c("REFLORA", "REFLORA"))
  expect_equal(ref$.jabot_link, c("https://img.org/a.jpg", NA))

  args$jabot <- NULL
  args$reflora <- NULL
  expect_equal(.load_identify_target("RB", "JABOT", "jp", "rp")$.jabot_source[1],
               "JABOT")
  expect_null(args$reflora)
  expect_equal(.load_identify_target("NY", "REFLORA", "jp", "rp")$.jabot_source[1],
               "REFLORA")
  expect_equal(args$reflora$path, "rp")
})


test_that(".prepare_identify_databases tells which REFLORA herbaria are taken from JABOT", {
  testthat::local_mocked_bindings(
    .dwca_codes = function(path, jabot = FALSE) {
      if (jabot) c("MBM", "RB", "FURB") else c("NY", "FURB")
    },
    .package = "jabotR"
  )

  expect_message(
    db <- .prepare_identify_databases(
      "RB", "JABOT", jabot = TRUE, network_herbaria = NULL, reflora = TRUE,
      reflora_herbaria = c("NY", "MBM", "FURB", "RB"), jabot_path = "jp",
      reflora_path = "rp", verbose = TRUE),
    "REFLORA herbaria already in JABOT, not used from REFLORA: MBM, FURB"
  )
  expect_equal(db$reflora_codes, "NY")
  expect_equal(db$jabot_codes, c("MBM", "FURB"))

  # Only the herbarium itself overlaps: nothing to report
  expect_silent(
    .prepare_identify_databases(
      "RB", "JABOT", jabot = TRUE, network_herbaria = NULL, reflora = TRUE,
      reflora_herbaria = c("NY", "RB"), jabot_path = "jp",
      reflora_path = "rp", verbose = TRUE)
  )
})


test_that(".scan_herbaria reports progress and skips unreadable or empty herbaria", {
  target <- .identify_record("t1", "RB", "RB1", "Fabaceae", "Inga", NA,
                             "Silva, J.", "100")
  good <- .identify_record("m1", "MBM", "MBM1", "Fabaceae", "Inga", "edulis",
                           "Silva, J.", "100")
  read_fun <- function(code) {
    switch(code,
           MBM = good,
           BAD = stop("corrupted archive"),
           EMPTY = good[0, ],
           NULL)
  }

  msgs <- testthat::capture_messages(
    out <- .scan_herbaria(c("BAD", "EMPTY", "MBM", "NONE"), read_fun, target,
                          "JABOT", verbose = TRUE)
  )

  expect_equal(out$occurrenceID, "m1")
  expect_equal(
    msgs,
    c("  [JABOT] BAD (1/4)\n",
      "  Note: BAD could not be read. corrupted archive\n",
      "  [JABOT] EMPTY (2/4)\n", "  [JABOT] MBM (3/4)\n",
      "  [JABOT] NONE (4/4)\n")
  )

  expect_silent(
    quiet <- .scan_herbaria(c("BAD", "MBM"), read_fun, target, "REFLORA",
                            verbose = FALSE)
  )
  expect_equal(quiet$occurrenceID, "m1")
})


test_that(".load_identify_links reads the multimedia links of the JABOT herbaria", {
  path <- withr::local_tempdir()
  for (f in c("dwca_alcb_v1_10", "dwca_jbrj_rb_v84_280"))
    dir.create(file.path(path, f))
  args <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    jabot_parse = function(path, herbarium, verbose) {
      args$herbarium <- herbarium
      args$verbose <- verbose
      list(ALCB = list(data = list(multimedia.txt = data.frame(
        id = "alcb1", references = "https://example.org/alcb1",
        stringsAsFactors = FALSE))))
    },
    .package = "jabotR"
  )

  links <- .load_identify_links(c("RB", NA, "ALCB", "ZZZ"), path, verbose = FALSE)
  expect_setequal(args$herbarium, c("RB", "ALCB"))
  expect_false(args$verbose)
  expect_equal(links$occurrenceID, "alcb1")
  expect_equal(links$specimenLink, "https://example.org/alcb1")

  # No herbarium with a DwC-A folder: empty table, nothing parsed
  args$herbarium <- "unset"
  none <- .load_identify_links(c("ZZZ", NA), path, verbose = TRUE)
  expect_equal(args$herbarium, "unset")
  expect_equal(nrow(none), 0)
  expect_named(none, c("occurrenceID", "specimenLink"))

  # Default folder 'jabot_download' in the working directory
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  expect_equal(nrow(.load_identify_links("RB", NULL, verbose = TRUE)), 0)
  dir.create(file.path("jabot_download", "dwca_jbrj_rb_v1"), recursive = TRUE)
  expect_equal(.load_identify_links("RB", NULL, verbose = FALSE)$occurrenceID,
               "alcb1")
  expect_equal(args$herbarium, "RB")
})


test_that(".load_identify_links returns no links when parsing fails", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "dwca_alcb_v1_10"))
  testthat::local_mocked_bindings(
    jabot_parse = function(path, herbarium, verbose) stop("bad archive"),
    .package = "jabotR"
  )

  expect_message(
    links <- .load_identify_links("ALCB", path, verbose = TRUE),
    "Note: specimen links could not be loaded. bad archive"
  )
  expect_equal(nrow(links), 0)
  expect_named(links, c("occurrenceID", "specimenLink"))

  expect_silent(quiet <- .load_identify_links("ALCB", path, verbose = FALSE))
  expect_equal(nrow(quiet), 0)
})


test_that(".select_candidate_duplicates needs a number or date to compare", {
  pool <- .identify_record("p1", "ALCB", "A1", "Fabaceae", "Inga", "edulis",
                           "J. Silva", "100")

  # Target without number and without a complete date
  no_base <- .identify_record("t1", "RB", "RB1", "Fabaceae", "Inga", NA,
                              "Silva, J.", NA)
  no_base$year <- NA
  res <- .select_candidate_duplicates(no_base, pool)
  expect_equal(nrow(res), 0)
  expect_named(res, names(pool))

  # Target with a number that no pool record shares
  other_number <- .identify_record("t2", "RB", "RB2", "Fabaceae", "Inga", NA,
                                   "Silva, J.", "999")
  res <- .select_candidate_duplicates(other_number, pool)
  expect_equal(nrow(res), 0)
  expect_named(res, names(pool))

  # Without a number, the collecting date is used when the genus is known
  by_date <- .identify_record("t3", "RB", "RB3", "Fabaceae", "Inga", NA,
                              "Silva, J.", NA)
  pool_date <- .identify_record("p2", "ALCB", "A2", "Fabaceae", "Inga", "vera",
                                "Silva, J.", NA)
  expect_equal(.select_candidate_duplicates(by_date, pool_date)$occurrenceID,
               "p2")
})


test_that(".compute_identify handles specimens without any duplicate group", {
  std <- data.frame(
    .jabot_row = 1:3,
    .jabot_target = c(TRUE, TRUE, FALSE),
    occurrenceID = c("a", "b", "c"),
    collectionCode = c("RB", "RB", "ALCB"),
    catalogNumber = c("RB1", "RB2", "ALCB1"),
    family = c("Fabaceae", NA, "Fabaceae"),
    genus = c("Inga", NA, "Inga"),
    specificEpithet = c(NA, NA, "edulis"),
    recordedBy = c("J. Silva", "Unknown", "M. Costa"),
    recordNumber = c("1", NA, "9"),
    year = "2010", month = "5", day = "12",
    duplicateGroup = c(NA_integer_, NA_integer_, 3L),
    stringsAsFactors = FALSE
  )

  res <- .compute_identify(std)
  oc <- .identify_outcomes()

  expect_equal(nrow(res$indets), 2)
  expect_equal(res$indets$n_duplicates, c(0L, 0L))
  expect_equal(res$indets$n_useful, c(0L, 0L))
  expect_setequal(res$indets$outcome, c(oc[["no_dups"]], oc[["unmatchable"]]))
  expect_true(all(is.na(res$indets$suggestedIdentification)))
  # Columns absent from the input are filled with NA
  expect_true(all(is.na(res$indets$identifiedBy)))
  expect_true(all(is.na(res$indets$source)))

  expect_equal(nrow(res$suggestions), 0)
  expect_named(
    res$suggestions,
    c("herbarium", "occurrenceID", "catalogNumber", "currentIdentification",
      "recordedBy", "recordNumber", "identifiedBy", "dateIdentified",
      "dup_herbarium", "dup_source", "dup_catalogNumber", "dup_identification",
      "dup_family", "dup_identifiedBy", "dup_dateIdentified",
      "fills_missing_rank", "compatible", "dup_occurrenceID", "specimenLink",
      "dup_specimenLink")
  )
  expect_type(res$suggestions$fills_missing_rank, "logical")
  expect_equal(nrow(.identify_source_summary(res$suggestions)), 0)
})


test_that(".save_identify_figures saves requested formats and skips NULL plots", {
  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  msgs <- testthat::capture_messages(
    .save_identify_figures(list(outcome = p, source = NULL, family = p),
                           tmp, c("pdf", "png"), 4, 3, verbose = TRUE)
  )

  expect_length(msgs, 4L)
  expect_match(msgs[1], "Saved: .*01_identify_outcome\\.pdf")
  expect_setequal(
    list.files(tmp),
    c("01_identify_outcome.pdf", "01_identify_outcome.png",
      "03_identify_families.pdf", "03_identify_families.png")
  )

  expect_silent(.save_identify_figures(list(outcome = p), tmp, "pdf", 4, 3,
                                       verbose = FALSE))
})
