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
