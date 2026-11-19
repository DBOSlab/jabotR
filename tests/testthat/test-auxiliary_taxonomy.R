# Tests for the taxonomic standardization helpers and data loaders in
# R/auxiliary_fxns.R: .std_inside_columns(), .fill_taxon_name(),
# .fill_scientific_name(), .flora_parse(), .load_ffb_data(),
# .load_herb_data() and .load_pool_data(). All offline.

# Build a records data frame with every column .std_inside_columns() touches;
# columns not given are NA. Vectors are recycled to the longest one.
.tx <- function(...) {
  given <- list(...)
  n <- max(c(1L, lengths(given)))
  cols <- c("taxonRank", "kingdom", "division", "class", "order", "family",
            "genus", "specificEpithet", "infraspecificEpithet",
            "scientificNameAuthorship", "identificationQualifier",
            "basisOfRecord", "species", "scientificName", "taxonName")
  df <- as.data.frame(
    stats::setNames(rep(list(rep(NA_character_, n)), length(cols)), cols),
    stringsAsFactors = FALSE
  )
  for (nm in names(given)) df[[nm]] <- rep_len(given[[nm]], n)
  df$occurrenceID <- as.character(seq_len(n))
  df
}

.std <- function(...) {
  .std_inside_columns(.tx(...), herbarium = "RB", i = 1, verbose = FALSE)
}


# .std_inside_columns(): taxonRank ---------------------------------------------

test_that(".std_inside_columns() reports progress when verbose", {
  expect_message(
    .std_inside_columns(.tx(taxonRank = "SPECIES", family = "Fabaceae",
                            genus = "Inga", specificEpithet = "edulis"),
                        herbarium = c("R", "RB"), i = 2, verbose = TRUE),
    "Standardizing taxonomic columns of 'RB'"
  )
})

test_that(".std_inside_columns() maps every spelling of taxonRank", {
  input <- c("f.", "var.", "ssp.", "sp.", "tribo", "sub_familia", "Ordem",
             "classe", "divisao", "reino")
  out <- .std(taxonRank = input, family = "Fabaceae")
  expect_equal(out$taxonRank,
               c("FORMA", "VARIETAS", "SUBSPECIES", "SPECIES", "TRIBE",
                 "SUBFAMILY", "ORDER", "CLASS", "DIVISION", "KINGDOM"))

  out <- .std(taxonRank = c("gen.", "Genus"), family = "Fabaceae",
              genus = "Inga")
  expect_equal(out$taxonRank, c("GENUS", "GENUS"))
})

test_that(".std_inside_columns() drops a rank with no name to rank", {
  out <- .std(taxonRank = c("SPECIES", "FAMILY"), family = c(NA, "Fabaceae"))
  expect_true(is.na(out$taxonRank[1]))
  expect_equal(out$taxonRank[2], "FAMILY")
})

test_that(".std_inside_columns() reclassifies names typed into taxonRank", {
  out <- .std(taxonRank = c("Fabaceae", "inga edulis", "Weird"),
              family = "Fabaceae",
              genus = c(NA, "Inga", "Inga"),
              specificEpithet = c(NA, "edulis", NA))
  expect_equal(out$taxonRank, c("FAMILY", "SPECIES", "GENUS"))
  expect_equal(out$taxonName, c("Fabaceae", "Inga edulis", "Inga"))
})

test_that(".std_inside_columns() gives a missing rank GENUS when a genus exists", {
  out <- .std(taxonRank = c(NA, NA, "SPECIES"), family = "Fabaceae",
              genus = c("Inga", NA, "Inga"),
              specificEpithet = c(NA, NA, "edulis"))
  expect_equal(out$taxonRank[1], "GENUS")
  expect_true(is.na(out$taxonRank[2]))
})


# .std_inside_columns(): invisible characters -----------------------------------

test_that(".std_inside_columns() trims Unicode spaces around names", {
  out <- .std(taxonRank = "SUBSPECIES", family = " Fabaceae\u00a0",
              genus = "\u2003Inga ", specificEpithet = " edulis",
              infraspecificEpithet = "vera  ")
  expect_equal(out$family, "Fabaceae")
  expect_equal(out$genus, "Inga")
  expect_equal(out$specificEpithet, "edulis")
  expect_equal(out$infraspecificEpithet, "vera")
  expect_equal(out$scientificName, "Inga edulis subsp. vera")
})


# .std_inside_columns(): family -------------------------------------------------

test_that(".std_inside_columns() repairs misspelled family names", {
  fam <- c("Fabaceae Subfam. Mimosoideae", "Leguminosae Subfam. Papil.",
           "Rubiacae", "Lindsaaceae", "Clusiceae", "Fabaceae?",
           "Myrtaceae.", "Cf. Lauraceae", "Mimosoideae", "Incertae Sedis")
  out <- .std(taxonRank = "FAMILY", family = fam)
  expect_equal(out$family,
               c("Fabaceae", "Leguminosae", "Rubiaceae", "Lindsaeaceae",
                 "Clusiaceae", "Fabaceae", "Myrtaceae", "Lauraceae",
                 "Fabaceae", "INSERTAE SEDIS"))
})

test_that(".std_inside_columns() blanks placeholders and negated families", {
  out <- .std(taxonRank = "FAMILY",
              family = c("Indet.", "Sem Informa\u00e7\u00e3o", "Not Fabaceae",
                         "N\u00e3o \u00e9 Myrtaceae", "Fabaceae"))
  expect_true(all(is.na(out$family[1:4])))
  expect_true(all(is.na(out$taxonRank[1:4])))
  expect_equal(out$family[5], "Fabaceae")
})

test_that(".std_inside_columns() moves orders, classes and divisions out of family", {
  out <- .std(taxonRank = c("FAMILY", "FAMILY", "FAMILY", "GENUS"),
              family = c("Fabales", "Chlorophyceae", "Bryophyta", "Fabales"),
              genus = c(NA, NA, NA, "Inga"))
  expect_true(all(is.na(out$family)))
  expect_equal(out$order, c("Fabales", NA, NA, "Fabales"))
  expect_equal(out$class, c(NA, "Chlorophyceae", NA, NA))
  expect_equal(out$division, c(NA, NA, "Bryophyta", NA))
  # the rank follows the name only when the record was a family
  expect_equal(out$taxonRank, c("ORDER", "CLASS", "DIVISION", "GENUS"))
  expect_equal(out$taxonName, c("Fabales", "Chlorophyceae", "Bryophyta",
                                "Inga"))
})


# .std_inside_columns(): family names typed into genus ---------------------------

test_that(".std_inside_columns() moves a family in genus to family for family records", {
  out <- .std(taxonRank = "FAMILY", genus = "Myrtaceae")
  expect_equal(out$family, "Myrtaceae")
  expect_true(is.na(out$genus))
  expect_equal(out$taxonName, "Myrtaceae")
})

test_that(".std_inside_columns() shifts names left when genus holds the family", {
  out <- .std(taxonRank = c("SPECIES", "SPECIES", "SPECIES"),
              genus = "Myrtaceae",
              specificEpithet = c("eugenia", "eugenia", NA),
              infraspecificEpithet = c("uniflora", NA, NA),
              scientificNameAuthorship = c(NA, "sp.", NA))
  expect_equal(out$family, rep("Myrtaceae", 3))
  expect_equal(out$genus, c("Eugenia", "Eugenia", NA))
  expect_equal(out$specificEpithet, c("uniflora", NA, NA))
  expect_equal(out$taxonRank, c("SPECIES", "GENUS", "FAMILY"))
  expect_true(all(is.na(out$scientificNameAuthorship)))
  expect_equal(out$taxonName, c("Eugenia uniflora", "Eugenia", "Myrtaceae"))
})

test_that(".std_inside_columns() drops genus and epithet of a family record with the family in genus", {
  out <- .std(taxonRank = "FAMILY", genus = "Myrtaceae",
              specificEpithet = "eugenia")
  expect_equal(out$family, "Myrtaceae")
  expect_true(is.na(out$genus))
  expect_true(is.na(out$specificEpithet))
})

test_that(".std_inside_columns() splits 'Family Genus epithet' typed into genus", {
  out <- .std(taxonRank = "SPECIES",
              genus = c("Myrtaceae Eugenia uniflora", "Myrtaceae Eugenia uniflora"),
              specificEpithet = c("x", NA))
  expect_equal(out$family, c("Myrtaceae", "Myrtaceae"))
  expect_equal(out$genus, c("Eugenia", "Eugenia"))
  # the epithet is kept only when the record had one
  expect_equal(out$specificEpithet, c("uniflora", NA))
  expect_equal(out$taxonRank, c("SPECIES", "GENUS"))
})

test_that(".std_inside_columns() rewrites an authored genus ending in -aceae", {
  # documents current behaviour: the -aceae ending becomes -a
  out <- .std(taxonRank = "SPECIES", family = "Myrtaceae",
              genus = "Myrtaceae", specificEpithet = "communis",
              scientificNameAuthorship = "Berg")
  expect_equal(out$genus, "Myrta")
  expect_equal(out$scientificName, "Myrta communis Berg")
})


# .std_inside_columns(): genus ---------------------------------------------------

test_that(".std_inside_columns() blanks indeterminate genera", {
  out <- .std(taxonRank = "SPECIES", family = c("Fabaceae", NA),
              genus = c("Indet.", "Indeterminada"),
              specificEpithet = "sp1", infraspecificEpithet = "x")
  expect_true(all(is.na(out$genus)))
  expect_true(all(is.na(out$specificEpithet)))
  expect_true(all(is.na(out$infraspecificEpithet)))
  expect_equal(out$taxonRank[1], "FAMILY")
  expect_equal(out$taxonName[1], "Fabaceae")
})

test_that(".std_inside_columns() handles old family names in genus", {
  out <- .std(taxonRank = "SPECIES", family = c("Fabaceae", NA, NA),
              genus = c("Compositae", "Leguminosae Papil.", "Leguminosae Mimos."),
              specificEpithet = c("x", "inga", "Leguminosae"),
              infraspecificEpithet = c(NA, "edulis", "mimosa"))
  expect_true(is.na(out$genus[1]))
  expect_true(is.na(out$specificEpithet[1]))
  # the names shift left
  expect_equal(out$family[2:3], c("Leguminosae", "Leguminosae"))
  expect_equal(out$genus[2:3], c("Inga", "Mimosa"))
  expect_equal(out$specificEpithet[2:3], c("edulis", NA))
  expect_equal(out$taxonRank[2:3], c("SPECIES", "GENUS"))
})

test_that(".std_inside_columns() moves non-vascular groups out of genus", {
  out <- .std(taxonRank = "GENUS",
              family = c(NA, NA, "Parmeliaceae"),
              genus = c("Bri\u00f3fita", "Fungos", "Lichenes"))
  expect_true(all(is.na(out$genus)))
  expect_equal(out$division, c("Bryophyta", NA, "Ascomycota"))
  expect_equal(out$kingdom, c(NA, "Fungi", "Fungi"))
  expect_equal(out$taxonRank, c("DIVISION", "KINGDOM", "DIVISION"))
  expect_equal(out$taxonName, c("Bryophyta", "Fungi", "Ascomycota"))
})

test_that(".std_inside_columns() parses names typed into the epithet columns", {
  out <- .std(taxonRank = "SPECIES",
              specificEpithet = c("Myrtaceae", "Myrtaceae Eugenia", NA),
              infraspecificEpithet = c("eugenia", "Myrtaceae", "myrtaceae"),
              scientificNameAuthorship = c("uniflora", NA, "Eugenia"))
  expect_equal(out$family, rep("Myrtaceae", 3))
  expect_equal(out$genus, rep("Eugenia", 3))
  expect_equal(out$specificEpithet, c("uniflora", NA, NA))
  expect_true(all(is.na(out$infraspecificEpithet)))
  expect_true(all(is.na(out$scientificNameAuthorship)))
  expect_equal(out$taxonRank, c("SPECIES", "GENUS", "GENUS"))
})

test_that(".std_inside_columns() drops a family typed as the authorship", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = "edulis", scientificNameAuthorship = "Fabaceae")
  expect_true(is.na(out$scientificNameAuthorship))
  expect_equal(out$scientificName, "Inga edulis")
})

test_that(".std_inside_columns() cleans 'sp.' and '?' in genus", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae",
              genus = c("sp.", "Inga sp.", "?Inga"),
              specificEpithet = c(NA, "edulis", NA))
  expect_equal(out$genus, c(NA, "Inga", NA))
  expect_true(all(is.na(out$specificEpithet)))
  expect_equal(out$taxonRank, c("FAMILY", "GENUS", "FAMILY"))
})


# .std_inside_columns(): specificEpithet -----------------------------------------

test_that(".std_inside_columns() drops 'sp.' placeholders from the epithet", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = c("sp.", "sp. 1", "Sp.", "Sp. Nov.",
                                  "edulis sp.", "subsp."))
  expect_equal(out$specificEpithet, c(NA, NA, NA, NA, "edulis", NA))
  expect_equal(out$taxonRank[1:4], rep("GENUS", 4))
  expect_equal(out$taxonName[5], "Inga edulis")
})

test_that(".std_inside_columns() moves a division in genus of division records", {
  out <- .std(taxonRank = "DIVISION", genus = "Bryophyta")
  expect_equal(out$division, "Bryophyta")
  expect_true(is.na(out$genus))
  expect_equal(out$taxonName, "Bryophyta")
})


# .std_inside_columns(): division, fungi and lichens ------------------------------

test_that(".std_inside_columns() standardizes division names", {
  out <- .std(taxonRank = c("DIVISION", "DIVISION", "GENUS", "FAMILY"),
              division = c("Briophyta", "Pterydophyta", NA, NA),
              family = c("Fabaceae", "Fabaceae", NA, "Samambaia"),
              genus = c(NA, NA, "Pteridophyta", NA))
  expect_equal(out$division, c("Bryophyta", "Pteridophyta", "Pteridophyta",
                               "Pteridophyta"))
  expect_true(is.na(out$genus[3]))
  expect_true(is.na(out$family[4]))
  expect_equal(out$taxonRank, rep("DIVISION", 4))
})

test_that(".std_inside_columns() recognizes fungi", {
  out <- .std(taxonRank = c("FAMILY", "GENUS", "FAMILY", "DIVISION"),
              family = c("Fungi", "Fungi", "Basidiomycetes", NA),
              division = c(NA, NA, NA, "Fungos"),
              genus = c(NA, "Amanita", NA, NA))
  expect_equal(out$kingdom, rep("Fungi", 4))
  expect_true(all(is.na(out$family)))
  expect_equal(out$division, c(NA, NA, "Basidiomycota", NA))
  expect_equal(out$genus, c(NA, "Amanita", NA, NA))
  expect_equal(out$taxonRank[c(1, 2, 4)], c("KINGDOM", "GENUS", "KINGDOM"))
  expect_equal(out$taxonName[c(1, 2, 4)], c("Fungi", "Amanita", "Fungi"))
})

test_that(".std_inside_columns() recognizes lichens", {
  out <- .std(taxonRank = c("DIVISION", "FAMILY", "GENUS"),
              division = c("Lichen", NA, NA),
              family = c("Parmeliaceae", "Liquen", "Lichenes"),
              genus = c(NA, NA, "Usnea"))
  expect_equal(out$kingdom, rep("Fungi", 3))
  expect_equal(out$division, rep("Ascomycota", 3))
  expect_true(all(is.na(out$family)))
  expect_equal(out$taxonRank, rep("DIVISION", 3))
  expect_equal(out$genus, c(NA, NA, "Usnea"))
})

test_that(".std_inside_columns() moves angiosperm labels to division", {
  out <- .std(taxonRank = "FAMILY", family = c("Angiospermae", "Dicotyledoneae"),
              genus = c("Inga", NA))
  expect_equal(out$division, rep("Angiospermae", 2))
  expect_true(all(is.na(out$family)))
  expect_true(all(is.na(out$genus)))
  expect_equal(out$taxonRank, rep("DIVISION", 2))
})


# .std_inside_columns(): identificationQualifier ---------------------------------

test_that(".std_inside_columns() moves cf. and aff. to identificationQualifier", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = c("cf. edulis", "edulis cf.", "sp. aff. edulis",
                                  "aff. edulis", "edulis aff", "edulis"))
  expect_equal(out$specificEpithet, rep("edulis", 6))
  expect_equal(out$identificationQualifier,
               c("cf.", "cf.", "aff.", "aff.", "aff.", NA))
  expect_equal(out$species, rep("Inga edulis", 6))
})

test_that(".std_inside_columns() strips 'ined.' and 'ssp. ?' from the epithet", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = c("novum ined.", "edulis ssp. ?"))
  expect_equal(out$specificEpithet, c("novum", "edulis"))
})


# .std_inside_columns(): infraspecific names -------------------------------------

test_that(".std_inside_columns() splits infraspecific names out of the epithet", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = c("edulis subsp. vera", "edulis var. vera",
                                  "edulis f. vera"))
  expect_equal(out$specificEpithet, rep("edulis", 3))
  expect_equal(out$infraspecificEpithet, rep("vera", 3))
  expect_equal(out$taxonRank, c("SUBSPECIES", "VARIETAS", "FORMA"))
  expect_equal(out$scientificName,
               c("Inga edulis subsp. vera", "Inga edulis var. vera",
                 "Inga edulis f. vera"))
})

test_that(".std_inside_columns() reads the rank inside infraspecificEpithet", {
  out <- .std(taxonRank = "SUBSPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = "edulis",
              infraspecificEpithet = c("var.", "var. vera", "subsp. vera",
                                       "f. vera"))
  expect_equal(out$infraspecificEpithet, c(NA, "vera", "vera", "vera"))
  expect_equal(out$taxonRank, c("SPECIES", "VARIETAS", "SUBSPECIES", "FORMA"))
})


# .std_inside_columns(): scientificNameAuthorship --------------------------------

test_that(".std_inside_columns() parses varieties written in the authorship", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = "edulis",
              infraspecificEpithet = c(NA, NA, NA, NA, "vera x", NA),
              scientificNameAuthorship = c("var. Vera Benth.", "var. vera",
                                           "Mart. var. vera Benth.",
                                           "Mart. var. vera", "Mart. var.",
                                           "Mart. var.vera"))
  expect_equal(out$taxonRank, rep("VARIETAS", 6))
  expect_equal(out$infraspecificEpithet, rep("vera", 6))
  expect_equal(out$scientificNameAuthorship,
               c("Benth.", NA, "Benth.", "Mart.", NA, "Mart."))
  expect_equal(out$scientificName[1], "Inga edulis var. vera Benth.")
})

test_that(".std_inside_columns() parses subspecies written in the authorship", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = "edulis",
              scientificNameAuthorship = c("Mart. subsp. Vera Benth.",
                                           "Mart. subsp. vera",
                                           "Mart. subsp.vera"))
  expect_equal(out$taxonRank, rep("SUBSPECIES", 3))
  expect_equal(out$infraspecificEpithet, rep("vera", 3))
  expect_equal(out$scientificNameAuthorship, c("Benth.", "Mart.", "Mart."))
})

test_that(".std_inside_columns() handles 'sp.' and full names in the authorship", {
  out <- .std(taxonRank = c("SPECIES", "SPECIES", "SPECIES"),
              family = "Fabaceae", genus = "Inga",
              specificEpithet = c(NA, "edulis", "edulis"),
              scientificNameAuthorship = c("sp.", "sp.", "Inga edulis Mart."))
  expect_equal(out$taxonRank, c("GENUS", "SPECIES", "SPECIES"))
  expect_equal(out$scientificNameAuthorship, c(NA, NA, "Mart."))
  expect_equal(out$scientificName, c("Inga", "Inga edulis", "Inga edulis Mart."))
})


# .std_inside_columns(): basisOfRecord and final names ----------------------------

test_that(".std_inside_columns() standardizes basisOfRecord", {
  out <- .std(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
              specificEpithet = "edulis",
              basisOfRecord = c("PreservedSpecimen", "FossilSpecimen",
                                "HumanObservation"))
  expect_equal(out$basisOfRecord,
               c("PRESERVED_SPECIMEN", "FOSSIL_SPECIMEN", "HumanObservation"))
})

test_that(".std_inside_columns() leaves a clean record untouched", {
  df <- .tx(taxonRank = "SPECIES", family = "Fabaceae", genus = "Inga",
            specificEpithet = "edulis", scientificNameAuthorship = "Mart.",
            basisOfRecord = "PRESERVED_SPECIMEN")
  out <- .std_inside_columns(df, herbarium = "RB", i = 1, verbose = FALSE)
  keep <- c("taxonRank", "family", "genus", "specificEpithet",
            "scientificNameAuthorship", "basisOfRecord", "occurrenceID")
  expect_equal(out[keep], df[keep])
  expect_equal(out$species, "Inga edulis")
  expect_equal(out$taxonName, "Inga edulis")
  expect_equal(out$scientificName, "Inga edulis Mart.")
})


# .fill_taxon_name() ------------------------------------------------------------

test_that(".fill_taxon_name() uses the name of each higher rank", {
  df <- .tx(taxonRank = c("KINGDOM", "DIVISION", "CLASS", "ORDER", "FAMILY",
                          "GENUS", NA),
            kingdom = "Plantae", division = "Tracheophyta",
            class = "Magnoliopsida", order = "Fabales", family = "Fabaceae",
            genus = c(rep("Inga", 6), NA))
  out <- .fill_taxon_name(df)
  expect_equal(out$taxonName,
               c("Plantae", "Tracheophyta", "Magnoliopsida", "Fabales",
                 "Fabaceae", "Inga", ""))
})

test_that(".fill_taxon_name() works without the higher-rank columns", {
  df <- data.frame(taxonRank = c("KINGDOM", "SPECIES"),
                   genus = c(NA, "Inga"), specificEpithet = c(NA, "edulis"),
                   infraspecificEpithet = NA_character_,
                   stringsAsFactors = FALSE)
  out <- .fill_taxon_name(df)
  expect_equal(out$taxonName, c(NA, "Inga edulis"))

  # no record below family
  df <- data.frame(taxonRank = "FAMILY", family = NA_character_,
                   stringsAsFactors = FALSE)
  expect_true(is.na(.fill_taxon_name(df)$taxonName))
})


# .fill_scientific_name() -------------------------------------------------------

test_that(".fill_scientific_name() places the authorship of autonyms", {
  df <- data.frame(
    taxonRank = c("VARIETAS", "VARIETAS", "FORMA", NA),
    genus = "Inga",
    specificEpithet = "edulis",
    infraspecificEpithet = c("edulis", "edulis", "alba", "alba"),
    scientificNameAuthorship = c("Mart.", NA, "", "Mart."),
    stringsAsFactors = FALSE
  )
  out <- .fill_scientific_name(df)
  expect_equal(out$scientificName,
               c("Inga edulis Mart. var. edulis", "Inga edulis",
                 "Inga edulis f. alba", "Inga edulis Mart."))
})


# .load_herb_data() and .load_pool_data() -----------------------------------------

test_that(".load_herb_data() and .load_pool_data() call jabot_records()", {
  calls <- list()
  local_mocked_bindings(
    jabot_records = function(...) {
      calls[[length(calls) + 1]] <<- list(...)
      data.frame(id = 1)
    },
    .package = "jabotR"
  )
  expect_equal(.load_herb_data("RB", NULL, FALSE), data.frame(id = 1))
  .load_pool_data(c("RB", "R"), "some/path", TRUE)

  expect_equal(calls[[1]],
               list(herbarium = "RB", indets = FALSE, path = NULL,
                    updates = TRUE, verbose = FALSE, save = FALSE))
  expect_equal(calls[[2]],
               list(herbarium = c("RB", "R"), indets = FALSE,
                    path = "some/path", updates = FALSE, verbose = TRUE,
                    save = FALSE))
})


# .flora_parse() and .load_ffb_data() ---------------------------------------------

# A fake DwC-A as finch::dwca_read() returns it
.fake_dwca <- function() {
  list(data = list(
    taxon.txt = data.frame(
      id = c("1", "2", "3"),
      family = c("Fabaceae", "NA", "Fabaceae"),
      genus = c("Inga", "Inga", "NA"),
      specificEpithet = c("edulis", NA, NA),
      infraspecificEpithet = c("vera", NA, NA),
      taxonRank = c("SUBSPECIES", "GENUS", "FAMILY"),
      kingdom = "Plantae",
      class = c("Magnoliopsida", "Polypodiopsida", NA),
      stringsAsFactors = FALSE
    ),
    distribution.txt = data.frame(
      id = c("1", "2"),
      occurrenceRemarks = c(
        '{"endemism":"Endemica","phytogeographicDomain":["Amaz\u00f4nia","Cerrado"]}',
        '{"endemism":"N\u00e3o endemica","phytogeographicDomain":["Pampa"]}'
      ),
      stringsAsFactors = FALSE
    ),
    speciesprofile.txt = data.frame(
      id = c("1", "2"),
      lifeForm = c(
        '{"lifeForm":["\u00c1rvore","Arbusto"],"habitat":["Terr\u00edcola"],"vegetationType":["Cerrado"]}',
        NA
      ),
      stringsAsFactors = FALSE
    )
  ))
}

test_that(".flora_parse() reads only the latest version and reshapes it", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "dwca_ffb_v393_400"))
  dir.create(file.path(path, "dwca_ffb_v393_401_latest"))
  read_inputs <- character()
  local_mocked_bindings(
    dwca_read = function(input, ...) {
      read_inputs <<- c(read_inputs, input)
      .fake_dwca()
    },
    .package = "finch"
  )

  msgs <- capture_messages(out <- .flora_parse(path = path, verbose = TRUE))
  expect_match(msgs[1], "Parsing data from dwca folders")
  expect_match(msgs[2], "dwca_ffb_v393_401_latest")
  expect_equal(names(out), "dwca_ffb_v393_401_latest")
  expect_equal(read_inputs, paste0(path, "/dwca_ffb_v393_401_latest"))

  taxon <- out[[1]]$data$taxon.txt
  expect_equal(taxon$taxonName, c("Inga edulis vera", "Inga", NA))
  expect_equal(which(names(taxon) == "taxonName"),
               which(names(taxon) == "taxonRank") - 1)
  expect_true(is.na(taxon$family[2]))
  expect_true(is.na(taxon$genus[3]))

  distribution <- out[[1]]$data$distribution.txt
  expect_equal(distribution$id, c("1", "1", "2"))
  expect_equal(distribution$phytogeographicDomain,
               c("Amaz\u00f4nia", "Cerrado", "Pampa"))
  expect_equal(distribution$endemism,
               c("Endemica", "Endemica", "N\u00e3o endemica"))

  profile <- out[[1]]$data$speciesprofile.txt
  expect_equal(names(profile),
               c("id", "lifeForm", "habitat", "vegetationType", "lifeForm_json"))
  expect_equal(profile$id, c("1", "1"))
  expect_equal(profile$lifeForm, c("\u00c1rvore", "Arbusto"))
  expect_equal(profile$habitat, c("Terr\u00edcola", "Terr\u00edcola"))
  expect_equal(profile$vegetationType, c("Cerrado", "Cerrado"))
})

test_that(".flora_parse() is quiet when not verbose", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "dwca_ffb_v393_401_latest"))
  local_mocked_bindings(dwca_read = function(input, ...) .fake_dwca(),
                        .package = "finch")
  expect_silent(.flora_parse(path = path, verbose = FALSE))
})

test_that(".load_ffb_data() downloads, parses and groups FFB taxa", {
  downloaded <- NULL
  local_mocked_bindings(
    flora_download = function(version, verbose, dir) {
      downloaded <<- list(version = version, verbose = verbose, dir = dir)
      invisible(NULL)
    },
    .package = "floraR"
  )
  taxon <- data.frame(
    id = as.character(1:10),
    kingdom = "Plantae",
    class = c("Magnoliopsida", "Pinopsida", "Polypodiopsida",
              "Lycopodiopsida", "Bryopsida", "Incertae sedis",
              "Chlorophyceae", "Agaricomycetes", NA, "Other"),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    .flora_parse = function(path, version, verbose) {
      expect_equal(path, "flora_download")
      list(v1 = list(data = list(taxon.txt = taxon,
                                 distribution.txt = data.frame(id = "d"),
                                 speciesprofile.txt = data.frame(id = "s"))),
           v0 = list())
    },
    .package = "jabotR"
  )

  out <- .load_ffb_data(verbose = FALSE)
  expect_equal(downloaded,
               list(version = "latest", verbose = FALSE, dir = "flora_download"))
  expect_named(out, c("taxon", "distribution", "speciesprofile"))
  expect_equal(out$taxon$group,
               c("angiosperms", "gymnosperms", "ferns", "lycophytes",
                 "bryophytes", "incertae sedis", "algae", "fungi",
                 "incertae sedis", NA))
  expect_equal(names(out$taxon)[1:3], c("id", "group", "kingdom"))
  expect_equal(out$distribution, data.frame(id = "d"))
  expect_equal(out$speciesprofile, data.frame(id = "s"))
})
