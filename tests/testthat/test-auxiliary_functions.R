test_that(".save_csv() writes a file correctly", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  tmp_dir <- tempdir()
  filename <- "test_file"

  .save_csv(df, verbose = FALSE, filename = filename, dir = tmp_dir)

  output_path <- file.path(tmp_dir, paste0(filename, ".csv"))
  expect_true(file.exists(output_path))

  written_df <- read.csv(output_path)
  expect_equal(ncol(written_df), 2)
  expect_equal(nrow(written_df), 3)

  unlink(output_path)
})


test_that(".replace_cols() replaces target column with group", {
  result <- .replace_cols(c("herbarium", "collector"), replace = c("collectionCode"), columns_to_replace = "herbarium")
  expect_equal(result[1], "collectionCode")

  result2 <- .replace_cols(c("a", "taxa", "b"), replace = c("family", "genus"), columns_to_replace = "taxa")
  expect_equal(result2, c("a", "family", "genus", "b"))
})


test_that(".check_taxon_match() validates correctly", {
  df <- data.frame(family = "Fabaceae", genus = "Inga", taxonName = "Inga edulis")
  expect_silent(.check_taxon_match(df, taxon = "Fabaceae", verbose = FALSE))
  expect_error(.check_taxon_match(df, taxon = "Invalidus", verbose = FALSE))
})


test_that(".check_state_match() validates state presence", {
  df <- data.frame(stateProvince = c("BA", "MG"))
  expect_silent(.check_state_match(df, state = "BA", verbose = FALSE))
  expect_error(.check_state_match(df, state = "XX", verbose = FALSE))
})


test_that(".check_year_match() validates year presence", {
  df <- data.frame(year = c(2000, 2010))
  expect_silent(.check_year_match(df, recordYear = 2000, verbose = FALSE))
  expect_error(.check_year_match(df, recordYear = 1990, verbose = FALSE))
})


test_that(".filter_occur_df() handles taxon + year filtering", {
  df <- data.frame(
    family = c("Fabaceae", "Malvaceae"),
    genus = c("Inga", "Hibiscus"),
    taxonName = c("Inga edulis", "Hibiscus rosa-sinensis"),
    stateProvince = c("Bahia", "Minas Gerais"),
    year = c(2005, 2020)
  )

  result <- .filter_occur_df(df,
                             taxon = "Fabaceae",
                             state = "Bahia",
                             recordYear = c(2005, 2010),
                             verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$family, "Fabaceae")
})


test_that(".save_csv() writes a file correctly", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  tmp_dir <- tempdir()
  filename <- "test_file"

  .save_csv(df, verbose = FALSE, filename = filename, dir = tmp_dir)

  output_path <- file.path(tmp_dir, paste0(filename, ".csv"))
  expect_true(file.exists(output_path))

  written_df <- read.csv(output_path)
  expect_equal(ncol(written_df), 2)
  expect_equal(nrow(written_df), 3)

  unlink(output_path)
})


test_that(".save_log() creates a log with summary statistics", {
  df <- data.frame(
    collectionCode = c("RB", "RB", "K"),
    family = c("Fabaceae", "Fabaceae", "Asteraceae"),
    genus = c("Inga", "Inga", "Baccharis"),
    country = c("Brazil", "Brazil", "Brazil"),
    stateProvince = c("BA", "BA", "SP")
  )

  tmp_dir <- tempdir()
  filename <- "test_log_file"

  log_path <- file.path(tmp_dir, "log.txt")
  if (file.exists(log_path)) unlink(log_path)

  .save_log(df = df, herbarium = c("RB", "K"), filename = filename, dir = tmp_dir)
  expect_true(file.exists(log_path))

  log_contents <- readLines(log_path)
  expect_true(any(grepl("Total records: 3", log_contents)))
  expect_true(any(grepl("Records per herbarium:", log_contents)))
  expect_true(any(grepl("Records per family:", log_contents)))
  expect_true(any(grepl("Records per genus:", log_contents)))
  expect_true(any(grepl("Records per country:", log_contents)))
  expect_true(any(grepl("Records per stateProvince:", log_contents)))

  unlink(log_path)
})


test_that(".replace_cols() replaces target column with group", {
  result <- .replace_cols(c("herbarium", "collector"), replace = c("collectionCode"), columns_to_replace = "herbarium")
  expect_equal(result[1], "collectionCode")

  result2 <- .replace_cols(c("a", "taxa", "b"), replace = c("family", "genus"), columns_to_replace = "taxa")
  expect_equal(result2, c("a", "family", "genus", "b"))
})


test_that(".check_taxon_match() validates correctly", {
  df <- data.frame(family = "Fabaceae", genus = "Inga", taxonName = "Inga edulis")
  expect_silent(.check_taxon_match(df, taxon = "Fabaceae", verbose = FALSE))
  expect_error(.check_taxon_match(df, taxon = "Invalidus", verbose = FALSE))
})


test_that(".check_state_match() validates state presence", {
  df <- data.frame(stateProvince = c("BA", "MG"))
  expect_silent(.check_state_match(df, state = "BA", verbose = FALSE))
  expect_error(.check_state_match(df, state = "XX", verbose = FALSE))
})


test_that(".check_year_match() validates year presence", {
  df <- data.frame(year = c(2000, 2010))
  expect_silent(.check_year_match(df, recordYear = 2000, verbose = FALSE))
  expect_error(.check_year_match(df, recordYear = 1990, verbose = FALSE))
})


test_that(".filter_occur_df() handles taxon + year filtering", {
  df <- data.frame(
    family = c("Fabaceae", "Malvaceae"),
    genus = c("Inga", "Hibiscus"),
    taxonName = c("Inga edulis", "Hibiscus rosa-sinensis"),
    stateProvince = c("Bahia", "Minas Gerais"),
    year = c(2005, 2020)
  )

  result <- .filter_occur_df(df,
                             taxon = "Fabaceae",
                             state = "Bahia",
                             recordYear = c(2005, 2010),
                             verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$family, "Fabaceae")
})


test_that(".reorder_df() uses default when reorder is NULL", {
  df <- data.frame(
    collectionCode = c("HUEFS", "RB"),
    family = c("Fabaceae", "Malvaceae"),
    genus = c("Inga", "Hibiscus"),
    specificEpithet = c("edulis", "Hibiscus rosa-sinensis"),
    recordedBy = c("R. C. Forzza", "D. Cardoso"),
    recordNumber = c("3450", "2025"),
    country = c("Brazil", "Brazil"),
    stateProvince = c("Bahia", "Bahia"),
    municipality = c("Prado", "Tucano"),
    year = c(2000, 1990)
  )
  expect_silent(.reorder_df(df, reorder = NULL))
})


test_that(".filter_occur_df() filters by species", {
  df <- data.frame(family = c("Fabaceae", "Fabaceae"),
                   genus = c("Mimosa", "Acacia"),
                   specificEpithet = c("pudica", "dealbata"),
                   taxonName = c("Mimosa pudica", "Acacia dealbata"),
                   stringsAsFactors = FALSE)
  result <- .filter_occur_df(df,
                             taxon = "Mimosa pudica",
                             state = NULL,
                             recordYear = NULL,
                             verbose = FALSE)
  expect_equal(nrow(result), 1)
})


test_that(".filter_occur_df() filters by single recordYear", {
  df <- data.frame(year = c("1990", "2000"), stringsAsFactors = FALSE)
  result <- .filter_occur_df(df,
                             taxon = NULL,
                             state = NULL,
                             recordYear = "1990",
                             verbose = FALSE)
  expect_equal(unique(result$year), "1990")
})


test_that(".filter_occur_df() filters by year range", {
  df <- data.frame(year = 1980:1995, stringsAsFactors = FALSE)
  result <- .filter_occur_df(df,
                             taxon = NULL,
                             state = NULL,
                             recordYear = c("1985", "1990"),
                             verbose = FALSE)
  expect_true(all(result$year >= 1985 & result$year <= 1990))
})


test_that(".check_taxon_match() handles valid and invalid taxa", {
  df <- data.frame(
    family = c("Fabaceae", "Rosaceae"),
    genus = c("Luetzelburgia", "Rosa"),
    taxonName = c("Luetzelburgia auriculata", "Rosa canina"),
    stringsAsFactors = FALSE
  )
  expect_silent(
    .check_taxon_match(df, c("Fabaceae", "Luetzelburgia"), verbose = FALSE)
  )
  expect_error(
    .check_taxon_match(df, c("Fakeplantus"), verbose = FALSE),
    "must contain at least one name"
  )
  expect_message(
    .check_taxon_match(df, c("Fabaceae", "Unknownus"), verbose = TRUE),
    "not found"
  )
})


test_that(".check_year_match() handles valid and invalid years", {
  df <- data.frame(
    year = c(1999, 2005, 2020),
    stringsAsFactors = FALSE
  )
  expect_silent(
    .check_year_match(df, c("2005", "2020"), verbose = FALSE)
  )
  expect_error(
    .check_year_match(df, c("1800", "1801"), verbose = FALSE),
    "must contain at least one year"
  )
  expect_message(
    .check_year_match(df, c("2005", "3000"), verbose = TRUE),
    "not found"
  )
})


test_that(".check_state_match() handles valid and invalid states", {
  df <- data.frame(
    stateProvince = c("Bahia", "Minas Gerais", "São Paulo"),
    stringsAsFactors = FALSE
  )
  expect_silent(
    .check_state_match(df, c("Bahia", "Minas Gerais"), verbose = FALSE)
  )
  expect_error(
    .check_state_match(df, c("ZZ", "XX"), verbose = FALSE),
    "must contain at least one name"
  )
  expect_message(
    .check_state_match(df, c("Minas Gerais", "ZZ"), verbose = TRUE),
    "not found"
  )
})


test_that(".std_inside_columns() handles unexpected taxonRank values", {
  df <- data.frame(
    taxonRank = c("Especie", "GENERO", "familia"),
    family = c(NA, NA, NA),
    genus = c(NA, NA, NA),
    specificEpithet = c(NA, NA, NA),
    infraspecificEpithet = c(NA, NA, NA),
    scientificNameAuthorship = c(NA, NA, NA),
    scientificName = c(NA, NA, NA),
    taxonName = c(NA, NA, NA),
    occurrenceID = c(1, 2, 3)
  )
  df_clean <- .std_inside_columns(df, verbose = FALSE)
  expect_equal(df_clean$taxonRank[1], "SPECIES")
  expect_equal(df_clean$taxonRank[2], "GENUS")
  expect_equal(df_clean$taxonRank[3], "FAMILY")
})


test_that(".std_inside_columns() cleans family column for trailing names and misspellings", {
  df <- data.frame(
    family = c("Amaranthaceae Alternanthera Maritima Mart", "Fagaceaeae", "Olacaeae"),
    taxonRank = c("FAMILY", "FAMILY", "FAMILY"),
    genus = c(NA, NA, NA),
    specificEpithet = c(NA, NA, NA),
    infraspecificEpithet = c(NA, NA, NA),
    scientificNameAuthorship = c(NA, NA, NA),
    scientificName = c(NA, NA, NA),
    taxonName = c(NA, NA, NA),
    occurrenceID = c(1, 2, 3)
  )
  df_clean <- .std_inside_columns(df, verbose = FALSE)
  expect_equal(df_clean$family[1], "Amaranthaceae")
  expect_equal(df_clean$family[2], "Fagaceae")
  expect_equal(df_clean$family[3], "Olacaceae")
})


test_that(".std_inside_columns() handles genus in family when genus ends with aceae", {
  df <- data.frame(
    family = c("Fabaceae", "Fabaceae"),
    genus = c("Fabaceae", "Fabaceae Subfam. Mimosoideae"),
    specificEpithet = c("maritima", NA),
    infraspecificEpithet = c(NA, NA),
    scientificNameAuthorship = c("Mart.", NA),
    taxonRank = c("GENUS", "GENUS"),
    scientificName = c(NA, NA),
    taxonName = c(NA, NA),
    occurrenceID = c(10, 11)
  )
  df_clean <- .std_inside_columns(df, verbose = FALSE)
  expect_equal(df_clean$family[1], "Fabaceae")
  expect_equal(df_clean$taxonRank[1], "FAMILY")
  expect_true(is.na(df_clean$infraspecificEpithet[1]))
  expect_equal(df_clean$taxonRank[2], "GENUS")
})


test_that(".firstUp() and .firstLower() capitalization functions", {
  expect_equal(.firstUp("test"), "Test")
  expect_equal(.firstLower("Test"), "test")
})


test_that(".check_state_match() filters by state correctly and gives verbose messages", {
  df <- data.frame(stateProvince = c("Bahia", "Sao Paulo", "Rio"))
  expect_error(
    .check_state_match(df, c("Amazonas"), verbose = TRUE),
    "must contain at least one name"
  )
})

test_that(".check_year_match() behaves with single year and range", {
  df <- data.frame(year = c("1999", "2000", "2001"))
  expect_error(
    .check_year_match(df, c("1980"), verbose = TRUE),
    "must contain at least one year"
  )
})


test_that(".fill_taxon_name() and .fill_scientific_name() are working", {
  df <- data.frame(
    taxonRank = c("FAMILY", "SPECIES", "SUBSPECIES"),
    family = c("Fabaceae", NA, NA),
    genus = c(NA, "Solanum", "Solanum"),
    specificEpithet = c(NA, "lycopersicum", "lycopersicum"),
    infraspecificEpithet = c(NA, NA, "esculentum"),
    scientificNameAuthorship = c(NA, NA, "Mill."),
    taxonName = NA_character_,
    scientificName = NA_character_,
    stringsAsFactors = FALSE
  )
  df <- .fill_taxon_name(df)
  df <- .fill_scientific_name(df)

  expect_equal(df$taxonName[1], "Fabaceae")
  expect_equal(df$taxonName[2], "Solanum lycopersicum")
  expect_equal(df$taxonName[3], "Solanum lycopersicum esculentum")

  expect_equal(df$scientificName[1], NA_character_)
  expect_equal(df$scientificName[2], "Solanum lycopersicum")
  expect_equal(df$scientificName[3], "Solanum lycopersicum subsp. esculentum Mill.")
})


test_that(".std_inside_columns() corrects duffixes like aceaea, aceaae, aceea", {
  df <- data.frame(
    family = c("Rubiaceaea", "Leguminosae Papilio", "Fabaceaae"),
    taxonRank = c("FAMILY", "FAMILY", "FAMILY"),
    genus = NA, specificEpithet = NA, infraspecificEpithet = NA,
    scientificNameAuthorship = NA, scientificName = NA,
    taxonName = NA, occurrenceID = 1:3
  )
  df_clean <- .std_inside_columns(df, verbose = FALSE)
  expect_equal(df_clean$family[1], "Rubiaceae")
  expect_equal(df_clean$family[2], "Fabaceae")
  expect_equal(df_clean$family[3], "Fabaceae")
})


test_that(".std_inside_columns() downgrads taxon rank SUBFAMILY to GENUS when genus is present", {
  df <- data.frame(
    taxonRank = c("SUBFAMILY", "SUBFAMILY"),
    family = c("Asteraceae", "Fabaceae"),
    genus = c("Helianthus", NA),
    specificEpithet = NA, infraspecificEpithet = NA,
    scientificNameAuthorship = NA, scientificName = NA,
    taxonName = NA, occurrenceID = 1:2
  )
  df_clean <- .std_inside_columns(df, verbose = FALSE)
  expect_equal(df_clean$taxonRank[1], "GENUS")
  expect_equal(df_clean$taxonRank[2], "SUBFAMILY")
})

