test_that(".norm_key removes accents and case", {
  expect_equal(.norm_key(c(" São Paulo ", "", NA, "BRASIL")),
               c("sao paulo", NA, NA, "brasil"))
})


test_that(".std_country gives English names, removes non-countries and fills from the code", {
  # after the cleaning of barRoso, which translates most of the names
  country <- c("Brasil", "Brazil", "Brasi", "Barsil", "Bolivia", "France",
               "United States", "Argentina", "Ceilão", "Capital", "Indisponível",
               "?", "Paris", "Florida", "Yugoslavia", "Zzzz", NA, NA, "")
  code <- c(rep(NA, 16), "AR", NA, NA)
  out <- .std_country(country, code)

  expect_equal(out[1:5], c("Brazil", "Brazil", "Brazil", "Brazil", "Bolivia"))
  expect_equal(out[6:9], c("France", "United States", "Argentina", "Sri Lanka"))
  # not a country, or impossible to tell
  expect_true(all(is.na(out[c(10, 11, 12, 16, 18, 19)])))
  expect_equal(out[13:15], c("France", "United States", "Yugoslavia"))
  # the country is taken from the code when missing
  expect_equal(out[17], "Argentina")
})


test_that(".country_code returns the ISO code and none for historical names", {
  expect_equal(.country_code(c("Brazil", "Argentina", "Yugoslavia", NA)),
               c("BR", "AR", NA, NA))
})


test_that(".impute_country_from_state uses the country of the other records", {
  country <- c(rep("Argentina", 6), rep("Paraguay", 3), NA, NA, NA, NA)
  state <- c(rep("Jujuy", 6), rep("Alto Paraguay", 3), "Jujuy", "Alto paraguay",
             "Unknown state", NA)
  out <- .impute_country_from_state(country, state)
  expect_equal(out[10], "Argentina")
  expect_equal(out[11], "Paraguay")
  expect_true(is.na(out[12]))
  expect_true(is.na(out[13]))

  # a state shared by two countries is not used
  country2 <- c("Brazil", "Brazil", "Venezuela", "Venezuela", NA)
  expect_true(is.na(.impute_country_from_state(country2, rep("Amazonas", 5))[5]))
})


test_that(".std_state_brazil fixes typos and forms of the Brazilian states", {
  df <- data.frame(
    country = rep("Brazil", 12),
    stateProvince = c("Pernanbuco", "Mato Groso do Sul", "Estado do Rio de Janeiro",
                      "Minas Gerais, MG", "Brasilia", "Guanabara", "Minas",
                      "Bahia", "Brasil", "Curitiba", "Parnaiba", "Argentina"),
    municipality = c(rep(NA, 7), "Salvador", NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  # Known municipalities, with their state, in the other records
  known <- data.frame(
    country = rep("Brazil", 8),
    stateProvince = rep(c("Paraná", "Piauí"), each = 4),
    municipality = rep(c("Curitiba", "Parnaíba"), each = 4),
    stringsAsFactors = FALSE
  )
  out <- .std_state_brazil(rbind(df, known))[1:12, ]

  expect_equal(
    out$stateProvince[1:8],
    c("Pernambuco", "Mato Grosso do Sul", "Rio de Janeiro", "Minas Gerais",
      "Distrito Federal", "Rio de Janeiro", "Minas Gerais", "Bahia")
  )
  expect_true(is.na(out$stateProvince[9]))
  # A municipality in the state column: state from its other records, and the
  # municipality is kept
  expect_equal(out$stateProvince[10], "Paraná")
  expect_equal(out$municipality[10], "Curitiba")
  # Not a typo of a state, because it is a known municipality
  expect_equal(out$stateProvince[11], "Piauí")
  # A country in the state column is not kept
  expect_true(is.na(out$stateProvince[12]))
  expect_true(is.na(out$municipality[12]))
})


test_that(".std_geography standardizes country, code, state and municipality", {
  df <- data.frame(
    country = c("Brasil", "Brazil", "Bolívia", NA, "Indeterminado"),
    countryCode = NA_character_,
    stateProvince = c("São Paulo", "SP", "La Paz", "Bahia", NA),
    municipality = c("Campinas", "Município de Santos", "La Paz", "Salvador", NA),
    locality = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- .std_geography(df)

  expect_equal(out$country, c("Brazil", "Brazil", "Bolivia", "Brazil", NA))
  expect_equal(out$countryCode, c("BR", "BR", "BO", "BR", NA))
  expect_equal(out$stateProvince[1:2], rep("São Paulo", 2))
  expect_equal(out$municipality[2], "Santos")

  # the columns are created when the file does not have them
  bare <- .std_geography(data.frame(family = "Fabaceae"))
  expect_true(all(c("country", "countryCode", "stateProvince") %in% names(bare)))
})


test_that(".std_typeStatus gives English terms and drops what is not a type", {
  x <- c("HOLOTYPE", "Holotipo", "Holotypus", "holoypus", "Isotipo", "ISOTYPUS",
         "Paratipo", "PARATYPUS", "Isoparatipo", "Isolectotype", "Sintipo",
         "Lectotipo", "Neotipo", "Epitipo", "Topotipo", "Tipo", "Typus",
         "Homéotipo fem", "Possivel Tipo", "possivel isotipo", "cf.", "aff.",
         "?", "Chapéu de couro", "pau-jacaré", "Emprestada - BHCB guia 28/2014",
         "nom. nov.", NA, "")
  out <- .std_typeStatus(x)

  expect_equal(
    out[1:19],
    c("Holotype", "Holotype", "Holotype", "Holotype", "Isotype", "Isotype",
      "Paratype", "Paratype", "Isoparatype", "Isolectotype", "Syntype",
      "Lectotype", "Neotype", "Epitype", "Topotype", "Type", "Type",
      "Homeotype", NA)
  )
  expect_true(all(is.na(out[20:29])))
})


test_that(".std_elevation removes placeholders and impossible values", {
  out <- .std_elevation(c("-9999", "-999", "1500", "1,5", "763.9", "20009",
                          "abc", "-3", NA, "8848"))
  expect_equal(out, c(NA, NA, "1500", "1.5", "763.9", NA, NA, "-3", NA, "8848"))
})


test_that(".std_date_part keeps only valid values", {
  expect_equal(.std_date_part(c("0", "5", "13", "x", NA, "12"), 1, 12),
               c(NA, "5", NA, NA, NA, "12"))
})


test_that(".std_eventDate writes ISO dates", {
  expect_equal(
    .std_eventDate(c("2014-9-4", "2014-09-04", "2014-9", "2014", "9-9-9",
                     "99-99-99", NA, " 2019-6-9 ")),
    c("2014-09-04", "2014-09-04", "2014-09", "2014", NA, NA, NA, "2019-06-09")
  )
})


test_that(".std_dateIdentified converts the Brazilian formats to ISO", {
  yy <- as.integer(format(Sys.Date(), "%y"))
  future <- sprintf("%02d", min(yy + 1, 99))
  out <- .std_dateIdentified(
    c("28/4/2020", "05/12/1999", "9/2001", "2005", "2005-03", "2005-03-07",
      "31/2/2010", "3/4/20", paste0("3/4/", future), "13/2001", NA, "abc")
  )
  expect_equal(out[1:6], c("2020-04-28", "1999-12-05", "2001-09", "2005",
                           "2005-03", "2005-03-07"))
  expect_true(is.na(out[7]))
  expect_equal(out[8], "2020-04-03")
  expect_equal(out[9], paste0("19", future, "-04-03"))
  expect_true(all(is.na(out[10:12])))
})


test_that(".std_recordNumber unifies the records without a collector number", {
  x <- c("s.n.", "s/n", "S/N", "sn", "s.n", "s/nº", "s/ns.n.", "sem nº",
         "sem número", "s.n,", "?", "", "123", "12A", "s.n. (INPA37700)", NA)
  out <- .std_recordNumber(x)
  expect_equal(out[1:10], rep("s.n.", 10))
  expect_true(all(is.na(out[c(11, 12, 16)])))
  expect_equal(out[13:15], c("123", "12A", "s.n. (INPA37700)"))
})


test_that(".std_dates_and_numbers cleans dates, elevation and coordinates", {
  df <- data.frame(
    eventDate = c("2014-9-4", "9-9-9", NA, "2005-03", "2010-05-12"),
    year = c("2014", "2000", "0", "0", "2010"),
    month = c("9", "0", "0", "0", "13"),
    day = c("4", "0", "0", "0", "12"),
    minimumElevationInMeters = c("100", "-9999", NA, "1,5", "30"),
    recordNumber = c("1", "S/N", "s.n", "?", "7"),
    dateIdentified = c("28/4/2020", NA, NA, "2005", "x"),
    decimalLatitude = c(-12.5, 0, -95, 10, NA),
    decimalLongitude = c(-38.5, 0, -40, 20, NA),
    stringsAsFactors = FALSE
  )
  out <- .std_dates_and_numbers(df)

  # event date in ISO format; year, month and day from it when they are invalid
  expect_equal(out$eventDate[c(1, 4, 5)], c("2014-09-04", "2005-03", "2010-05-12"))
  expect_equal(out$year, c("2014", "2000", NA, "2005", "2010"))
  expect_equal(out$month, c("9", NA, NA, "3", "5"))
  expect_equal(out$day, c("4", NA, NA, NA, "12"))
  # not a date: rebuilt from year, month and day, otherwise missing
  expect_equal(out$eventDate[2], "2000")
  expect_true(is.na(out$eventDate[3]))

  expect_equal(out$minimumElevationInMeters, c("100", NA, NA, "1.5", "30"))
  expect_equal(out$recordNumber, c("1", "s.n.", "s.n.", NA, "7"))
  expect_equal(out$dateIdentified, c("2020-04-28", NA, NA, "2005", NA))
  # 0/0 and out-of-range coordinates are not coordinates
  expect_equal(out$decimalLatitude, c(-12.5, NA, NA, 10, NA))
  expect_equal(out$decimalLongitude, c(-38.5, NA, NA, 20, NA))

  # columns that are not in the data are not created
  expect_equal(names(.std_dates_and_numbers(data.frame(a = 1))), "a")
})


test_that(".std_family_synonyms unifies the alternative family names", {
  out <- .std_family_synonyms(data.frame(
    family = c("Leguminosae", "Compositae", "Palmae", "Gramineae", "Fabaceae",
               "Guttiferae", NA),
    stringsAsFactors = FALSE))
  expect_equal(out$family, c("Fabaceae", "Asteraceae", "Arecaceae", "Poaceae",
                             "Fabaceae", "Guttiferae", NA))
  expect_equal(.std_family_synonyms(data.frame(a = 1))$a, 1)
})


test_that("jabot_parse returns the standardized columns", {
  tmp <- tempfile("jabot_parse_std_")
  dir.create(tmp)
  folder <- file.path(tmp, "dwca_AFR_001")
  dir.create(folder, recursive = TRUE)
  utils::write.csv(data.frame(collectionCode = "AFR", Records = 3),
                   file.path(folder, "AFR_Jabot.csv"), row.names = FALSE)

  fake_read <- function(input, read, encoding, na.strings) {
    occurrence <- data.frame(
      occurrenceID = c("1", "2", "3"),
      institutionCode = "JBRJ",
      collectionCode = "AFR",
      catalogNumber = c("1", "2", "3"),
      taxonRank = NA_character_,
      family = c("Leguminosae", "fabaceae", "Compositae"),
      genus = c("inga", "inga", "baccharis"),
      specificEpithet = c("edulis", "vera", "trimera"),
      infraspecificEpithet = NA_character_,
      scientificNameAuthorship = NA_character_,
      scientificName = NA_character_,
      recordNumber = c("S/N", "10", "sn"),
      eventDate = c("2014-9-4", "2015", NA),
      year = c("2014", "2015", "0"),
      month = c("9", NA, "0"),
      day = c("4", NA, "0"),
      country = c("Brasil", "Brazil", "Bolívia"),
      countryCode = NA_character_,
      stateProvince = c("São Paulo", "Pernanbuco", "La Paz"),
      municipality = c("Campinas", "Recife", "La Paz"),
      locality = NA_character_,
      minimumElevationInMeters = c("-9999", "100", NA),
      typeStatus = c("Holotipo", "Chapéu de couro", NA),
      dateIdentified = c("28/4/2020", NA, NA),
      stringsAsFactors = FALSE
    )
    list(data = list("occurrence.txt" = occurrence),
         files = list(xml_files = file.path(input, "eml.xml")))
  }
  testthat::local_mocked_bindings(dwca_read = fake_read, .package = "finch")
  testthat::local_mocked_bindings(
    .arg_check_path = function(...) invisible(TRUE),
    .std_inside_columns = function(df, herbarium = NULL, i = NULL, verbose = TRUE) df,
    .package = "jabotR"
  )

  out <- jabot_parse(path = tmp, verbose = FALSE)
  occ <- out[[1]]$data$occurrence.txt

  expect_equal(occ$country, c("Brazil", "Brazil", "Bolivia"))
  expect_equal(occ$countryCode, c("BR", "BR", "BO"))
  expect_equal(occ$stateProvince[1:2], c("São Paulo", "Pernambuco"))
  expect_equal(occ$family, c("Fabaceae", "Fabaceae", "Asteraceae"))
  expect_equal(occ$typeStatus, c("Holotype", NA, NA))
  expect_equal(occ$recordNumber, c("s.n.", "10", "s.n."))
  expect_equal(occ$eventDate, c("2014-09-04", "2015", NA))
  expect_equal(occ$year, c("2014", "2015", NA))
  expect_equal(occ$minimumElevationInMeters, c(NA, "100", NA))
  expect_equal(occ$dateIdentified, c("2020-04-28", NA, NA))
})
