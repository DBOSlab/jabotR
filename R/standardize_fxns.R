#_______________________________________________________________________________
# Standardization of geographic, type status, date and numeric columns of the
# JABOT occurrence records (used by jabot_parse) ####

#' Text key without accents, in lower case, to compare names
#'
#' @keywords internal
#' @noRd
.norm_key <- function(x) {
  x <- tolower(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
  x <- gsub("\\s+", " ", x)
  x[!nzchar(x)] <- NA_character_
  x
}

#' Wrapper of the whole standardization of the non-taxonomic columns
#'
#' @keywords internal
#' @noRd
.std_occurrence_columns <- function(df) {
  df <- .std_geography(df)
  if ("typeStatus" %in% names(df)) {
    df$typeStatus <- .std_typeStatus(df$typeStatus)
  }
  df <- .std_dates_and_numbers(df)
  df <- .std_family_synonyms(df)
  df
}


#_______________________________________________________________________________
# Country, country code, state and municipality ####

#' Country values that are not recognized as an English country name, and what
#' they are in fact (NA when it is not a country or cannot be told)
#'
#' @keywords internal
#' @noRd
.country_fixes <- function() {
  c(# Typos of Brazil
    "brasi" = "Brazil", "braasil" = "Brazil", "barsil" = "Brazil",
    "basil" = "Brazil", "brasil" = "Brazil",
    # Brazilian places in the country column
    "alagoas" = "Brazil", "rio de janeiro" = "Brazil",
    "rio grande do sul" = "Brazil", "belem" = "Brazil",
    # Portuguese or old names, typos
    "antartica" = "Antarctica", "peninsula antartica" = "Antarctica",
    "ceilao" = "Sri Lanka", "tchecoslovaquia" = "Czechoslovakia",
    "iugoslavia" = "Yugoslavia", "iougoslavia" = "Yugoslavia",
    "urss" = "Soviet Union", "rodesia" = "Zimbabwe", "zimbaue" = "Zimbabwe",
    "tasmania" = "Australia", "nova granada" = "Colombia",
    "st. domingue" = "Haiti", "republica da mauricia" = "Mauritius",
    "ilhas mauricio" = "Mauritius", "japapn" = "Japan", "urugay" = "Uruguay",
    "nova guine" = "Papua New Guinea", "emirados arabes" = "United Arab Emirates",
    "honduras brit." = "Belize", "arabia" = "Saudi Arabia",
    "austria inferior" = "Austria", "bosnia" = "Bosnia & Herzegovina",
    "ira" = "Iran", "persia" = "Iran",
    "rep. dominicana" = "Dominican Republic", "malaya" = "Malaysia",
    "estados malaios" = "Malaysia", "java" = "Indonesia",
    "congo belga" = "Congo - Kinshasa",
    "democratic republic of th" = "Congo - Kinshasa",
    "democratic republic of co" = "Congo - Kinshasa",
    # Regions, cities and provinces of other countries
    "ilhas britanicas" = "United Kingdom", "british isles" = "United Kingdom",
    "wales" = "United Kingdom", "oxfordshire" = "United Kingdom",
    "antofagasta" = "Chile", "provincia de talca" = "Chile",
    "paris" = "France", "florida" = "United States", "hawai" = "United States",
    "hanover" = "Germany", "hamburg" = "Germany", "karavasta" = "Albania",
    "nassau" = "Bahamas", "transvaal" = "South Africa", "zanzibar" = "Tanzania",
    "tibet" = "China", "tibete" = "China", "hunan" = "China",
    "antioquia" = "Colombia", "campeche" = "Mexico",
    # Not a country, or impossible to tell
    "capital" = NA, "indisponivel" = NA, "indeterminado" = NA,
    "indeterminada" = NA, "sem procedencia" = NA, "?" = NA, "s.loc." = NA,
    "s." = NA, "bb" = NA, "africa" = NA, "america" = NA,
    "america do norte" = NA, "caribe" = NA, "antilhas" = NA, "coreia" = NA,
    "espanha - franca" = NA, "brunswick" = NA, "borneu" = NA, "trindade" = NA,
    "ilhas canarias" = "Spain", "corsega" = "France")
}

#' Standardize country and country code (English names, ISO 3166 alpha-2 codes)
#'
#' @keywords internal
#' @noRd
.std_country <- function(country, countryCode) {
  out <- trimws(as.character(country))
  out[!nzchar(out)] <- NA_character_

  fixes <- .country_fixes()
  hit <- match(.norm_key(out), names(fixes))
  has <- !is.na(hit)
  out[has] <- unname(fixes[hit[has]])

  # Whatever is not an English country name is not kept
  historical <- c("Yugoslavia", "Czechoslovakia", "Soviet Union")
  iso <- suppressWarnings(countrycode::countrycode(out, "country.name",
                                                    "iso2c", warn = FALSE))
  out[!is.na(out) & is.na(iso) & !out %in% historical] <- NA_character_

  # Country from the code, when the country is missing
  code <- toupper(trimws(as.character(countryCode)))
  from_code <- suppressWarnings(countrycode::countrycode(code, "iso2c",
                                                         "country.name",
                                                         warn = FALSE))
  fill <- is.na(out) & !is.na(from_code)
  out[fill] <- from_code[fill]

  out
}

#' Country code of each country name
#'
#' @keywords internal
#' @noRd
.country_code <- function(country) {
  code <- suppressWarnings(countrycode::countrycode(country, "country.name",
                                                    "iso2c", warn = FALSE))
  code[country %in% c("Yugoslavia", "Czechoslovakia", "Soviet Union")] <-
    NA_character_
  code
}

#' Missing country from the country of the other records with the same state
#'
#' @keywords internal
#' @noRd
.impute_country_from_state <- function(country, stateProvince) {
  key <- .norm_key(stateProvince)
  known <- !is.na(country) & !is.na(key)
  if (!any(known) || !any(is.na(country) & !is.na(key))) return(country)

  tab <- table(key[known], country[known])
  top <- apply(tab, 1, which.max)
  share <- apply(tab, 1, function(x) max(x) / sum(x))
  n <- rowSums(tab)
  map <- colnames(tab)[top]
  names(map) <- rownames(tab)
  ok <- share >= 0.95 & n >= 3

  miss <- which(is.na(country) & !is.na(key) & key %in% names(map)[ok])
  country[miss] <- unname(map[key[miss]])
  country
}

#' Brazilian state aliases found in the state column
#'
#' @keywords internal
#' @noRd
.state_aliases <- function() {
  c("brasilia" = "Distrito Federal", "guanabara" = "Rio de Janeiro",
    "minas" = "Minas Gerais", "r.j" = "Rio de Janeiro",
    "territorio do amapa" = "Amap\u00e1", "roraima t.f." = "Roraima",
    "rio g do sul" = "Rio Grande do Sul", "rio g. do sul" = "Rio Grande do Sul")
}

#' Standardize the Brazilian states: spelling, ", MG", "Estado do ..." and
#' typos. Values that are not a state but a known municipality go to the
#' municipality column.
#'
#' @keywords internal
#' @noRd
.std_state_brazil <- function(df) {
  valid <- names(.br_states())
  vkeys <- .norm_key(valid)
  is_br <- !is.na(df$country) & df$country == "Brazil" &
    !is.na(df$stateProvince)
  idx <- which(is_br)
  if (length(idx) == 0) return(df)

  raw <- df$stateProvince[idx]
  key <- .norm_key(raw)
  key <- gsub("[()?]", "", key)
  key <- sub("^estado (do|da|de) ", "", key)
  key <- sub("\\s*,\\s*[a-z]{2}$", "", key)
  key <- trimws(key)

  out <- rep(NA_character_, length(idx))
  m <- match(key, vkeys)
  out[!is.na(m)] <- valid[m[!is.na(m)]]

  al <- .state_aliases()
  m <- match(key, names(al))
  fill <- is.na(out) & !is.na(m)
  out[fill] <- unname(al[m[fill]])

  # Typos (Pernanbuco, Mato Groso do Sul), unless it is a known municipality
  munis <- .norm_key(df$municipality[!is.na(df$municipality) &
                                       df$stateProvince %in% valid])
  munis <- unique(munis[!is.na(munis)])
  todo <- which(is.na(out) & !is.na(key) & nchar(key) >= 5 &
                  !key %in% munis)
  if (length(todo)) {
    uk <- unique(key[todo])
    d <- utils::adist(uk, vkeys)
    best <- apply(d, 1, which.min)
    mind <- apply(d, 1, min)
    thr <- ifelse(nchar(uk) >= 8, 2, 1)
    fz <- ifelse(mind <= thr, valid[best], NA_character_)
    out[todo] <- fz[match(key[todo], uk)]
  }

  # Values that are not a state: a known municipality is kept as municipality
  # and its state taken from the other records with this municipality
  bad <- which(is.na(out))
  if (length(bad)) {
    ok_rows <- which(df$stateProvince %in% valid & !is.na(df$municipality))
    mk <- .norm_key(df$municipality[ok_rows])
    tab <- table(mk, df$stateProvince[ok_rows])
    top <- apply(tab, 1, which.max)
    share <- apply(tab, 1, function(x) max(x) / sum(x))
    map <- colnames(tab)[top]
    names(map) <- rownames(tab)
    map <- map[share >= 0.9]

    bk <- key[bad]
    is_muni <- bk %in% munis
    st <- unname(map[bk])
    out[bad] <- ifelse(is_muni, st, NA_character_)

    set_muni <- idx[bad][is_muni & is.na(df$municipality[idx[bad]])]
    df$municipality[set_muni] <- raw[bad][is_muni &
                                          is.na(df$municipality[idx[bad]])]
  }

  df$stateProvince[idx] <- out
  df
}

#' Standardize country, country code, state and municipality
#'
#' The cleaning of barRoso (translation of countries to English, state
#' abbreviations, words such as "Municipio de") is followed by the corrections
#' needed by the JABOT records.
#'
#' @keywords internal
#' @noRd
.std_geography <- function(df) {
  cols <- c("country", "countryCode", "stateProvince", "municipality",
            "locality")
  for (nm in setdiff(cols, names(df))) df[[nm]] <- NA_character_

  # In chunks, which is much faster than a single call for the large herbaria
  place <- df[, cols, drop = FALSE]
  chunks <- split(seq_len(nrow(place)), ceiling(seq_len(nrow(place)) / 50000))
  place <- dplyr::bind_rows(lapply(chunks, function(i) {
    suppressWarnings(suppressMessages(
      barRoso::std_place(place[i, , drop = FALSE], rm_original_column = TRUE)))
  }))
  for (nm in cols) df[[nm]] <- place[[nm]]

  df$country <- .std_country(df$country, df$countryCode)
  df <- .std_state_brazil(df)
  df$country <- .impute_country_from_state(df$country, df$stateProvince)

  # Brazilian state with no country
  tf <- is.na(df$country) & df$stateProvince %in% names(.br_states())
  df$country[tf] <- "Brazil"

  df$countryCode <- .country_code(df$country)
  df
}


#_______________________________________________________________________________
# Type status ####

#' Standardize the type status to English terms (Holotype, Isotype...)
#'
#' Words in Portuguese, Latin or English (holotipo, Holotypus, HOLOTYPE) become
#' the English term. Values that are not a type (vernacular names, "cf.",
#' loans) and possible types are removed.
#'
#' @keywords internal
#' @noRd
.std_typeStatus <- function(x) {
  key <- .norm_key(x)
  out <- rep(NA_character_, length(key))

  # The type to be standardized: "tipo", "typus", "type", "typo", "tyj..."
  rules <- list(
    c("^isoparat[iy]p[a-z]*$", "Isoparatype"),
    c("^isosin?t[iy]p[a-z]*$|^isosynt[iy]p[a-z]*$", "Isosyntype"),
    c("^isolecto?t[iy]p[a-z]*$", "Isolectotype"),
    c("^isoneo?t[iy]p[a-z]*$", "Isoneotype"),
    c("^isoepi?t[iy]p[a-z]*$", "Isoepitype"),
    c("^isot[iy]p[a-z]*$", "Isotype"),
    c("^paralecto?t[iy]p[a-z]*$", "Paralectotype"),
    c("^parat[iy]p[a-z]*$", "Paratype"),
    c("^lecto?t[iy]p[a-z]*$", "Lectotype"),
    c("^neo?t[iy]p[a-z]*$", "Neotype"),
    c("^epi?t[iy]p[a-z]*$", "Epitype"),
    c("^holo?[iy]p[a-z]*$|^holot[iy]p[a-z]*$", "Holotype"),
    c("^sin?t[iy]p[a-z]*$|^synt[iy]p[a-z]*$", "Syntype"),
    c("^topot[iy]p[a-z]*$", "Topotype"),
    c("^cot[iy]p[a-z]*$", "Cotype"),
    c("^clonot[iy]p[a-z]*$", "Clonotype"),
    c("^homeot[iy]p[a-z]*$", "Homeotype"),
    c("^fotot[iy]p[a-z]*$|^phototyp[a-z]*$", "Phototype"),
    c("^typus$|^type$|^tipo$|^typo$", "Type")
  )

  key <- sub("\\s+(fem|masc|f|m)$", "", key)
  for (r in rules) {
    tf <- !is.na(key) & is.na(out) & grepl(r[1], key)
    out[tf] <- r[2]
  }
  out
}


#_______________________________________________________________________________
# Dates and numbers ####

#' Elevation without sentinels (-9999), decimal commas or impossible values
#'
#' @keywords internal
#' @noRd
.std_elevation <- function(x) {
  txt <- trimws(as.character(x))
  txt <- sub(",", ".", txt, fixed = TRUE)
  num <- suppressWarnings(as.numeric(txt))
  num[!is.na(num) & (num <= -500 | num > 8849)] <- NA
  out <- ifelse(is.na(num), NA_character_, format(num, trim = TRUE,
                                                  scientific = FALSE,
                                                  drop0trailing = TRUE))
  out[is.na(x)] <- NA_character_
  out
}

#' Year, month and day inside their valid range (0 is "unknown")
#'
#' @keywords internal
#' @noRd
.std_date_part <- function(x, min, max) {
  num <- suppressWarnings(as.integer(trimws(as.character(x))))
  num[!is.na(num) & (num < min | num > max)] <- NA
  as.character(num)
}

#' Event date in the ISO format yyyy-mm-dd (or yyyy-mm, yyyy)
#'
#' @keywords internal
#' @noRd
.std_eventDate <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & grepl("^[0-9]{4}(-[0-9]{1,2}){0,2}$", x)
  if (!any(ok)) return(out)

  v <- x[ok]
  y <- substr(v, 1, 4)
  parts <- strsplit(sub("^[0-9]{4}-?", "", v), "-", fixed = TRUE)
  mo <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, "")
  d <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, "")
  mo <- sprintf("%02d", suppressWarnings(as.integer(mo)))
  d <- sprintf("%02d", suppressWarnings(as.integer(d)))

  txt <- y
  has_m <- !grepl("NA", mo, fixed = TRUE)
  has_d <- has_m & !grepl("NA", d, fixed = TRUE)
  txt[has_m] <- paste(y[has_m], mo[has_m], sep = "-")
  txt[has_d] <- paste(y[has_d], mo[has_d], d[has_d], sep = "-")
  out[ok] <- txt
  out
}

#' Date of identification (d/m/yyyy, m/yyyy, yyyy, d/m/yy) in the ISO format
#'
#' Two-digit years are in the 2000s up to the current year, otherwise in the
#' 1900s.
#'
#' @keywords internal
#' @noRd
.std_dateIdentified <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  this_yy <- as.integer(format(Sys.Date(), "%y"))

  # d/m/y
  dmy <- !is.na(x) & grepl("^[0-9]{1,2}/[0-9]{1,2}/([0-9]{2}|[0-9]{4})$", x)
  if (any(dmy)) {
    v <- x[dmy]
    d <- as.integer(sub("^([0-9]{1,2})/.*", "\\1", v))
    mo <- as.integer(sub("^[0-9]{1,2}/([0-9]{1,2})/.*", "\\1", v))
    y <- as.integer(sub("^[0-9]{1,2}/[0-9]{1,2}/", "", v))
    y <- ifelse(y < 100, ifelse(y <= this_yy, 2000L + y, 1900L + y), y)
    iso <- sprintf("%04d-%02d-%02d", y, mo, d)
    good <- !is.na(as.Date(iso, optional = TRUE))
    res <- rep(NA_character_, length(v))
    res[good] <- iso[good]
    out[dmy] <- res
  }

  # m/yyyy
  my <- !is.na(x) & grepl("^[0-9]{1,2}/[0-9]{4}$", x)
  if (any(my)) {
    v <- x[my]
    mo <- as.integer(sub("/.*", "", v))
    y <- as.integer(sub("^[0-9]{1,2}/", "", v))
    res <- ifelse(mo >= 1 & mo <= 12, sprintf("%04d-%02d", y, mo), NA_character_)
    out[my] <- res
  }

  # yyyy, or already in the ISO format
  yy <- !is.na(x) & grepl("^[0-9]{4}$", x)
  out[yy] <- x[yy]
  iso <- !is.na(x) & is.na(out) & grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?$", x)
  out[iso] <- x[iso]
  out
}

#' Collector numbers without number: s.n., s/n, S/N, sn, s/n, sem numero...
#'
#' @keywords internal
#' @noRd
.std_recordNumber <- function(x) {
  x <- trimws(as.character(x))
  key <- gsub("[\u00ba\u00b0]", "", .norm_key(x))
  sn <- !is.na(key) & (grepl("^(s[./ ]?n[.,/]*)+$", key) |
                         grepl("^sem n(umero|[.])?$", key))
  x[sn] <- "s.n."
  x[!is.na(x) & x %in% c("", "?")] <- NA_character_
  x
}

#' Standardize dates, collector numbers, elevation and coordinates
#'
#' @keywords internal
#' @noRd
.std_dates_and_numbers <- function(df) {
  for (nm in c("minimumElevationInMeters", "maximumElevationInMeters")) {
    if (nm %in% names(df)) df[[nm]] <- .std_elevation(df[[nm]])
  }

  if ("recordNumber" %in% names(df)) {
    df$recordNumber <- .std_recordNumber(df$recordNumber)
  }

  # Year, month and day, from the event date when they are invalid or missing
  ed <- if ("eventDate" %in% names(df)) .std_eventDate(df$eventDate) else NULL
  if (!is.null(ed)) {
    ed_y <- ifelse(is.na(ed), NA_character_, substr(ed, 1, 4))
    ed_m <- ifelse(!is.na(ed) & nchar(ed) >= 7, substr(ed, 6, 7), NA_character_)
    ed_d <- ifelse(!is.na(ed) & nchar(ed) >= 10, substr(ed, 9, 10), NA_character_)
  }
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  parts <- list(year = c(1500, this_year), month = c(1, 12), day = c(1, 31))
  for (nm in names(parts)) {
    if (!nm %in% names(df)) next
    v <- .std_date_part(df[[nm]], parts[[nm]][1], parts[[nm]][2])
    if (!is.null(ed)) {
      src <- list(year = ed_y, month = ed_m, day = ed_d)[[nm]]
      fill <- is.na(v) & !is.na(src)
      v[fill] <- .std_date_part(src[fill], parts[[nm]][1], parts[[nm]][2])
    }
    df[[nm]] <- v
  }

  # Event date: ISO format, rebuilt from year/month/day when it is not valid
  if (!is.null(ed)) {
    rebuilt <- rep(NA_character_, nrow(df))
    if (all(c("year", "month", "day") %in% names(df))) {
      y <- df$year; mo <- df$month; d <- df$day
      rebuilt <- ifelse(!is.na(y) & !is.na(mo) & !is.na(d),
                        sprintf("%s-%02d-%02d", y, suppressWarnings(as.integer(mo)),
                                suppressWarnings(as.integer(d))),
                        ifelse(!is.na(y) & !is.na(mo),
                               sprintf("%s-%02d", y, suppressWarnings(as.integer(mo))),
                               ifelse(!is.na(y), y, NA_character_)))
    }
    no_ed <- is.na(ed) & !is.na(rebuilt)
    ed[no_ed] <- rebuilt[no_ed]
    df$eventDate <- ed
  }

  if ("dateIdentified" %in% names(df)) {
    df$dateIdentified <- .std_dateIdentified(df$dateIdentified)
  }

  # Coordinates: 0/0 and values out of range are not coordinates
  if (all(c("decimalLatitude", "decimalLongitude") %in% names(df))) {
    la <- suppressWarnings(as.numeric(df$decimalLatitude))
    lo <- suppressWarnings(as.numeric(df$decimalLongitude))
    bad <- (!is.na(la) & !is.na(lo) & la == 0 & lo == 0) |
      (!is.na(la) & abs(la) > 90) | (!is.na(lo) & abs(lo) > 180)
    la[bad] <- NA
    lo[bad] <- NA
    df$decimalLatitude <- la
    df$decimalLongitude <- lo
  }
  df
}


#_______________________________________________________________________________
# Family synonyms ####

#' Alternative family names that have only one equivalent in the current
#' classification (Leguminosae = Fabaceae, Compositae = Asteraceae...)
#'
#' @keywords internal
#' @noRd
.std_family_synonyms <- function(df) {
  if (!"family" %in% names(df)) return(df)
  syn <- c(Leguminosae = "Fabaceae", Mimosaceae = "Fabaceae",
           Caesalpiniaceae = "Fabaceae", Papilionaceae = "Fabaceae",
           Compositae = "Asteraceae", Palmae = "Arecaceae",
           Cruciferae = "Brassicaceae", Labiatae = "Lamiaceae",
           Umbelliferae = "Apiaceae", Gramineae = "Poaceae")
  m <- match(df$family, names(syn))
  tf <- !is.na(m)
  df$family[tf] <- unname(syn[m[tf]])
  df
}
