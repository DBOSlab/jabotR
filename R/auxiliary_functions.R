# Auxiliary functions to support main functions
# Author: Domingos Cardoso


#_______________________________________________________________________________
# Function to get raw metadata from JABOT repository ####

.get_ipt_info <- function(herbarium) {

  ipt_metadata <- readLines("https://ipt.jbrj.gov.br/jabot/dcat",
                            encoding = "UTF-8",
                            warn = F)

  pos = which(grepl("dcat[:]downloadURL\\s", ipt_metadata))
  URLs <- gsub(".*\\s[<]|[>]\\s;$", "", ipt_metadata[pos])
  herb_URLs <- gsub(".*r[=]|[>]\\s;$", "", URLs)
  herb_code <- toupper(gsub("_.*", "", herb_URLs))

  ini = which(grepl("a dcat:Dataset ;", ipt_metadata))
  end = which(grepl("dcat:mediaType \"application/zip\" ;", ipt_metadata))
  temp <- list()
  for (i in seq_along(ini)) {
    temp[[i]] = paste0(ipt_metadata[ini[i]:end[i]], collapse = " | ")
  }
  ipt_metadata = temp

  ipt_metadata <- lapply(ipt_metadata, function(x) strsplit(x, "\\s[|]\\s")[[1]])

  rb <- .get_ipt_info_rb()

  ipt_metadata <- append(ipt_metadata, rb[[1]])
  herb_URLs <- append(herb_URLs, rb[[2]])
  herb_code <- append(herb_code, rb[[3]])

  if (!is.null(herbarium)) {
    ipt_metadata <- ipt_metadata[herb_code %in% herbarium]
    herb_URLs <- herb_URLs[herb_code %in% herbarium]
    herb_code <- herb_code[herb_code %in% herbarium]
  }
  return(list(ipt_metadata, herb_URLs, herb_code))
}


.get_ipt_info_rb <- function() {

  ipt_metadata <- readLines("https://ipt.jbrj.gov.br/jbrj/dcat",
                            encoding = "UTF-8",
                            warn = F)

  pos = which(grepl("dcat[:]downloadURL\\s", ipt_metadata))
  URLs <- gsub(".*\\s[<]|[>]\\s;$", "", ipt_metadata[pos])
  herb_URLs <- gsub(".*r[=]|[>]\\s;$", "", URLs)
  herb_code <- toupper(gsub("_.*", "", herb_URLs))

  ini = which(grepl("a dcat:Dataset ;", ipt_metadata))
  end = which(grepl("dcat:mediaType \"application/zip\" ;", ipt_metadata))
  temp <- list()
  for (i in seq_along(ini)) {
    temp[[i]] = paste0(ipt_metadata[ini[i]:end[i]], collapse = " | ")
  }
  ipt_metadata = temp

  ipt_metadata <- lapply(ipt_metadata, function(x) strsplit(x, "\\s[|]\\s")[[1]])
  tf <- herb_URLs %in% "jbrj_rb"
  ipt_metadata <- ipt_metadata[tf]
  herb_URLs <- herb_URLs[tf]
  herb_code <- "RB"

  return(list(ipt_metadata, herb_URLs, herb_code))
}


#_______________________________________________________________________________
# Function to get summary information of each JABOT-associated collection ####

.get_herb_info <- function(herb_URLs, ipt_metadata, i) {

  if (herb_URLs[i] %in% "jbrj_rb") {
    herb_url <- paste0("https://ipt.jbrj.gov.br/jbrj/resource?r=", herb_URLs[i])
  } else {
    herb_url <- paste0("https://ipt.jbrj.gov.br/jabot/resource?r=", herb_URLs[i])
  }

  version <- readLines(herb_url,
                       encoding = "UTF-8",
                       warn = F)

  ini = which(grepl("latestVersion", version))[1]
  end = which(grepl("\\d{4}-\\d{2}-\\d{2}", version))[1]+1

  version = version[ini:end]

  version <- gsub("(\\s){2,}|\\'|,$", "", version)
  version <- gsub(".*[>]", "", version)

  contact <- ipt_metadata[[i]][which(grepl("dcat:contactPoint", ipt_metadata[[i]]))[1]]
  # Regular expression for extracting the name
  name <- regmatches(contact, gregexpr('vcard:fn "([^"]+)"', contact, perl = TRUE))[[1]]
  name <- gsub('vcard:fn "|\"', "", name)  # Remove the 'vcard:fn "' part

  # Regular expression for extracting the email
  email <- regmatches(contact, gregexpr('<mailto:([^>]+)>', contact, perl = TRUE))[[1]]
  email <- gsub('<mailto:|>', "", email)  # Remove the '<mailto:' part

  rights_holder <- gsub("Flores\\s-\\s\\s|Plantulas\\s-\\s", "", ipt_metadata[[i]][2])
  rights_holder <- sub('^[^\\-]+ - ([^\\-]+?)\\s*-.*$', '\\1', rights_holder)
  rights_holder <- gsub(".*\\s-\\s|[.]\\sJabot.*|[.]\\sJABOT.*|\"\\s;$|^\\s|\\sHerbarium\\sCollection$",
                        "", rights_holder)

  return(list(version, name, email, rights_holder, herb_url))
}


#_______________________________________________________________________________
# Function to reorder retrieved data based on specific columns ####

.reorder_df <- function(df, reorder) {

  if (!is.null(reorder)) {
    columns_to_order <- reorder
  } else {
    columns_to_order <- c("herbarium", "taxa", "collector", "area", "year")
  }

  tf <- columns_to_order %in% "herbarium"
  if (any(tf)) {
    columns_to_order[tf] <- "collectionCode"
  }
  tf <- columns_to_order %in% "taxa"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("family", "genus", "specificEpithet"),
                                      columns_to_replace = "taxa")
  }
  tf <- columns_to_order %in% "collector"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("recordedBy", "recordNumber"),
                                      columns_to_replace = "collector")
  }
  tf <- columns_to_order %in% "area"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("country", "stateProvince", "municipality"),
                                      columns_to_replace = "area")
  }

  # Arrange the dataframe based on the vector of column names
  df <- df %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(columns_to_order)))

  return(df)
}

.replace_cols <- function (columns_to_order,
                           replace,
                           columns_to_replace) {
  index <- which(columns_to_order == columns_to_replace)
  # Perform the replacement based on the index position
  if (index == 1) {
    # Special handling if 'herbarium' is the first element
    columns_to_order <- c(replace, columns_to_order[(index+1):length(columns_to_order)])
  } else if (index == length(columns_to_order)) {
    # Special handling if the element to replace is the last element
    columns_to_order <- c(columns_to_order[1:(index-1)], replace)
  } else {
    # Standard replacement for elements in the middle
    columns_to_order <- c(columns_to_order[1:(index-1)], replace,
                          columns_to_order[(index+1):length(columns_to_order)])
  }

  return(columns_to_order)
}


#_______________________________________________________________________________
# Function for standardizing taxonRank and taxonomic columns ####

.std_inside_columns <- function(df,
                                verbose = verbose) {

  if (verbose) {
    message("Standardizing taxonomic columns...")
  }

  # Standardize and clean taxonRank column
  taxonrank_form <- c("f.", "form", "Forma", "forma", "FORM", "FORMA")
  taxonrank_var <- c("var.", "VAR.", "Variedade", "variedade", "VARIEDADE", "VARIETY", "variety")
  taxonrank_subsp <- c("subsp.", "ssp.", "subespecie", "Subespecie", "subesp\u00e9cie", "Subesp\u00e9cie", "SUBSP.", "SUBSP", "SUB_ESPECIE", "Infr.", "infr.", "infraspecific", "subspecies", "Subspecies", "SUBSPECIES")
  taxonrank_species <- c("sp", "sp.", "especie", "ESPECIE", "Especie", "Esp\u00e9cie", "esp\u00e9cie", "ESP\u00caCIE", "species", "Species", "specie", "SPECIES", "SPECIE")
  taxonrank_genus <- c("genero", "Genero", "GENERO", "G\u00eanero", "g\u00eanero", "G\u00caNERO", "gen.", "genus", "Genus", "GENUS")
  taxonrank_tribe <- c("tribo", "TRIBO", "tribe", "Tribe", "TRIBE")
  taxonrank_subfam <- c("sub_familia", "SUB_FAMILIA", "subfamily", "Subfamily", "SUBFAMILY")
  taxonrank_family <- c("fam.", "fam\u00edlia", "Fam\u00edlia", "FAM\u00cdLIA", "familia", "Familia", "FAMILIA", "family", "Family", "FAMILY")
  taxonrank_order <- c("ordem", "Ordem", "ORDEM", "order", "Order", "ORDER")
  taxonrank_class <- c("classe", "CLASSE", "class", "Class", "CLASS")
  taxonrank_division <- c("divisao", "DIVISAO", "divis\u00e3o", "DIVIS\u00c3O", "division", "Division", "DIVISION")
  taxonrank_kingdom <- c("reino", "Reino", "REINO", "kingdom", "Kingdom", "KINGDOM")

  # Create unified taxon rank mapping
  taxonrank_map <- c(
    stats::setNames(rep("FORMA", length(taxonrank_form)), taxonrank_form),
    stats::setNames(rep("VARIETAS", length(taxonrank_var)), taxonrank_var),
    stats::setNames(rep("SUBSPECIES", length(taxonrank_subsp)), taxonrank_subsp),
    stats::setNames(rep("SPECIES", length(taxonrank_species)), taxonrank_species),
    stats::setNames(rep("GENUS", length(taxonrank_genus)), taxonrank_genus),
    stats::setNames(rep("TRIBE", length(taxonrank_tribe)), taxonrank_tribe),
    stats::setNames(rep("SUBFAMILY", length(taxonrank_subfam)), taxonrank_subfam),
    stats::setNames(rep("FAMILY", length(taxonrank_family)), taxonrank_family),
    stats::setNames(rep("ORDER", length(taxonrank_order)), taxonrank_order),
    stats::setNames(rep("CLASS", length(taxonrank_class)), taxonrank_class),
    stats::setNames(rep("DIVISION", length(taxonrank_division)), taxonrank_division),
    stats::setNames(rep("KINGDOM", length(taxonrank_kingdom)), taxonrank_kingdom)
  )

  # Normalize taxonRank values
  df <- df %>%
    dplyr::mutate(
      taxonRank = dplyr::recode(taxonRank, !!!taxonrank_map)
    )

  tf <- !is.na(df$taxonRank) & is.na(df$family) & is.na(df$genus) & is.na(df$specificEpithet) & is.na(df$infraspecificEpithet)
  if (any(tf)) {
    df$taxonRank[tf] = NA
  }

  # Remove scientific names erroneously added into the taxonRank column
  taxonranks <- c("INFRASPECIFIC", "FORMA", "SUBSPECIES", "VARIETAS",
                  "SPECIES", "GENUS", "TRIBE", "SUBFAMILY",
                  "FAMILY", "ORDER", "CLASS", "DIVISION", "KINGDOM")
  n_diff <- setdiff(df$taxonRank, taxonranks)
  if (length(n_diff > 0)) {
    tf <- grepl("aceae$|ACEAE", n_diff)
    if (any(tf)) {
      df$taxonRank[df$taxonRank %in% n_diff[tf]] <- "FAMILY"
    }
    tf <- grepl("[[:lower:]]\\s[[:lower:]]", n_diff)
    if (any(tf)) {
      df$taxonRank[df$taxonRank %in% n_diff[tf]] <- "SPECIES"
    }
    df$taxonRank[df$taxonRank %in% n_diff] <- "GENUS"
  }

  #_____________________________________________________________________________
  # Cleanup invisible characters  ####
  pattern <- c("^\\p{Z}+|\\p{Z}+$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- stringi::stri_trim_both(df$family[tf])
  }

  pattern <- c("^\\p{Z}+|\\p{Z}+$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- stringi::stri_trim_both(df$genus[tf])
  }

  pattern <- c("^\\p{Z}+|\\p{Z}+$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$specificEpithet, pattern), FALSE)
  if (any(tf)) {
    df$specificEpithet[tf] <- stringi::stri_trim_both(df$specificEpithet[tf])
  }

  pattern <- c("^\\p{Z}+|\\p{Z}+$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$infraspecificEpithet, pattern), FALSE)
  if (any(tf)) {
    df$infraspecificEpithet[tf] <- stringi::stri_trim_both(df$infraspecificEpithet[tf])
  }

  #_____________________________________________________________________________
  # $family cleaning ####

  # tf <- grepl("aceae$|Leguminosae|Compositae|Palmae|Cruciferae|Labiatae|Umbeliferae|Guttiferae",
  #             df$family)
  # sort(unique(df$family[!tf]))

  # Clean examples like "Amaranthaceae Alternanthera Maritima Mart"
  tf <- grepl("aceae\\s([[:upper:]]|[[:lower:]]|[(][[:upper:]])|Leguminosae\\sSubfam", df$family)
  if (any(tf)) {
    df$family[tf] <- gsub("\\s.*", "", df$family[tf])
  }

  tf <- grepl("acaeae$|aceaeae$", df$family)
  if (any(tf)) {
    df$family[tf] <- gsub("acaeae$|aceaeae$", "aceae", df$family[tf])
    df$family[tf] <- gsub("Olaceae", "Olacaceae", df$family[tf])
  }

  suffixes <- c("acae", "ace", "acea", "adeae", "aeae", "acedae", "eceae", "aceaea",
                "aceac", "acieae", "aceea", "arceae", "acee", "acaee", "acese",
                "acaea", "acceae", "sceae", "acieae", "ac Eae", "acia", "aecae",
                "aceaae")
  pattern <- paste0("(", paste0(suffixes, collapse = "|"), ")$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- gsub(paste(paste0(suffixes, "$"), collapse = "|"),
                          "aceae", df$family[tf])
  }

  tf <- grepl("aaceae$", df$family)
  if (any(tf)) {
    df$family[tf] <- gsub("aa", "a", df$family[tf])
    df$family[tf] <- gsub("Lindsaceae", "Lindsaeaceae", df$family[tf])
  }

  tf <- df$family %in% c("Leg", "Leguminosa", "Leguminoseae", "Leguminosa",
                         "Leg Caesalpinioideae", "Fabaceae Caesalp.",
                         "Fabaceae Caesalpinioideae", "Faboideae",
                         "Leg Papilionoideae", "Flaboideae", "Fabace",
                         "Fab.", "Papi.", "Papi", "Papilionoideae", "Leguminosae Papilio",
                         "Fabaceae Cercideae", "Fabaceae Faboideae",
                         "Fabaceae Mimosoideae", "Fabaceae/Mimosoideae",
                         "Fabaceaemimosoideae", "Leguminosae Mimos",
                         "Caes.", "Caesalpinioideae", "Caesalpinoideae",
                         "Mim.", "Mim", "Mimosoideae", "Dial.", "Dialioideae",
                         "Cerci.", "Cerc.", "Cercidoideae")
  if (any(tf)) {
    df$family[tf] <- "Fabaceae"
  }

  tf <- df$family %in% c("L", "Jes", "Sem", "X", "Cf", "unknown", "Unknown",
                         "Indet", "Indet.", "Indt", "Em Branco", "Det.",
                         "Indeterminada", "Indeterminado", "Indetermindada",
                         "Ordem", "Classe", "N\u00famero Cancelado",
                         "N\u00famero N\u00e3o Localizado",  "Sp.", "sp.",
                         "Sem Informa\u00e7\u00e3o", "N\u00famero N\u00e3o Encontrado",
                         "Ignorada", "Undesignated", "Zzoutras")
  if (any(tf)) {
    df$family[tf] <- NA
    df$taxonRank[tf] <- NA
  }

  tf <- df$family %in% c("Incertae", "Incertaesedes", "Incertae Sedis")
  if (any(tf)) {
    df$family[tf] <- "INSERTAE SEDIS"
  }

  suffixes <- c("lceae", "siceae", "nceae", "iceae")
  pattern <- paste0("(", paste0(suffixes, collapse = "|"), ")$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- gsub("lceae$", "laceae", df$family[tf])
    df$family[tf] <- gsub("siceae$", "siaceae", df$family[tf])
    df$family[tf] <- gsub("nceae$", "naceae", df$family[tf])
    df$family[tf] <- gsub("iceae$", "iaceae", df$family[tf])
  }

  suffixes <- c("[?]", "[.]", "\\s", "\\s[[:upper:]]", "\\sCf[.]", "\\scf[.]")
  pattern <- paste0("(", paste0(suffixes, collapse = "|"), ")$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- gsub(paste(paste0(suffixes, "$"), collapse = "|"),
                          "", df$family[tf])
  }

  pattern <- c("^(Not\\s|Não é\\s).*aceae$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- NA
    df$taxonRank[tf] <- NA
  }

  pattern <- c("^Cf.*\\s.*aceae$|^cf.*\\s.*aceae$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- gsub("^Cf.*\\s|^cf.*\\s", "", df$family[tf])
  }

  tf <- grepl("sp$|sp[.]$", df$specificEpithet[df$taxonRank %in% "SPECIES"])
  #sort(unique(df$specificEpithet[df$taxonRank %in% "species"][tf]))
  if (any(tf)) {
    df$specificEpithet[df$taxonRank %in% "SPECIES"][tf] <- NA
    df$taxonRank[df$taxonRank %in% "SPECIES"][tf] <- "GENUS"
  }

  tf <- grepl("aceae$", df$genus[df$taxonRank %in% "GENUS"])
  #sort(unique(df$genus[df$taxonRank %in% "GENUS"][tf]))
  if (any(tf)) {
    df$genus[df$taxonRank %in% "GENUS"][tf] <- NA
    df$taxonRank[df$taxonRank %in% "GENUS"][tf] <- "FAMILY"
  }

  index <- which(df$taxonRank == "FAMILY" & is.na(df$specificEpithet))
  tf <- grepl("aceae$", df$genus[index])
  if (any(tf)) {
    df$family[index[tf]] <- df$genus[index[tf]]
    df$genus[index[tf]] <- NA
  }

  temp <- df %>%
    dplyr::filter(
      taxonRank != "FAMILY",
      grepl("aceae$", genus),
      is.na(scientificNameAuthorship) | scientificNameAuthorship %in% names(taxonrank_map)
    )
  if (nrow(temp) > 0) {
    tf <- df$occurrenceID %in% temp$occurrenceID
    df$family[tf] <- temp$genus
    df$genus[tf] <- temp$specificEpithet
    df$specificEpithet[tf] <- temp$infraspecificEpithet
    df$infraspecificEpithet[tf] <- NA
    df$scientificNameAuthorship[tf] <- NA
    df$scientificName[tf] <- NA
    df$taxonName[tf] <- NA
    df$genus[tf] <- .firstUp(df$genus[tf])
    df$taxonRank[tf][is.na(df$genus[tf])] <- "FAMILY"
    df$taxonRank[tf][df$taxonRank[tf] %in% "SPECIES"] <- "GENUS"
    df$taxonRank[tf][!is.na(df$specificEpithet[tf])] <- "SPECIES"
  }

  temp <- df %>%
    dplyr::filter(
      grepl("aceae$", genus),
      is.na(scientificNameAuthorship)
    )
  if (nrow(temp) > 0) {
    tf <- df$occurrenceID %in% temp$occurrenceID
    df$family[tf] <- temp$genus
    df$genus[tf] <- NA
    df$specificEpithet[tf] <- NA
  }

  temp <- df %>%
    dplyr::filter(
      grepl("aceae", genus),
      is.na(scientificNameAuthorship)
    )
  if (nrow(temp) > 0) {
    tf <- df$occurrenceID %in% temp$occurrenceID
    df$family[tf] <- gsub("\\s.*", "", temp$genus)
    temp$genus <- sub("^\\S+\\s+", "", temp$genus)
    df$genus[tf] <- gsub("\\s.*", "", temp$genus)

    df$specificEpithet[tf] <- .firstLower(sub("^\\S+\\s+(\\S+).*", "\\1", temp$genus))
    df$infraspecificEpithet[tf] <- NA

    if (any(is.na(temp$specificEpithet))) {
      df$specificEpithet[which(tf)[is.na(temp$specificEpithet)]] <- NA
    }

    df$taxonRank[tf][is.na(df$specificEpithet[tf])] <- "GENUS"
    df$taxonRank[tf][!is.na(df$specificEpithet[tf])] <- "SPECIES"
  }

  #_____________________________________________________________________________
  # $genus cleaning ####
  temp <- df %>%
    dplyr::filter(
      grepl("aceae", genus)
    )
  if (nrow(temp) > 0) {
    tf <- df$occurrenceID %in% temp$occurrenceID
    df$genus[tf] <- gsub("aceae", "a", temp$genus)
  }

  pattern <- c("^Indeterminad|Indet[.]")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- NA
    df$specificEpithet[tf] <- NA
    df$infraspecificEpithet[tf] <- NA
    if (any(!is.na(df$family[tf]))) {
      df$taxonRank[tf][!is.na(df$family[tf])] <- "FAMILY"
    }
  }

  pattern <- c("Leguminosae$|Guttiferae$|Compositae$|Cruciferae$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- NA
    df$specificEpithet[tf] <- NA
  }

  pattern <- c("Leguminosae")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$family[tf] <- "Leguminosae"
    df$genus[tf] <- .firstUp(df$specificEpithet[tf])
    df$specificEpithet[tf] <- df$infraspecificEpithet[tf]
    df$infraspecificEpithet[tf] <- NA
    index <- which(grepl("Leguminosae", df$genus[tf]))
    if (length(index) > 0) {
      df$genus[tf][index] <- .firstUp(df$specificEpithet[tf][index])
      df$specificEpithet[tf][index] <- NA
      df$taxonRank[tf][index] <- "GENUS"
    }
    df$taxonRank[tf][df$taxonRank[tf] != "GENUS"] <- "SPECIES"
  }

  pattern <- c("Bri\\u00f3fita$|Lichen$|Briophyta$|Lichenes|Fungos$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$division[tf] <- df$genus[tf]
    df$genus[tf] <- NA
    df$taxonRank[tf] <- "DIVISION"
    index <- which(grepl("aceae$", df$family[tf]))
    if (length(index) > 0) {
      df$taxonRank[tf][index] <- "FAMILY"
      df$family[tf][!grepl("aceae$", df$family[tf])] <- NA
    }
  }

  temp <- df %>%
    dplyr::filter(
      grepl("aceae", specificEpithet),
      is.na(family)
    )
  if (nrow(temp) > 0) {
    tf <- df$occurrenceID %in% temp$occurrenceID
    # remove everything after the second space with removing of trailing space)
    temp$specificEpithet <- sub("^((\\S+\\s+){2})\\S+.*", "\\1", temp$specificEpithet) |> trimws()

    df$family[tf] <- .firstUp(gsub("\\s.*", "", temp$specificEpithet))
    df$genus[tf] <- .firstUp(temp$infraspecificEpithet)
    df$specificEpithet[tf] <- temp$scientificNameAuthorship
    df$infraspecificEpithet[tf] <- NA
    df$scientificNameAuthorship[tf] <- NA

    tftf <- grepl("aceae", temp$infraspecificEpithet)
    if (any(tftf)){
      df$genus[df$occurrenceID %in% temp$occurrenceID[tftf]] <- .firstUp(sub("^\\S+\\s+", "", temp$specificEpithet[tftf]))
    }

    df$taxonRank[tf][is.na(df$specificEpithet[tf])] <- "GENUS"
    df$taxonRank[tf][!is.na(df$specificEpithet[tf])] <- "SPECIES"
  }

  temp <- df %>%
    dplyr::filter(
      grepl("aceae", infraspecificEpithet),
      is.na(genus)
    )
  if (nrow(temp) > 0) {
    tf <- df$occurrenceID %in% temp$occurrenceID

    df$family[tf] <- .firstUp(temp$infraspecificEpithet)
    df$genus[tf] <- temp$scientificNameAuthorship
    df$infraspecificEpithet[tf] <- NA
    df$scientificNameAuthorship[tf] <- NA
    df$taxonRank[tf] <- "GENUS"
  }

  index <- which(df$taxonRank == "SUBFAMILY" & !is.na(df$genus))
  if (length(index) > 0) {
    df$taxonRank[index] <- "GENUS"
  }

  tf <- grepl("aceae", df$scientificNameAuthorship)
  if (any(tf)) {
    df$scientificNameAuthorship[tf] <- NA
  }

  pattern <- c("^Sp$|^sp$|^sp[.]$|^Sp[.]$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- NA
    df$taxonRank[tf] <- "FAMILY"
  }

  pattern <- c("Sp$|sp$|sp[.]$|Sp[.]$|sp[.]\\snov[.]|Sp[.]\\sNov[.]")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- gsub("\\s.*", "", df$genus[tf])
    df$specificEpithet[tf] <- NA
    df$taxonRank[tf] <- "GENUS"
  }

  pattern <- c("^[?]")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- NA
    df$taxonRank[tf] <- "FAMILY"
  }

  #_____________________________________________________________________________
  # $specificEpithet cleaning ####
  pattern <- c("^Sp$|^sp$|^sp[.]$|^Sp[.]$|^sp[.]\\snov[.]|^Sp[.]\\sNov[.]")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$specificEpithet, pattern), FALSE)
  if (any(tf)) {
    df$specificEpithet[tf] <- NA
    df$taxonRank[tf] <- "GENUS"
  }

  pattern <- c("Sp$|sp$|sp[.]$|Sp[.]$|sp[.]\\snov[.]|Sp[.]\\sNov[.]")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$specificEpithet, pattern), FALSE)
  if (any(tf)) {
    df$specificEpithet[tf] <- gsub("\\s.*", "", df$specificEpithet[tf])
    df$specificEpithet[tf] <- gsub("sp[.]|subsp[.]", NA, df$specificEpithet[tf])
  }

  index <- which(df$taxonRank == "DIVISION" & !is.na(df$genus))
  tf <- grepl("phyta$", df$genus[index])
  if (any(tf)) {
    df$division[index][tf] <- df$genus[index][tf]
    df$genus[index][tf] <- NA
  }

  tf <- is.na(df$genus) & df$taxonRank %in% "GENUS"
  if (any(tf)) {
    df$taxonRank[tf] <- NA
  }

  pattern <- c("ales$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$order[tf] <- df$family[tf]
    df$family[tf] <- NA
    index <- which(tf)
    tf <- df$taxonRank[index] %in% "FAMILY"
    if (any(tf)) {
      df$taxonRank[index[tf]] <- "ORDER"
    }
  }

  pattern <- c("phyceae$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$class[tf] <- df$family[tf]
    df$family[tf] <- NA
    index <- which(tf)
    tf <- df$taxonRank[index] %in% "FAMILY"
    if (any(tf)) {
      df$taxonRank[index[tf]] <- "CLASS"
    }
  }

  pattern <- c("phyta$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$division[tf] <- df$family[tf]
    df$family[tf] <- NA
    index <- which(tf)
    tf <- df$taxonRank[index] %in% "FAMILY"
    if (any(tf)) {
      df$taxonRank[index[tf]] <- "DIVISION"
    }
  }

  tf <- df$division %in% c("Bri\u00f3fita", "Briophyta")
  if (any(tf)) {
    df$division[tf] <- "Bryophyta"
  }

  tf <- df$division %in% c("Pterydophyta")
  if (any(tf)) {
    df$division[tf] <- "Pteridophyta"
  }

  pattern <- c("Pterid.*ta$|Pterid.*phyta$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$genus, pattern), FALSE)
  if (any(tf)) {
    df$genus[tf] <- NA
    df$division[tf] <- "Pterydophyta"
    df$taxonRank[tf] <- "DIVISION"
  }

  tf <- grepl("Pterid.*ta$|Pterid.*phyta$|Samambaia|Pteridophyte", df$family) & df$taxonRank %in% "FAMILY"
  if (any(tf)) {
    df$family[tf] <- NA
    df$division[tf] <- "Pterydophyta"
    df$taxonRank[tf] <- "DIVISION"
  }

  tf <- df$division %in% c("Fungos") & is.na(df$genus)
  if (any(tf)) {
    df$division[tf] <- NA
    df$kingdom[tf] <- "Fungi"
    df$taxonRank[tf] <- "KINGDOM"
    df$family[tf] <- NA
  }

  tf <- grepl("Fungos|Fungi|Fungae|Unk[.]fungus|fungi$|Fungae", df$family) & is.na(df$genus)
  if (any(tf)) {
    df$division[tf] <- NA
    df$kingdom[tf] <- "Fungi"
    df$taxonRank[tf] <- "KINGDOM"
    df$family[tf] <- NA
  }

  tf <- grepl("Fungos|Fungi|Fungae|Unk[.]fungus|fungi$|Fungae", df$family) & !is.na(df$genus)
  if (any(tf)) {
    df$division[tf] <- NA
    df$kingdom[tf] <- "Fungi"
    df$family[tf] <- NA
  }

  tf <- grepl("Basidiom", df$family)
  if (any(tf)) {
    df$kingdom[tf] <- "Fungi"
    df$division[tf] <- "Basidiomycota"
    df$family[tf] <- NA
  }

  tf <- df$division %in% c("Lichenes", "Lichen") & is.na(df$genus)
  if (any(tf)) {
    df$kingdom[tf] <- "Fungi"
    df$division[tf] <- "Ascomycota"
    df$taxonRank[tf] <- "DIVISION"
    df$family[tf] <- NA
  }

  tf <- df$family %in% c("Lichenes", "Lichen", "Liquem", "Liquen", "Ascomycota",
                         "Unk.ascomycete") & is.na(df$genus)
  if (any(tf)) {
    df$kingdom[tf] <- "Fungi"
    df$family[tf] <- NA
    df$division[tf] <- "Ascomycota"
    df$taxonRank[tf] <- "DIVISION"
  }

  tf <- df$family %in% c("Lichenes", "Lichen", "Liquem", "Liquen", "Ascomycota",
                         "Unk.ascomycete") & !is.na(df$genus)
  if (any(tf)) {
    df$kingdom[tf] <- "Fungi"
    df$family[tf] <- NA
    df$division[tf] <- "Ascomycota"
    df$taxonRank[tf] <- "DIVISION"
  }

  pattern <- c("Angiosperm|Dicotyled|Dicot$")
  tf <- tidyr::replace_na(stringi::stri_detect_regex(df$family, pattern), FALSE)
  if (any(tf)) {
    df$division[tf] <- "Angiospermae"
    df$family[tf] <- NA
    df$genus[tf] <- NA
    df$taxonRank[tf] <- "DIVISION"
  }

  # tf <- grepl("aceae$|Leguminosae|Compositae|Palmae|Cruciferae|Labiatae|Umbelliferae|Guttiferae", df$family)
  # sort(unique(df$family[!tf]))
  # index <- which(!is.na(df$genus[!tf]))
  # unique(df$family[!tf][index])

  # Clean names within specificEpithet
  # pattern <- c("\\s")
  # tf <- tidyr::replace_na(stringi::stri_detect_regex(df$specificEpithet, pattern), FALSE)
  # if (any(tf)) {
  #   df$specificEpithet[tf] <- gsub("\\s.*", "", df$specificEpithet[tf])
  #   df$specificEpithet[tf] <- gsub("sp[.]|subsp[.]", NA, df$specificEpithet[tf])
  # }

  tf <- df$basisOfRecord %in% "PreservedSpecimen"
  if (any(tf)) {
    df$basisOfRecord[tf] <- "PRESERVED_SPECIMEN"
  }
  tf <- df$basisOfRecord %in% "FossilSpecimen"
  if (any(tf)) {
    df$basisOfRecord[tf] <- "FOSSIL_SPECIMEN"
  }

  df <- .fill_species_name(df)
  df <- .fill_taxon_name(df)
  df <- .fill_scientific_name(df)

  return(df)
}

.firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

.firstLower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

.fill_species_name <- function(df) {
  df$species <- NA_character_

  is_species <- df$taxonRank %in% "SPECIES" & !is.na(df$family)
  if (any(is_species)) {
    df$species[is_species] <- paste(df$genus[is_species], df$specificEpithet[is_species])
  }

  return(df)
}

.fill_taxon_name <- function(df) {
  df$taxonName <- NA_character_

  is_kingdom <- df$taxonRank %in% "KINGDOM" & !is.na(df$kingdom)
  if (any(is_kingdom)) {
    df$taxonName[is_kingdom] <- df$kingdom[is_kingdom]
  }

  is_division <- df$taxonRank %in% "DIVISION" & !is.na(df$division)
  if (any(is_division)) {
    df$taxonName[is_division] <- df$division[is_division]
  }

  is_class <- df$taxonRank %in% "CLASS" & !is.na(df$class)
  if (any(is_class)) {
    df$taxonName[is_class] <- df$class[is_class]
  }

  is_order <- df$taxonRank %in% "ORDER" & !is.na(df$order)
  if (any(is_order)) {
    df$taxonName[is_order] <- df$order[is_order]
  }

  is_family <- df$taxonRank %in% "FAMILY" & !is.na(df$family)
  if (any(is_family)) {
    df$taxonName[is_family] <- df$family[is_family]
  }

  is_taxon <- !is_kingdom & !is_division & !is_class & !is_order & !is_family
  if (any(is_taxon)) {
    df$taxonName[is_taxon] <- paste(
      ifelse(!is.na(df$genus[is_taxon]), df$genus[is_taxon], ""),
      ifelse(!is.na(df$specificEpithet[is_taxon]), df$specificEpithet[is_taxon], ""),
      ifelse(!is.na(df$infraspecificEpithet[is_taxon]), df$infraspecificEpithet[is_taxon], "")
    ) |>
      trimws()
  }

  return(df)
}

.fill_scientific_name <- function(df) {
  df$scientificName <- NA_character_

  # Abbreviation lookup
  rank_abbr <- function(rank) {
    if (is.na(rank)) return("")
    switch(rank,
           "VARIETAS" = "var.",
           "SUBSPECIES" = "subsp.",
           "FORMA" = "f.",
           "")
  }

  # Vectorized abbreviation mapping
  ranks <- vapply(df$taxonRank, rank_abbr, character(1))

  # Build name base
  name_base <- ifelse(
    !is.na(df$genus) & !is.na(df$specificEpithet),
    paste(df$genus, df$specificEpithet),
    df$genus
  )

  # Skip duplication if specific == infraspecific
  same_epi <- !is.na(df$specificEpithet) & !is.na(df$infraspecificEpithet) &
    df$specificEpithet == df$infraspecificEpithet

  add_infra <- !is.na(df$infraspecificEpithet) & ranks != "" & !same_epi

  name_base[add_infra] <- paste(
    name_base[add_infra],
    ranks[add_infra],
    df$infraspecificEpithet[add_infra]
  )

  df$scientificName <- trimws(name_base)

  # Add authorship before infraspecific when same
  has_auth <- !is.na(df$scientificNameAuthorship) & df$scientificNameAuthorship != ""
  has_same <- same_epi & has_auth

  df$scientificName[has_same] <- paste(
    name_base[has_same],
    df$scientificNameAuthorship[has_same],
    ranks[has_same],
    df$infraspecificEpithet[has_same]
  )

  # Regular authorship placement otherwise
  regular_auth <- has_auth & !has_same
  df$scientificName[regular_auth] <- paste(
    df$scientificName[regular_auth],
    df$scientificNameAuthorship[regular_auth]
  )

  df$scientificName <- trimws(df$scientificName)
  return(df)
}


#_______________________________________________________________________________
# Extract each "occurrence.txt" data frame and merge them ####

.merge_occur_txt <- function(dwca_files,
                             verbose = verbose) {
  occur_df <- dplyr::bind_rows(lapply(dwca_files, function(x) {

    df <- x[["data"]][["occurrence.txt"]]

    df$recordNumber <- suppressWarnings(as.character(df$recordNumber))
    df$decimalLongitude <- suppressWarnings(as.numeric(df$decimalLongitude))
    df$decimalLatitude <- suppressWarnings(as.numeric(df$decimalLatitude))
    df$verbatimLatitude <- as.character(df$verbatimLatitude)
    df$verbatimLongitude <- as.character(df$verbatimLongitude)
    df$occurrenceID <- as.character(df$occurrenceID)
    df$recordNumber <- as.character(df$recordNumber)
    df$minimumElevationInMeters <- as.character(df$minimumElevationInMeters)
    df$maximumElevationInMeters <- as.character(df$maximumElevationInMeters)
    df$eventDate <- as.character(df$eventDate)
    df$year <- as.character(df$year)
    df$month <- as.character(df$month)
    df$day <- as.character(df$day)

    return(df)
  }))

  return(occur_df)
}


#_______________________________________________________________________________
# Function to filter occurrence data ####

.filter_occur_df <- function(occur_df, taxon, state, recordYear, verbose) {

  temp_occur_df <- data.frame(matrix(ncol = length(names(occur_df)), nrow = 0))
  colnames(temp_occur_df) <- names(occur_df)

  #_____________________________________________________________________________
  # Filter by taxon only

  if (!is.null(taxon)) {
    if (verbose) {
      message("\nFiltering taxon names... ")
    }

    .check_taxon_match(occur_df, taxon, verbose)

    tf_fam <- grepl("aceae$", taxon)
    if (any(tf_fam)) {
      taxon_fam <- taxon[tf_fam]
      tf <- occur_df$family %in% taxon_fam
      if (any(tf)) {
        occur_df_fam <- occur_df[tf, ]
        temp_occur_df <- occur_df_fam
      }
    }

    tf_gen <- grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon)
    if (any(tf_gen)) {
      taxon_gen <- taxon[tf_gen]
      tf <- occur_df$genus %in% taxon_gen
      if (any(tf)) {
        occur_df_gen <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_gen)
      }
    }

    tf_spp <- grepl("\\s", taxon)
    if (any(tf_spp)) {
      taxon_spp <- taxon[tf_spp]
      tf <- occur_df$taxonName %in% taxon_spp
      if (any(tf)) {
        occur_df_spp <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_spp)
      }
    }

    if (nrow(temp_occur_df) != 0){
      occur_df <- temp_occur_df
    }

  }

  #_____________________________________________________________________________
  # Filter by state only

  if (!is.null(state)) {
    if (verbose) {
      message("\nFiltering states... ")
    }

    .check_state_match(occur_df, state, verbose)

    tf <- occur_df$stateProvince %in% state
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  #_____________________________________________________________________________
  # Filter by record year only

  if (!is.null(recordYear)) {
    if (verbose) {
      message("\nFiltering recordYear... ")
    }

    .check_year_match(occur_df, recordYear, verbose)

    if (length(recordYear) == 1) {
      # If only one year is given, filter for that specific year
      occur_df <- occur_df %>% dplyr::filter(year == recordYear)
    } else if (length(recordYear) == 2) {
      # If a range is given, filter for records within that range
      occur_df <- occur_df %>%
        dplyr::filter(!is.na(occur_df$year) &
                        occur_df$year >= as.numeric(recordYear[1]) &
                        occur_df$year <= as.numeric(recordYear[2]))
    }
  }
  return(occur_df)
}

.check_taxon_match <- function(occur_df, taxon, verbose) {

  all_names <- unique(c(occur_df$family, occur_df$genus, occur_df$taxonName))
  matched_taxa <- taxon[taxon %in% all_names]
  unmatched_taxa <- setdiff(taxon, matched_taxa)

  if (verbose) {
    if (length(unmatched_taxa) > 0) {
      message("The following taxa were not found in any column: ", paste(unmatched_taxa, collapse = ", "))
    }
  }
  matches <- occur_df$family %in% matched_taxa |
    occur_df$genus %in% matched_taxa |
    occur_df$taxonName %in% matched_taxa

  if (!any(matches)) {
    stop(paste0(
      "Your input 'taxon' list must contain at least one name existing within the REFLORA collections.\n",
      "Check whether the input taxon list has any typo: ",
      paste(unmatched_taxa, collapse = ", ")
    ))
  }
}

.check_state_match <- function(occur_df, state, verbose) {

  matched_state <- state[state %in% unique(occur_df$stateProvince)]
  unmatched_state <- setdiff(state, matched_state)

  if (verbose && length(unmatched_state) > 0) {
    message("The following states were not found: ", paste(unmatched_state, collapse = ", "))
  }

  if (length(matched_state) == 0) {
    stop(paste0(
      "Your input 'state' list must contain at least one name existing within the REFLORA collections.\n",
      "Check whether the input state list has any typo: ",
      paste(unmatched_state, collapse = ", ")
    ))
  }
}

.check_year_match <- function(occur_df, recordYear, verbose) {

  matched_year <- recordYear[recordYear %in% unique(occur_df$year)]
  unmatched_year <- setdiff(recordYear, matched_year)

  if (verbose && length(unmatched_year) > 0) {
    message("The following recordYear were not found: ", paste(unmatched_year, collapse = ", "))
  }

  if (length(matched_year) == 0) {
    stop(paste0(
      "Your input 'recordYear' list must contain at least one year existing within the REFLORA collections.\n",
      "Check whether the input recordYear list has any typo: ",
      paste(unmatched_year, collapse = ", ")
    ))
  }
}


#_______________________________________________________________________________
# Function to save csv files ####

.save_csv <- function(df,
                      verbose = TRUE,
                      filename = NULL,
                      dir = dir) {

  # Save the data frame if param save is TRUE
  # Create a new directory to save the results with current date
  # If there is no directory... make one!

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  filename <- paste0(filename, ".csv")
  # Create and save the spreadsheet in .csv format
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  utils::write.csv(df, file = paste0(dir, "/", filename), row.names = FALSE)
}


#_______________________________________________________________________________
# Function to save log.txt file ####

.save_log <- function(df,
                      herbarium = NULL,
                      filename = NULL,
                      dir = dir) {

  log_line <- sprintf("[%s] Downloaded: %s | Records saved to: %s/%s.csv\n",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      ifelse(is.null(herbarium), "ALL", paste(herbarium, collapse = ", ")),
                      dir,
                      filename)

  # Add summary statistics
  count_total <- nrow(df)
  by_herbarium <- capture.output(print(table(df$collectionCode)))
  by_family <- capture.output(print(table(df$family)))
  by_genus <- capture.output(print(table(df$genus)))
  by_country <- capture.output(print(table(df$country)))
  by_state <- capture.output(print(table(df$stateProvince)))

  stats_summary <- c(
    sprintf("Total records: %d", count_total),
    "\nRecords per herbarium:", by_herbarium,
    "\nRecords per family:", by_family,
    "\nRecords per genus:", by_genus,
    "\nRecords per country:", by_country,
    "\nRecords per stateProvince:", by_state,
    "--------------------------------------------------\n"
  )

  write(c(log_line, stats_summary), file = file.path(dir, "log.txt"), append = TRUE)

}
