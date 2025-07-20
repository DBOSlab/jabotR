# Auxiliary functions to support main functions
# Author: Domingos Cardoso


#_______________________________________________________________________________
### Function to get raw metadata from JABOT repository ###

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
### Function to get summary information of each JABOT-associated collection ###

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
### Function to reorder retrieved data based on specific columns ###

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
### Extract each "occurrence.txt" data frame and merge them ###
.merge_occur_txt <- function(dwca_files) {
  dfs <- lapply(dwca_files, function(x) {
    df <- x[["data"]][["occurrence.txt"]]

    df$recordNumber <- suppressWarnings(as.character(df$recordNumber))
    df$decimalLongitude <- suppressWarnings(as.numeric(df$decimalLongitude))
    df$decimalLatitude <- suppressWarnings(as.numeric(df$decimalLatitude))

    df$occurrenceID <- as.character(df$occurrenceID)
    df$minimumElevationInMeters <- as.character(df$minimumElevationInMeters)
    df$maximumElevationInMeters <- as.character(df$maximumElevationInMeters)
    df$eventDate <- as.character(df$eventDate)
    df$year <- as.character(df$year)
    df$month <- as.character(df$month)
    df$day <- as.character(df$day)
    df$institutionCode <- as.character(df$institutionCode)
    df$collectionCode <- as.character(df$collectionCode)
    df$catalogNumber <- as.character(df$catalogNumber)
    df$taxonRank <- as.character(df$taxonRank)
    df$specificEpithet <- as.character(df$specificEpithet)
    df$infraspecificEpithet <- as.character(df$infraspecificEpithet)
    df$scientificNameAuthorship <- as.character(df$scientificNameAuthorship)
    df$scientificName <- as.character(df$scientificName)

    return(as.data.frame(df))  # Ensure it's a proper data frame
  })

  occur_df <- dplyr::bind_rows(dfs)  # Handles different columns by filling NA

  occur_df <- .std_inside_columns(occur_df)

  return(occur_df)
}

.std_inside_columns <- function(occur_df) {

  # Standardize and clean taxonRank column
  taxonrank_infr <- c("Infr.", "infr.", "infraspecific")
  taxonrank_form <- c("f.", "form", "forma", "FORM")
  taxonrank_var <- c("var.", "VAR.", "variedade", "VARIEDADE", "VARIETY", "variety")
  taxonrank_subsp <- c("subsp.", "ssp.", "SUBSP.", "SUBSP")
  taxonrank_species <- c("sp", "sp.", "especie", "ESPECIE", "espécie", "ESPÊCIE", "species", "specie", "SPECIES", "SPECIE")
  taxonrank_genus <- c("genero", "GENERO", "gênero", "GÊNERO", "gen.", "genus", "GENUS")
  taxonrank_tribe <- c("tribo", "TRIBO", "tribe","TRIBE")
  taxonrank_subfam <- c("sub_familia", "SUB_FAMILIA", "subfamily", "SUBFAMILY")
  taxonrank_family <- c("fam.", "família","FAMÍLIA", "familia", "FAMILIA", "family", "FAMILY")
  taxonrank_order <- c("ordem", "ORDEM","order", "ORDER")
  taxonrank_class <- c("classe", "CLASSE", "class", "CLASS")
  taxonrank_division <- c("divisao", "DIVISAO", "divisão", "DIVISÃO", "division", "DIVISION")

  occur_df <- .replace_taxonrank(occur_df, taxonrank_infr, "infraspecific")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_form, "forma")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_subsp, "subspecies")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_var, "varietas")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_species, "species")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_genus, "genus")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_tribe, "tribe")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_subfam, "subfamily")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_family, "family")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_order, "order")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_class, "class")
  occur_df <- .replace_taxonrank(occur_df, taxonrank_division, "division")

  # Remove scientific names erroneously added into the taxonRank column
  taxonranks <- c("infraspecific", "forma", "subspecies", "varietas",
                  "species", "genus", "tribe", "subfamily",
                  "family", "order", "class", "division")
  n_diff <- setdiff(occur_df$taxonRank, taxonranks)
  if (length(n_diff > 0)) {
    tf <- grepl("aceae$|ACEAE", n_diff)
    if (any(tf)) {
      occur_df$taxonRank[occur_df$taxonRank %in% n_diff[tf]] <- "family"
    }
    tf <- grepl("[[:lower:]]\\s[[:lower:]]", n_diff)
    if (any(tf)) {
      occur_df$taxonRank[occur_df$taxonRank %in% n_diff[tf]] <- "species"
    }
    occur_df$taxonRank[occur_df$taxonRank %in% n_diff] <- "genus"
  }

  #_____________________________________________________________________________
  # Finding errors within family column ####
  # tf <- grepl("aceae$|Leguminosae|Compositae|Palmae|Cruciferae|Labiatae|Umbeliferae|Guttiferae",
  #             occur_df$family)
  # sort(unique(occur_df$family[!tf]))

  tf <- grepl("acaeae$|aceaeae$", occur_df$family)
  if(any(tf)) {
    occur_df$family[tf] <- gsub("acaeae$|aceaeae$", "aceae", occur_df$family[tf])
    occur_df$family[tf] <- gsub("Olaceae", "Olacaceae", occur_df$family[tf])
  }
  tf <- grepl("acae$|ace$|acea$|adeae$|aeae$|acedae$|eceae$|aceaea$|aceac$|acieae$|aceea$|arceae$|acee$|acaee$|acese$|acaea$|acceae$|sceae$",
              occur_df$family)
  if(any(tf)) {
    occur_df$family[tf] <- gsub("acae$|ace$|acea$|adeae$|aeae$|acedae$|eceae$|aceaea$|aceac$|acieae$|aceea$|arceae$|acee$|acaee$|acese$|acaea$|acceae$|sceae$",
                          "aceae", occur_df$family[tf])
  }
  tf <- grepl("aaceae$", occur_df$family)
  if(any(tf)) {
    occur_df$family[tf] <- gsub("aa", "a", occur_df$family[tf])
    occur_df$family[tf] <- gsub("Lindsaceae", "Lindsaeaceae", occur_df$family[tf])
  }
  tf <- occur_df$family %in% c("Leg", "Fab.", "Papi.", "Papi", "Papilionoideae",
                         "Caes.", "Caesalpinioideae", "Caesalpinoideae",
                         "Mim.", "Mim", "Mimosoideae", "Dial.", "Dialioideae",
                         "Cerci.", "Cerc.", "Cercidoideae")
  if(any(tf)) {
    occur_df$family[tf] <- "Fabaceae"
  }
  tf <- occur_df$family %in% c("Incertae", "Incertaesedes", "Ignotae", "Incertae Sedis",
                         "L", "Jes", "Sem", "X", "Dicot", "Cf",
                         "Indet", "Indet.", "Indt", "Em Branco", "Det.",
                         "Indeterminada", "Indeterminado", "Angiosperma",
                         "Ordem", "Classe")
  if(any(tf)) {
    occur_df$family[tf] <- NA
    occur_df$taxonRank[tf] <- NA
  }
  tf <- grepl("lceae$|siceae$|nceae$", occur_df$family)
  if(any(tf)) {
    occur_df$family[tf] <- gsub("lceae$", "laceae", occur_df$family[tf])
    occur_df$family[tf] <- gsub("siceae$", "siaceae", occur_df$family[tf])
    occur_df$family[tf] <- gsub("nceae$", "naceae", occur_df$family[tf])
  }

  tf <- grepl("sp$|sp[.]$", occur_df$specificEpithet[occur_df$taxonRank %in% "species"])
  #sort(unique(occur_df$specificEpithet[occur_df$taxonRank %in% "species"][tf]))
  if(any(tf)) {
    occur_df$specificEpithet[occur_df$taxonRank %in% "species"][tf] <- NA
    occur_df$taxonRank[occur_df$taxonRank %in% "species"][tf] <- "genus"
  }

  tf <- grepl("aceae$", occur_df$genus[occur_df$taxonRank %in% "genus"])
  #sort(unique(occur_df$genus[occur_df$taxonRank %in% "genus"][tf]))
  if(any(tf)) {
    occur_df$genus[occur_df$taxonRank %in% "genus"][tf] <- NA
    occur_df$taxonRank[occur_df$taxonRank %in% "genus"][tf] <- "family"
  }

  return(occur_df)
}

.replace_taxonrank <- function(occur_df, taxonrank, rank) {
  tf <- occur_df$taxonRank %in% taxonrank
  if (any(tf)) {
    occur_df$taxonRank[tf] <- rank
  }
  return(occur_df)
}


#_______________________________________________________________________________
### Function to filter occurrence data ###

.filter_occur_df <- function(occur_df, taxon, state, recordYear, verbose) {

  temp_occur_df <- data.frame(matrix(ncol = length(names(occur_df)), nrow = 0))
  colnames(temp_occur_df) <- names(occur_df)

  #_____________________________________________________________________________
  # Filter by taxon only

  if (!is.null(taxon)) {
    if (verbose) {
      message("\nFiltering taxon names... ")
    }

    tf <- grepl("aceae$", taxon)
    if (any(tf)) {
      taxon <- append(taxon, toupper(taxon[tf]))
    }

    .check_taxon_match(occur_df, taxon, verbose)

    tf_fam <- grepl("aceae$|ACEAE$", taxon)
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
      occur_df <- occur_df[occur_df$year == recordYear, ]
    } else if (length(recordYear) == 2) {
      # If a range is given, filter for records within that range
      occur_df <- occur_df[!is.na(occur_df$year) &
                             occur_df$year >= as.numeric(recordYear[1]) &
                             occur_df$year <= as.numeric(recordYear[2]), ]
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
  matches <- any(matches)

  if (!matches) {
    stop(paste0(
      "Your input 'taxon' list must contain at least one name existing within the JABOT collections.\n",
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
      "Your input 'state' list must contain at least one name existing within the JABOT collections.\n",
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
      "Your input 'recordYear' list must contain at least one year existing within the JABOT collections.\n",
      "Check whether the input recordYear list has any typo: ",
      paste(unmatched_year, collapse = ", ")
    ))
  }
}


#_______________________________________________________________________________
### Function to save csv files ###

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
### Function to save log.txt file ###

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
