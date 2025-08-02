#' Parse Darwin Core Archive files
#'
#' @author Domingos Cardoso
#'
#' @description Read Darwin Core Archive (DwC-A) files from any downloaded dwca
#' folder at \href{https://ipt.jbrj.gov.br/jabot}{JABOT} hosted by the
#' \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' jabot_parse(path = NULL,
#'             herbarium = NULL,
#'             verbose = TRUE)
#'
#' @param path Pathway to the computer's directory, where the REFLORA-downloaded
#' dwca folders are.
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as \code{NULL} to parse specimen records for all
#' herbarim dcwa folders in the defined path directory.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when
#' summarizing specimen records will not be printed in the console in full.
#'
#' @return A list of dwca files, including data and associated metada.
#'
#' @seealso \code{\link{jabot_download}}
#'
#' @examples
#' \dontrun{
#'
#' jabot_download(herbarium = c("ALCB", "HUEFS", "K", "RB"),
#'                verbose = TRUE,
#'                dir = "jabot_download")
#'
#' dwca <- jabot_parse(path = "jabot_download",
#'                     verbose = TRUE)
#'}
#'
#' @importFrom finch dwca_read
#' @importFrom utils read.csv
#' @importFrom stringr str_extract str_to_title
#' @importFrom stringi stri_detect_regex
#' @importFrom dplyr filter mutate select recode
#' @importFrom tidyr replace_na
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames
#'
#' @export
#'

jabot_parse <- function(path = NULL,
                        herbarium = NULL,
                        verbose = TRUE) {

  dwca_folders <- list.files(path)
  dwca_filenames <- lapply(paste0(path, "/", dwca_folders), list.files)

  if (!is.null(herbarium)) {
    current_herbarium <- toupper(stringr::str_extract(dwca_folders,
                                                      "(?<=dwca[-_])[^-_]+"))
    current_herbarium <- gsub("JBRJ", "RB", current_herbarium)
    dwca_folders <- dwca_folders[current_herbarium %in% herbarium]
    dwca_filenames <- dwca_filenames[current_herbarium %in% herbarium]
  }

  # path check
  .arg_check_path(path, dwca_folders, dwca_filenames)

  # Calling all dwca files

  if (verbose) {
    message("Parsing data from dwca folders...\n\n")
  }
  dwca_files <- lapply(dwca_folders,
                       function(x) finch::dwca_read(input = paste0(path, "/", x),
                                                    read = TRUE,
                                                    encoding = "UTF-8",
                                                    na.strings = ""))

  # Order and clean specific columns within dwca_files
  fields <- c(
    "occurrenceID",
    "institutionCode",
    "collectionCode",
    "catalogNumber",
    "taxonRank",
    "kingdom",
    "division",
    "class",
    "order",
    "family",
    "genus",
    "specificEpithet",
    "species",
    "infraspecificEpithet",
    "taxonName",
    "scientificNameAuthorship",
    "scientificName",
    "recordedBy",
    "recordNumber",
    "fieldNumber",
    "eventDate",
    "year",
    "month",
    "day",
    "fieldNotes",
    "occurrenceRemarks",
    "eventRemarks",
    "country",
    "countryCode",
    "stateProvince",
    "municipality",
    "locality",
    "minimumElevationInMeters",
    "maximumElevationInMeters",
    "decimalLatitude",
    "decimalLongitude",
    "verbatimLatitude",
    "verbatimLongitude",
    "identificationQualifier",
    "typeStatus",
    "identifiedBy",
    "dateIdentified",
    "identificationRemarks",
    "basisOfRecord"
  )

  for (i in seq_along(dwca_files)) {

    temp <- dwca_files[[i]][["data"]][["occurrence.txt"]]

    fields <- fields[fields %in% names(temp)]
    pos <- match("specificEpithet", fields)
    fields <- append(fields, "species", after = pos)
    pos <- match("infraspecificEpithet", fields)
    fields <- append(fields, "taxonName", after = pos)

    temp <- temp %>%
      dplyr::mutate(
        family = stringr::str_to_title(family),
        genus = stringr::str_to_title(genus),
        species = NA_character_,
        taxonName = NA_character_
      ) %>%
      dplyr::select(all_of(fields)) %>%
      dplyr::mutate(taxonRank = tidyr::replace_na(taxonRank, "FAMILY")) %>%
      dplyr::mutate(kingdom = if (!"kingdom" %in% names(.)) NA else kingdom,
                    .after = taxonRank) %>%
      dplyr::mutate(division = if (!"division" %in% names(.)) NA else division,
                    .after = kingdom) %>%
      dplyr::mutate(class = if (!"class" %in% names(.)) NA else class,
                    .after = division) %>%
      dplyr::mutate(order = if (!"order" %in% names(.)) NA else order,
                    .after = class)

    temp <- .std_inside_columns(temp, verbose = verbose)
    dwca_files[[i]][["data"]][["occurrence.txt"]] <- temp
  }

  # Parsing csv files, if they exist
  tf <- lapply(dwca_filenames, function(x) grepl("[.]csv$", x))
  tf_csv <- unlist(lapply(tf, any))

  if (any(tf_csv)) {
    csv_filenames <- unlist(Map(`[`, dwca_filenames, tf)[tf_csv])
    dwca_folders = dwca_folders[tf_csv]
    csv_df <- lapply(seq_along(csv_filenames),
                     function(i) utils::read.csv(paste0(path,
                                                        "/",
                                                        dwca_folders[i],
                                                        "/",
                                                        csv_filenames[i]),
                                                 na.strings = ""))

    name <- paste0("summary_",
                   unlist(lapply(seq_along(csv_df),
                                 function(i) csv_df[[i]]$collectionCode)))

    # Use mapply to add each summary data frame to each dwca_files element
    dwca_files[tf_csv] <- mapply(function(x, y, name) {
      x[[name]] <- y
      x
    }, dwca_files[tf_csv], csv_df, name, SIMPLIFY = FALSE)

  }

  # Name the list with uploaded dwca files
  temp <- unlist(lapply(seq_along(dwca_files),
                        function(i) dwca_files[[i]][["files"]][["xml_files"]][1]))
  names(dwca_files) <- gsub(".*[/]", "", gsub("[/]eml.*", "", temp))


  if (verbose) {
    message("Collections and associated metadata were parsed from the following dwca folders: \n\n",
            paste0(names(dwca_files), "\n"))
  }

  return(dwca_files)
}
