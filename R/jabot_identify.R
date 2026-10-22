#' Suggest identifications for indeterminate specimens from duplicates in JABOT and REFLORA
#'
#' @author
#' Domingos Cardoso
#'
#' @description
#' Looks for identifications for the indeterminate specimens of a herbarium
#' hosted in the \href{https://ipt.jbrj.gov.br/jabot}{JABOT} network or in the
#' \href{https://ipt.jbrj.gov.br/reflora/}{Reflora Virtual Herbarium}. A specimen
#' is considered indeterminate when it lacks the family, the genus or the
#' specific epithet. The records of the JABOT network and of REFLORA (or subsets
#' of their herbaria) are standardized with \code{barRoso::barroso_std()}, which
#' flags blocks of duplicates (same collector and collector number, or same
#' species, collector and date when the number is missing; see
#' \code{barRoso::barroso_flag_duplicates()}). For each indeterminate specimen,
#' the duplicates held by other herbaria are inspected to find out whether any
#' of them was already identified, and to which level. The results are
#' summarized in a self-contained, interactive HTML report.
#'
#' @details
#' The herbarium to be investigated (\code{herbarium}) can be any JABOT-hosted
#' herbarium or, when it is not hosted in JABOT, any herbarium available in
#' REFLORA, which also gathers Brazilian specimens held by international
#' herbaria (e.g. \code{"NY"}, \code{"K"}, \code{"P"}, \code{"US"}). When it is
#' available in both, the JABOT records are used. The duplicates are searched in
#' the other herbaria of JABOT (\code{jabot = TRUE}) and of REFLORA
#' (\code{reflora = TRUE}). The databases are first checked in
#' \code{jabot_path} and \code{reflora_path} (or in the default folders
#' \code{jabot_download} and \code{reflora_download}) and only what is missing
#' or outdated is downloaded, with \code{jabot_download()} and
#' \code{refloraR::reflora_download()}. REFLORA herbaria that are already in
#' the JABOT data are not used from REFLORA (they are not even downloaded), so
#' no specimen is counted twice, and the source of each record (JABOT or
#' REFLORA) is kept. The records are read with \code{jabot_parse()} and
#' \code{refloraR::reflora_parse()}, one herbarium at a time, keeping in memory
#' only the records that may be duplicates of an indeterminate specimen, and
#' then standardized with barRoso.
#'
#' The analysis includes the number of indeterminate specimens of the herbarium,
#' how many of them can be matched with duplicates in other herbaria, how many
#' have a duplicate with a better identification (to species, genus or family),
#' the suggested identification and its source (data source, herbarium, catalog
#' number and, when available, links to both the indeterminate specimen and the
#' duplicate that identifies it, so that both images can be compared), the
#' herbaria that most contribute with identifications, indeterminate specimens by
#' family, conflicts between the identification of the specimen and of its
#' duplicates, graphical summaries and an HTML report.
#'
#' A duplicate is considered a candidate identification when it fills at least
#' one rank missing in the indeterminate specimen (family, genus or specific
#' epithet). Candidates that disagree with ranks already present in the
#' specimen (e.g. a different family) are reported as conflicts and never
#' suggested. Only duplicates held by herbaria other than \code{herbarium} are
#' considered. Specimens without a known collector and a collector number (or,
#' for those identified at least to genus, a complete collection date) cannot be
#' matched with duplicates and are counted separately.
#'
#' The determiner (\code{identifiedBy}) and the date of identification
#' (\code{dateIdentified}) of the indeterminate specimen and of each duplicate
#' are included in the tables, to help deciding which identification to trust
#' when they conflict.
#'
#' The suggested identifications come from the records of other herbaria and
#' should be checked, preferably against the specimen images, before being
#' adopted.
#'
#' @note
#' - If \code{jabot_path = NULL} or \code{reflora_path = NULL}, the DwC-A files
#'   are downloaded into folders named \code{jabot_download} and
#'   \code{reflora_download} within your working directory, which can take a
#'   long time and a lot of disk space for the whole JABOT network and REFLORA.
#'   Supply \code{jabot_path} and \code{reflora_path} afterwards to reuse the
#'   files, and consider restricting \code{network_herbaria} and
#'   \code{reflora_herbaria}.
#'
#' - Ensure an active internet connection if downloading is required.
#'
#' - The packages \pkg{barRoso} and \pkg{refloraR} are required.
#'
#' @usage
#' jabot_identify(herbarium = NULL,
#'                taxon = NULL,
#'                jabot = TRUE,
#'                network_herbaria = NULL,
#'                reflora = TRUE,
#'                reflora_herbaria = NULL,
#'                jabot_path = NULL,
#'                reflora_path = NULL,
#'                format = NULL,
#'                fig_width = 10,
#'                fig_height = 6,
#'                open_report = TRUE,
#'                verbose = TRUE,
#'                dir = NULL)
#'
#' @param herbarium Character. A single herbarium acronym in uppercase
#' (e.g., \code{"RB"}) whose indeterminate specimens will be investigated. It
#' can be hosted in JABOT or, otherwise, in REFLORA (e.g., \code{"HUEFS"}).
#'
#' @param taxon Character vector. Family and/or genus names to restrict the
#' indeterminate specimens of \code{herbarium} (e.g., \code{"Fabaceae"} or
#' \code{c("Inga", "Mimosa")}). Specimens without any family or genus cannot
#' match a \code{taxon}. Use \code{NULL} (default) to analyse all
#' indeterminate specimens.
#'
#' @param jabot Logical. If \code{TRUE} (default), searches for duplicates in
#' the herbaria of the JABOT network.
#'
#' @param network_herbaria Character vector. JABOT herbarium acronyms to
#' restrict the JABOT records searched for duplicates. \code{NULL} (default)
#' uses all JABOT-hosted herbaria.
#'
#' @param reflora Logical. If \code{TRUE} (default), also searches for
#' duplicates in the herbaria of REFLORA, including the international ones.
#'
#' @param reflora_herbaria Character vector. REFLORA herbarium acronyms to
#' restrict the REFLORA records searched for duplicates (e.g.,
#' \code{c("NY", "K", "P", "US")}). \code{NULL} (default) uses all REFLORA
#' herbaria. Use \code{refloraR::reflora_summary()} to see them.
#'
#' @param jabot_path Character. Path to existing JABOT DwC-A files. If
#' \code{NULL} (default), the files are downloaded automatically.
#'
#' @param reflora_path Character. Path to existing REFLORA DwC-A files. If
#' \code{NULL} (default), the files are downloaded automatically.
#'
#' @param format Character vector. Static formats used to save the individual
#' figures: \code{"pdf"}, \code{"png"}, or both. The HTML report is always
#' generated. Defaults to \code{NULL} (figures are not saved separately).
#'
#' @param fig_width,fig_height Numeric. Dimensions (in inches) of saved figures.
#'
#' @param open_report Logical. If \code{TRUE} (default), opens the HTML report
#' in the default browser after rendering (interactive sessions only).
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param dir Character. Output directory. Defaults to
#' \code{"<herbarium>_identify"}.
#'
#' @return Invisibly, a named list with the computed objects, among them
#' \code{indets} (one row per indeterminate specimen, with the outcome and the
#' suggested identification), \code{suggestions} (one row per specimen and
#' duplicate held by another herbarium), the summary tables and the figures.
#' The HTML report and two CSV files (\code{<herbarium>_identify_indets.csv} and
#' \code{<herbarium>_identify_suggestions.csv}) are saved in \code{dir}.
#'
#' @seealso \code{\link{jabot_indets}}
#' @seealso \code{\link{jabot_records}}
#' @seealso \code{refloraR::reflora_download()}, \code{refloraR::reflora_parse()}
#'
#' @examples
#' \dontrun{
#' # All indeterminate specimens of RB, compared with JABOT and REFLORA
#' jabot_identify(herbarium = "RB")
#'
#' # Only indeterminate Fabaceae and Inga, reusing downloaded files
#' jabot_identify(herbarium = "RB",
#'                taxon = c("Fabaceae", "Inga"),
#'                jabot_path = "jabot_download",
#'                reflora_path = "reflora_download")
#'
#' # Herbarium available only in REFLORA, compared with international herbaria
#' jabot_identify(herbarium = "HUEFS",
#'                jabot = FALSE,
#'                reflora_herbaria = c("NY", "K", "P", "US"))
#'
#' # Restrict the herbaria searched for duplicates
#' jabot_identify(herbarium = "RB",
#'                network_herbaria = c("MBM", "UPCB", "FURB"),
#'                reflora_herbaria = c("NY", "HUEFS"),
#'                jabot_path = "jabot_download",
#'                reflora_path = "reflora_download")
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise arrange desc n_distinct left_join bind_rows distinct
#' @importFrom magrittr "%>%"
#' @importFrom utils head browseURL write.csv
#' @importFrom stats reorder
#' @importFrom scales comma
#' @importFrom rmarkdown render
#' @importFrom barRoso barroso_std
#' @importFrom refloraR reflora_download reflora_parse reflora_summary
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs scale_fill_manual
#' @importFrom DT datatable
#' @importFrom stringi stri_trans_general
#'
#' @export

jabot_identify <- function(herbarium = NULL,
                           taxon = NULL,
                           jabot = TRUE,
                           network_herbaria = NULL,
                           reflora = TRUE,
                           reflora_herbaria = NULL,
                           jabot_path = NULL,
                           reflora_path = NULL,
                           format = NULL,
                           fig_width = 10,
                           fig_height = 6,
                           open_report = TRUE,
                           verbose = TRUE,
                           dir = NULL) {

  # -- Input validation --------------------------------------------------------
  if (is.null(herbarium))
    stop("'herbarium' must be provided (e.g. herbarium = 'RB').", call. = FALSE)
  herbarium <- toupper(trimws(herbarium))
  if (length(herbarium) != 1)
    stop("'herbarium' must be a single herbarium acronym.", call. = FALSE)

  if (!is.null(taxon)) {
    taxon <- trimws(as.character(taxon))
    taxon <- taxon[!is.na(taxon) & nzchar(taxon)]
    if (length(taxon) == 0) taxon <- NULL
  }

  for (arg in c("jabot", "reflora")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1 || is.na(val))
      stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!jabot && !reflora)
    stop("At least one of 'jabot' and 'reflora' must be TRUE, to have a ",
         "source of duplicates.", call. = FALSE)

  if (!is.null(format)) {
    format <- tolower(trimws(format))
    bad <- setdiff(format, c("pdf", "png"))
    if (length(bad))
      stop("'format' must be 'pdf', 'png', or both. Unrecognised: ",
           paste(bad, collapse = ", "), call. = FALSE)
  }

  for (pkg in c("barRoso", "refloraR")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("The package '", pkg, "' is required. Install it with ",
           "remotes::install_github('DBOSlab/", pkg, "').", call. = FALSE)
  }

  # Where the herbarium is hosted: JABOT first, otherwise REFLORA
  source <- .resolve_identify_source(herbarium)

  if (!is.null(network_herbaria)) {
    network_herbaria <- toupper(trimws(network_herbaria))
    .arg_check_herbarium(network_herbaria, verbose = FALSE)
  }

  if (!is.null(reflora_herbaria)) {
    reflora_herbaria <- toupper(trimws(reflora_herbaria))
    bad <- setdiff(reflora_herbaria, .reflora_codes())
    if (length(bad))
      stop("The following acronym(s) are not recognized by REFLORA: ",
           paste(shQuote(bad), collapse = ", "),
           "\nUse `refloraR::reflora_summary()` to view available collections.",
           call. = FALSE)
  }

  if (is.null(dir)) dir <- paste0(herbarium, "_identify")
  .check_report_pkgs()
  dir <- .arg_check_dir(dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  fig_dir <- file.path(dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

  # -- 1. Databases: download what is not available yet ---------------------------
  if (verbose) message("\n[1/6] Checking the JABOT and REFLORA databases...")
  db <- .prepare_identify_databases(herbarium, source, jabot, network_herbaria,
                                    reflora, reflora_herbaria, jabot_path,
                                    reflora_path, verbose)

  # -- 2. Indeterminate specimens of the herbarium -------------------------------
  if (verbose) message("[2/6] Reading the records of '", herbarium, "' (",
                       source, ") and selecting its indeterminate specimens...")
  occ <- .load_identify_target(herbarium, source, db$jabot_dir, db$reflora_dir)
  if (nrow(occ) == 0)
    stop("No records were found for herbarium '", herbarium, "'.", call. = FALSE)

  target <- occ[.is_indet(occ), , drop = FALSE]

  if (!is.null(taxon)) {
    tf <- toupper(target$family) %in% toupper(taxon) |
      toupper(target$genus) %in% toupper(taxon)
    tf[is.na(tf)] <- FALSE
    if (!any(tf))
      stop("No indeterminate specimen of '", herbarium, "' belongs to the ",
           "given 'taxon'. Note that specimens without family and genus ",
           "cannot match a taxon.", call. = FALSE)
    target <- target[tf, , drop = FALSE]
  }

  if (nrow(target) == 0)
    stop("Herbarium '", herbarium, "' has no indeterminate specimens.",
         call. = FALSE)
  rm(occ)

  # -- 3. Merge the databases, keeping track of the source -------------------------
  if (verbose) message("[3/6] Reading the other herbaria of ",
                       paste(c(if (length(db$jabot_codes)) "JABOT",
                               if (length(db$reflora_codes)) "REFLORA"),
                             collapse = " and "),
                       " and keeping the records that may be duplicates...")
  candidates <- .load_identify_candidates(target, db$jabot_dir, db$jabot_codes,
                                          db$reflora_dir, db$reflora_codes,
                                          verbose)
  candidates$.jabot_target <- rep(FALSE, nrow(candidates))
  target$.jabot_target <- TRUE

  # -- 4. Standardization and duplicates (barRoso) --------------------------------
  if (verbose) message("[4/6] Standardizing records and flagging duplicates ",
                       "with barRoso...")

  jabot_input <- dplyr::bind_rows(target, candidates)
  jabot_input$.jabot_row <- seq_len(nrow(jabot_input))

  std_fun <- function() {
    barRoso::barroso_std(jabot_input,
                         unvouchered = FALSE,
                         delunkcoll = FALSE,
                         flag_duplicates = TRUE,
                         rm_duplicates = FALSE,
                         rm_original_column = TRUE)
  }
  std <- if (verbose) suppressWarnings(std_fun()) else
    suppressMessages(suppressWarnings(std_fun()))

  # -- 5. Identification suggestions --------------------------------------------------
  if (verbose) message("[5/6] Comparing the identification of duplicates...")
  res <- .compute_identify(std)

  # Links of the JABOT specimens come from the multimedia extension; REFLORA
  # records already carry theirs. Both specimens are linked, the indeterminate
  # one and the one that carries the identification, so that both images can
  # be compared
  links <- .load_identify_links(
    unique(c(herbarium, res$suggestions$dup_herbarium)),
    jabot_path, verbose)
  res$indets <- .fill_links(res$indets, links, "occurrenceID", "specimenLink")
  res$indets <- .fill_links(res$indets, links, "suggestedOccurrenceID",
                            "suggestedSpecimenLink")
  res$indets$suggestedOccurrenceID <- NULL
  res$suggestions <- .fill_links(res$suggestions, links, "occurrenceID",
                                 "specimenLink")
  res$suggestions <- .fill_links(res$suggestions, links, "dup_occurrenceID",
                                 "dup_specimenLink")
  res$suggestions$occurrenceID <- NULL
  res$suggestions$dup_occurrenceID <- NULL

  outcome_summary <- .identify_outcome_summary(res$indets)
  source_summary <- .identify_source_summary(res$suggestions)
  family_summary <- .identify_family_summary(res$indets)

  n_indets <- nrow(res$indets)
  n_matchable <- sum(res$indets$outcome != .identify_outcomes()[["unmatchable"]])
  n_with_dups <- sum(res$indets$n_duplicates > 0)
  n_resolvable <- sum(res$indets$n_useful > 0)
  n_species <- sum(res$indets$outcome == .identify_outcomes()[["species"]])
  n_conflict <- sum(res$indets$outcome == .identify_outcomes()[["conflict"]])

  # -- 6. Figures and report ---------------------------------------------------------
  if (verbose) message("[6/6] Building figures and report...")

  taxon_label <- if (is.null(taxon)) "all indeterminate specimens" else
    paste(taxon, collapse = ", ")

  plots <- .make_identify_plots(herbarium, taxon_label, outcome_summary,
                                source_summary, family_summary, n_indets)

  if (!is.null(format)) {
    .save_identify_figures(plots, fig_dir, format, fig_width, fig_height, verbose)
  }

  logo <- system.file("figures/jabotr_hex_sticker.png", package = "jabotR")
  file.copy(logo, file.path(fig_dir, "jabotr_hex_sticker.png"), overwrite = TRUE)

  csv_indets <- file.path(dir, paste0(herbarium, "_identify_indets.csv"))
  csv_sugg <- file.path(dir, paste0(herbarium, "_identify_suggestions.csv"))
  utils::write.csv(res$indets, csv_indets, row.names = FALSE, na = "")
  utils::write.csv(res$suggestions, csv_sugg, row.names = FALSE, na = "")

  analysis_list <- list(
    herbarium = herbarium,
    herbarium_source = source,
    taxon = taxon,
    taxon_label = taxon_label,
    sources = c(if (length(db$jabot_codes)) "JABOT",
                if (length(db$reflora_codes)) "REFLORA"),
    jabot_herbaria = db$jabot_codes,
    reflora_herbaria_used = db$reflora_codes,
    network_herbaria = network_herbaria,
    reflora_herbaria = reflora_herbaria,
    date_run = Sys.Date(),
    n_indets = n_indets,
    n_matchable = n_matchable,
    n_with_dups = n_with_dups,
    n_resolvable = n_resolvable,
    n_species = n_species,
    n_conflict = n_conflict,
    indets = res$indets,
    suggestions = res$suggestions,
    outcome_summary = outcome_summary,
    source_summary = source_summary,
    family_summary = family_summary,
    plots = plots
  )

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(analysis_list, tmp_rds)

  rmd_template <- system.file("rmd/jabot_identify_report.Rmd", package = "jabotR")
  if (!nzchar(rmd_template))
    stop("Rmd template not found. Reinstall the jabotR package.", call. = FALSE)

  html_out <- file.path(normalizePath(dir),
                        paste0(herbarium, "_identify.html"))

  if (verbose) message("Rendering HTML report...")
  rmarkdown::render(
    input = rmd_template,
    output_file = html_out,
    params = list(data_path = tmp_rds,
                  herbarium = herbarium),
    envir = new.env(parent = globalenv()),
    quiet = !verbose
  )
  unlink(tmp_rds)

  if (verbose) message("Report saved: ", html_out)

  if (open_report && interactive()) utils::browseURL(html_out)

  invisible(analysis_list)
}


# -- Internal helpers ----------------------------------------------------------

#' Acronyms of the herbaria with records in JABOT and in REFLORA
#'
#' The JABOT catalog also lists collections that publish no records, so the
#' summary of published collections is used.
#'
#' @keywords internal
#' @noRd
.jabot_codes <- function() {
  jabot_summary(herbarium = NULL, verbose = FALSE, save = FALSE)$collectionCode
}

.reflora_codes <- function() {
  refloraR::reflora_summary(herbarium = NULL, verbose = FALSE,
                            save = FALSE)$collectionCode
}

#' Where a herbarium is hosted: JABOT first, otherwise REFLORA
#'
#' @keywords internal
#' @noRd
.resolve_identify_source <- function(herbarium) {
  if (herbarium %in% .jabot_codes()) return("JABOT")
  if (herbarium %in% .reflora_codes()) return("REFLORA")
  stop("Herbarium '", herbarium, "' was not found in JABOT nor in REFLORA.",
       "\nUse `jabot_summary()` or `refloraR::reflora_summary()` to view the ",
       "available collections.", call. = FALSE)
}

#' Herbarium acronyms of the DwC-A folders of a directory
#'
#' @keywords internal
#' @noRd
.dwca_codes <- function(path, jabot = FALSE) {
  if (is.null(path) || !dir.exists(path)) return(character(0))
  folders <- list.files(path)
  codes <- toupper(stringr::str_extract(folders, "(?<=dwca[-_])[^-_]+"))
  # The records of RB are in the JABOT folder "jbrj"
  if (jabot) codes <- gsub("JBRJ", "RB", codes)
  unique(codes[!is.na(codes)])
}

#' First URL of a text (the image of the specimen, in REFLORA records)
#'
#' @keywords internal
#' @noRd
.first_url <- function(x) {
  x <- as.character(x)
  m <- regexpr("https?://[^[:space:]|;\"']+", x)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(m) & m > 0
  out[ok] <- regmatches(x, m)
  out
}

#' Same column types in every herbarium, so that they can be combined
#'
#' @keywords internal
#' @noRd
.std_occ_types <- function(df) {
  for (nm in names(df)) {
    if (nm %in% c("decimalLatitude", "decimalLongitude")) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    } else if (!is.character(df[[nm]])) {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  df
}

.merge_occ <- function(dwca, source) {
  df <- dplyr::bind_rows(lapply(dwca, function(x) {
    .std_occ_types(x[["data"]][["occurrence.txt"]])
  }))
  df$.jabot_source <- rep(source, nrow(df))
  df$.jabot_link <- if (source == "REFLORA" && "associatedMedia" %in% names(df))
    .first_url(df$associatedMedia) else rep(NA_character_, nrow(df))
  df
}

.merge_reflora_occur <- function(dwca) .merge_occ(dwca, "REFLORA")

#' All the records of one JABOT herbarium
#'
#' @keywords internal
#' @noRd
.read_jabot_herb <- function(path, herbarium) {
  .merge_occ(jabot_parse(path = path, herbarium = herbarium, verbose = FALSE),
             "JABOT")
}

#' All the records of one REFLORA herbarium
#'
#' @keywords internal
#' @noRd
.read_reflora_herb <- function(path, herbarium) {
  .merge_reflora_occur(refloraR::reflora_parse(path = path,
                                               herbarium = herbarium,
                                               verbose = FALSE))
}

#' Download what is not available yet and choose the herbaria to be read
#'
#' With \code{jabot_path = NULL} and \code{reflora_path = NULL} the default
#' folders (\code{jabot_download} and \code{reflora_download}) are used, and
#' only what is missing or outdated is downloaded. The REFLORA herbaria that
#' are already in the JABOT data are not used from REFLORA, nor downloaded.
#'
#' @keywords internal
#' @noRd
.prepare_identify_databases <- function(herbarium, source, jabot,
                                        network_herbaria, reflora,
                                        reflora_herbaria, jabot_path,
                                        reflora_path, verbose) {
  jabot_dir <- if (is.null(jabot_path)) "jabot_download" else jabot_path
  reflora_dir <- if (is.null(reflora_path)) "reflora_download" else reflora_path

  # JABOT
  jabot_codes <- character(0)
  if (jabot || source == "JABOT") {
    if (is.null(jabot_path)) {
      wanted <- if (jabot && is.null(network_herbaria)) NULL else
        unique(c(if (jabot) network_herbaria, if (source == "JABOT") herbarium))
      jabot_download(herbarium = wanted, verbose = verbose, dir = jabot_dir)
    }
    if (jabot) {
      available <- .dwca_codes(jabot_dir, jabot = TRUE)
      jabot_codes <- if (is.null(network_herbaria)) available else
        intersect(available, network_herbaria)
      jabot_codes <- setdiff(jabot_codes, herbarium)
    }
  }

  # REFLORA: not the herbaria already in JABOT, nor the one under investigation
  reflora_codes <- character(0)
  if (reflora || source == "REFLORA") {
    if (reflora) {
      selected <- if (is.null(reflora_herbaria)) .reflora_codes() else
        reflora_herbaria
      in_jabot <- intersect(selected, c(jabot_codes, herbarium))
      if (verbose && length(setdiff(in_jabot, herbarium)))
        message("REFLORA herbaria already in JABOT, not used from REFLORA: ",
                paste(setdiff(in_jabot, herbarium), collapse = ", "))
      reflora_codes <- setdiff(selected, in_jabot)
    }
    if (is.null(reflora_path)) {
      needed <- unique(c(reflora_codes,
                         if (source == "REFLORA") herbarium))
      if (length(needed))
        refloraR::reflora_download(herbarium = needed, verbose = verbose,
                                   dir = reflora_dir)
    }
    if (reflora)
      reflora_codes <- intersect(reflora_codes, .dwca_codes(reflora_dir))
  }

  list(jabot_dir = jabot_dir, reflora_dir = reflora_dir,
       jabot_codes = jabot_codes, reflora_codes = reflora_codes)
}

#' All the records of the herbarium to be investigated
#'
#' @keywords internal
#' @noRd
.load_identify_target <- function(herbarium, source, jabot_dir, reflora_dir) {
  if (source == "JABOT") .read_jabot_herb(jabot_dir, herbarium) else
    .read_reflora_herb(reflora_dir, herbarium)
}

#' Read the herbaria one at a time, keeping only the plausible duplicates
#'
#' @keywords internal
#' @noRd
.scan_herbaria <- function(codes, read_fun, target, label, verbose) {
  out <- vector("list", length(codes))
  for (i in seq_along(codes)) {
    if (verbose)
      message(sprintf("  [%s] %s (%d/%d)", label, codes[i], i, length(codes)))
    df <- tryCatch(read_fun(codes[i]), error = function(e) {
      if (verbose) message("  Note: ", codes[i], " could not be read. ",
                           conditionMessage(e))
      NULL
    })
    if (is.null(df) || nrow(df) == 0) next
    out[[i]] <- .select_candidate_duplicates(target, df)
  }
  dplyr::bind_rows(out)
}

#' Records of the other herbaria of JABOT and REFLORA that may be duplicates
#'
#' @keywords internal
#' @noRd
.load_identify_candidates <- function(target, jabot_dir, jabot_codes,
                                      reflora_dir, reflora_codes, verbose) {
  jabot_df <- .scan_herbaria(jabot_codes,
                             function(h) .read_jabot_herb(jabot_dir, h),
                             target, "JABOT", verbose)
  reflora_df <- .scan_herbaria(reflora_codes,
                               function(h) .read_reflora_herb(reflora_dir, h),
                               target, "REFLORA", verbose)
  dplyr::bind_rows(jabot_df, reflora_df)
}

#' Specimen links of JABOT herbaria (multimedia extension of the DwC-A)
#'
#' @keywords internal
#' @noRd
.load_identify_links <- function(herbaria, jabot_path, verbose) {
  empty <- data.frame(occurrenceID = character(0), specimenLink = character(0))
  dwca_path <- if (is.null(jabot_path)) "jabot_download" else jabot_path
  herbaria <- intersect(herbaria[!is.na(herbaria)],
                        .dwca_codes(dwca_path, jabot = TRUE))
  if (length(herbaria) == 0) return(empty)

  tryCatch({
    mm_dwca <- jabot_parse(path = dwca_path, herbarium = herbaria,
                           verbose = FALSE)
    .merge_multimedia_txt(mm_dwca)
  }, error = function(e) {
    if (verbose)
      message("Note: specimen links could not be loaded. ", e$message)
    empty
  })
}

#' Fill the links of JABOT specimens, keeping those already carried (REFLORA)
#'
#' @keywords internal
#' @noRd
.fill_links <- function(df, links, by_col, link_col) {
  idx <- match(df[[by_col]], links$occurrenceID)
  from_jabot <- links$specimenLink[idx]
  current <- if (link_col %in% names(df)) df[[link_col]] else
    rep(NA_character_, nrow(df))
  df[[link_col]] <- ifelse(!is.na(from_jabot), from_jabot, current)
  df
}

#' Placeholders for an unidentified rank
#'
#' @keywords internal
#' @noRd
.rank_missing <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x == "" | tolower(x) %in% c("na", "sp", "sp.", "spp", "spp.",
                                          "indet", "indet.", "cf", "cf.",
                                          "aff", "aff.")
}

#' Records lacking the family, the genus or the specific epithet
#'
#' @keywords internal
#' @noRd
.is_indet <- function(df) {
  epithet <- if ("specificEpithet" %in% names(df)) df$specificEpithet else
    rep(NA_character_, nrow(df))
  .rank_missing(df$family) | .rank_missing(df$genus) | .rank_missing(epithet)
}

.norm_number <- function(x) {
  x <- toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
  x[x %in% c("", "SN", "SNUM", "SEMNUMERO")] <- NA
  sub("^0+(?=.)", "", x, perl = TRUE)
}

.collector_tokens <- function(x) {
  x <- tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
  x <- gsub("[^a-z ]", " ", x)
  lapply(strsplit(x, "\\s+"), function(tk) unique(tk[nchar(tk) >= 2]))
}

#' Number or date that, with the collector, identifies a collection
#'
#' @keywords internal
#' @noRd
.identify_bases <- function(df, require_genus_for_date = TRUE) {
  num <- .norm_number(df$recordNumber)

  date <- paste(df$year, df$month, df$day, sep = "-")
  date[is.na(df$year) | is.na(df$month) | is.na(df$day)] <- NA
  if (require_genus_for_date) date[.rank_missing(df$genus)] <- NA

  ifelse(!is.na(num), paste0("n:", num),
         ifelse(!is.na(date), paste0("d:", date), NA_character_))
}

#' Loose keys (collector token + number or date) used to pre-select, among the
#' millions of network records, those that may be duplicates of a specimen.
#' Always a superset of what barRoso flags, which makes the exact matching.
#'
#' @keywords internal
#' @noRd
.identify_keys <- function(df, require_genus_for_date = TRUE,
                           base = .identify_bases(df, require_genus_for_date)) {
  tok <- .collector_tokens(df$recordedBy)

  n <- lengths(tok)
  idx <- rep(seq_along(tok), n)
  data.frame(row = idx,
             key = ifelse(is.na(base[idx]), NA_character_,
                          paste(base[idx], unlist(tok), sep = "|")),
             stringsAsFactors = FALSE)
}

.select_candidate_duplicates <- function(target, pool) {
  if (nrow(pool) == 0) return(pool)
  tb <- unique(.identify_bases(target))
  tb <- tb[!is.na(tb)]
  if (length(tb) == 0) return(pool[0, , drop = FALSE])

  # Stage 1, cheap: same number (or date) as some indeterminate specimen
  pb <- .identify_bases(pool, require_genus_for_date = FALSE)
  keep <- which(!is.na(pb) & pb %in% tb)
  if (length(keep) == 0) return(pool[0, , drop = FALSE])
  pool <- pool[keep, , drop = FALSE]
  pb <- pb[keep]

  # Stage 2: and sharing a collector name with it
  tk <- .identify_keys(target)
  tk <- tk$key[!is.na(tk$key)]
  pk <- .identify_keys(pool, base = pb)
  rows <- unique(pk$row[!is.na(pk$key) & pk$key %in% tk])
  pool[rows, , drop = FALSE]
}

.identify_outcomes <- function() {
  c(species = "Species-level identification available",
    genus = "Genus-level identification available",
    family = "Family-level identification available",
    conflict = "Duplicates identified differently (conflict)",
    none_better = "Duplicates found, none better identified",
    no_dups = "No duplicates found",
    unmatchable = "Cannot be matched (no collector or number)")
}

.dup_name <- function(family, genus, epithet) {
  ifelse(!.rank_missing(genus) & !.rank_missing(epithet),
         paste(genus, epithet),
         ifelse(!.rank_missing(genus), genus,
                ifelse(!.rank_missing(family), family, NA_character_)))
}

.dup_level <- function(family, genus, epithet) {
  ifelse(!.rank_missing(genus) & !.rank_missing(epithet), 3L,
         ifelse(!.rank_missing(genus), 2L,
                ifelse(!.rank_missing(family), 1L, 0L)))
}

#' Compare each indeterminate specimen with its duplicates in other herbaria
#'
#' @param std Output of barRoso::barroso_std() with the columns `.jabot_target`
#'   and `.jabot_row`.
#'
#' @keywords internal
#' @noRd
.compute_identify <- function(std) {
  oc <- .identify_outcomes()

  col <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else
    rep(NA_character_, nrow(df))
  clean <- function(x) { x <- trimws(x); x[.rank_missing(x)] <- NA; x }

  std$family <- clean(col(std, "family"))
  std$genus <- clean(col(std, "genus"))
  std$specificEpithet <- clean(col(std, "specificEpithet"))

  tgt <- std[std$.jabot_target %in% TRUE, , drop = FALSE]
  tgt <- tgt[.is_indet(tgt), , drop = FALSE]
  oth <- std[!(std$.jabot_target %in% TRUE) & !is.na(std$duplicateGroup), ,
             drop = FALSE]

  known_coll <- !is.na(tgt$recordedBy) & !tgt$recordedBy %in% "Unknown"
  has_date <- !is.na(tgt$year) & !is.na(tgt$month) & !is.na(tgt$day)
  matchable <- known_coll &
    (!is.na(tgt$recordNumber) | (!is.na(tgt$genus) & has_date))

  pairs <- merge(
    data.frame(.jabot_row = tgt$.jabot_row, duplicateGroup = tgt$duplicateGroup,
               t_family = tgt$family, t_genus = tgt$genus,
               t_epithet = tgt$specificEpithet, stringsAsFactors = FALSE)[
                 !is.na(tgt$duplicateGroup), ],
    data.frame(duplicateGroup = oth$duplicateGroup,
               dup_herbarium = col(oth, "collectionCode"),
               dup_catalogNumber = col(oth, "catalogNumber"),
               dup_occurrenceID = col(oth, "occurrenceID"),
               d_family = oth$family, d_genus = oth$genus,
               d_epithet = oth$specificEpithet,
               dup_identifiedBy = col(oth, "identifiedBy"),
               dup_dateIdentified = col(oth, "dateIdentified"),
               dup_source = col(oth, ".jabot_source"),
               d_link = col(oth, ".jabot_link"),
               stringsAsFactors = FALSE),
    by = "duplicateGroup")

  same <- function(a, b) is.na(a) | is.na(b) | a == b
  pairs$compatible <- same(pairs$t_family, pairs$d_family) &
    same(pairs$t_genus, pairs$d_genus) &
    same(pairs$t_epithet, pairs$d_epithet)
  pairs$fills <- (is.na(pairs$t_family) & !is.na(pairs$d_family)) |
    (is.na(pairs$t_genus) & !is.na(pairs$d_genus)) |
    (is.na(pairs$t_epithet) & !is.na(pairs$d_epithet))
  pairs$dup_level <- .dup_level(pairs$d_family, pairs$d_genus, pairs$d_epithet)
  pairs$dup_name <- .dup_name(pairs$d_family, pairs$d_genus, pairs$d_epithet)
  pairs$useful <- pairs$fills & pairs$compatible

  # Best suggestion for each specimen: highest level, then the most frequent
  # name, then the one with a determiner
  useful <- pairs[pairs$useful, , drop = FALSE]
  if (nrow(useful) > 0) {
    key <- paste(useful$.jabot_row, useful$dup_name)
    useful$freq <- as.integer(table(key)[key])
    useful <- useful[order(useful$.jabot_row, -useful$dup_level, -useful$freq,
                           is.na(useful$dup_identifiedBy),
                           useful$dup_herbarium, useful$dup_catalogNumber), ]
    best <- useful[!duplicated(useful$.jabot_row), , drop = FALSE]
  } else {
    best <- useful
  }

  agg <- function(f) {
    if (nrow(pairs) == 0) return(integer(0))
    tapply(f, pairs$.jabot_row, sum)
  }
  n_dup <- agg(rep(1L, nrow(pairs)))
  n_useful <- agg(as.integer(pairs$useful))
  n_conf <- agg(as.integer(pairs$fills & !pairs$compatible))

  pick <- function(v, rows) {
    out <- v[match(tgt$.jabot_row, rows)]
    out[is.na(out)] <- 0L
    as.integer(out)
  }
  n_duplicates <- if (length(n_dup)) pick(n_dup, as.integer(names(n_dup))) else
    rep(0L, nrow(tgt))
  n_useful_v <- if (length(n_useful)) pick(n_useful, as.integer(names(n_useful))) else
    rep(0L, nrow(tgt))
  n_conflict_v <- if (length(n_conf)) pick(n_conf, as.integer(names(n_conf))) else
    rep(0L, nrow(tgt))

  bi <- match(tgt$.jabot_row, best$.jabot_row)
  best_level <- best$dup_level[bi]

  outcome <- rep(oc[["no_dups"]], nrow(tgt))
  outcome[n_duplicates > 0] <- oc[["none_better"]]
  outcome[n_conflict_v > 0 & n_useful_v == 0] <- oc[["conflict"]]
  outcome[n_useful_v > 0 & best_level %in% 1L] <- oc[["family"]]
  outcome[n_useful_v > 0 & best_level %in% 2L] <- oc[["genus"]]
  outcome[n_useful_v > 0 & best_level %in% 3L] <- oc[["species"]]
  outcome[!matchable & n_duplicates == 0] <- oc[["unmatchable"]]

  current <- .dup_name(tgt$family, tgt$genus, tgt$specificEpithet)

  indets <- data.frame(
    herbarium = col(tgt, "collectionCode"),
    source = col(tgt, ".jabot_source"),
    catalogNumber = col(tgt, "catalogNumber"),
    occurrenceID = col(tgt, "occurrenceID"),
    recordedBy = tgt$recordedBy,
    recordNumber = tgt$recordNumber,
    identifiedBy = col(tgt, "identifiedBy"),
    dateIdentified = col(tgt, "dateIdentified"),
    year = col(tgt, "year"),
    stateProvince = col(tgt, "stateProvince"),
    municipality = col(tgt, "municipality"),
    family = tgt$family,
    genus = tgt$genus,
    specificEpithet = tgt$specificEpithet,
    currentIdentification = current,
    outcome = outcome,
    n_duplicates = n_duplicates,
    n_useful = n_useful_v,
    suggestedFamily = best$d_family[bi],
    suggestedIdentification = best$dup_name[bi],
    suggestedFrom = ifelse(is.na(bi), NA_character_,
                           paste(best$dup_herbarium[bi],
                                 best$dup_catalogNumber[bi])),
    suggestedIdentifiedBy = best$dup_identifiedBy[bi],
    suggestedDateIdentified = best$dup_dateIdentified[bi],
    suggestedSource = best$dup_source[bi],
    suggestedOccurrenceID = best$dup_occurrenceID[bi],
    specimenLink = col(tgt, ".jabot_link"),
    suggestedSpecimenLink = best$d_link[bi],
    stringsAsFactors = FALSE
  )
  indets$outcome <- factor(indets$outcome, levels = unname(oc))
  ord <- order(indets$outcome, indets$family, indets$genus, indets$recordedBy,
               suppressWarnings(as.numeric(indets$recordNumber)))
  indets <- indets[ord, , drop = FALSE]
  rownames(indets) <- NULL
  indets$outcome <- as.character(indets$outcome)

  suggestions <- pairs
  if (nrow(suggestions) > 0) {
    ti <- match(suggestions$.jabot_row, tgt$.jabot_row)
    suggestions <- data.frame(
      herbarium = indets$herbarium[1],
      occurrenceID = col(tgt, "occurrenceID")[ti],
      catalogNumber = col(tgt, "catalogNumber")[ti],
      currentIdentification = current[ti],
      recordedBy = tgt$recordedBy[ti],
      recordNumber = tgt$recordNumber[ti],
      identifiedBy = col(tgt, "identifiedBy")[ti],
      dateIdentified = col(tgt, "dateIdentified")[ti],
      dup_herbarium = suggestions$dup_herbarium,
      dup_source = suggestions$dup_source,
      dup_catalogNumber = suggestions$dup_catalogNumber,
      dup_identification = suggestions$dup_name,
      dup_family = suggestions$d_family,
      dup_identifiedBy = suggestions$dup_identifiedBy,
      dup_dateIdentified = suggestions$dup_dateIdentified,
      fills_missing_rank = suggestions$fills,
      compatible = suggestions$compatible,
      dup_occurrenceID = suggestions$dup_occurrenceID,
      specimenLink = col(tgt, ".jabot_link")[ti],
      dup_specimenLink = suggestions$d_link,
      stringsAsFactors = FALSE
    )
    suggestions <- suggestions[order(!suggestions$fills_missing_rank,
                                     !suggestions$compatible,
                                     suggestions$catalogNumber,
                                     suggestions$dup_herbarium), ]
    rownames(suggestions) <- NULL
  } else {
    suggestions <- data.frame(
      herbarium = character(0), occurrenceID = character(0),
      catalogNumber = character(0),
      currentIdentification = character(0), recordedBy = character(0),
      recordNumber = character(0), identifiedBy = character(0),
      dateIdentified = character(0), dup_herbarium = character(0),
      dup_source = character(0),
      dup_catalogNumber = character(0), dup_identification = character(0),
      dup_family = character(0), dup_identifiedBy = character(0),
      dup_dateIdentified = character(0), fills_missing_rank = logical(0),
      compatible = logical(0), dup_occurrenceID = character(0),
      specimenLink = character(0), dup_specimenLink = character(0),
      stringsAsFactors = FALSE)
  }

  list(indets = indets, suggestions = suggestions)
}

.identify_outcome_summary <- function(indets) {
  oc <- unname(.identify_outcomes())
  n <- as.integer(table(factor(indets$outcome, levels = oc)))
  data.frame(outcome = oc, n = n,
             pct = round(n / max(nrow(indets), 1) * 100, 1),
             stringsAsFactors = FALSE)
}

.identify_source_summary <- function(suggestions) {
  s <- suggestions[suggestions$fills_missing_rank & suggestions$compatible, ,
                   drop = FALSE]
  if (nrow(s) == 0)
    return(data.frame(herbarium = character(0), n_specimens = integer(0)))
  out <- stats::aggregate(catalogNumber ~ dup_herbarium, data = s,
                          FUN = function(x) length(unique(x)))
  names(out) <- c("herbarium", "n_specimens")
  out[order(-out$n_specimens), , drop = FALSE]
}

.identify_family_summary <- function(indets) {
  fam <- ifelse(is.na(indets$family), "Unknown family", indets$family)
  d <- data.frame(family = fam, resolvable = indets$n_useful > 0,
                  stringsAsFactors = FALSE)
  out <- stats::aggregate(resolvable ~ family, data = d,
                          FUN = function(x) c(sum(x), length(x)))
  data.frame(family = out$family,
             n_resolvable = out$resolvable[, 1],
             n_indets = out$resolvable[, 2],
             stringsAsFactors = FALSE)[order(-out$resolvable[, 2]), ]
}

.make_identify_plots <- function(herbarium, taxon_label, outcome_summary,
                                 source_summary, family_summary, n_indets) {
  col_main <- "#1D4E89"
  pal <- c("Species-level identification available" = "#2E7D32",
           "Genus-level identification available" = "#7CB342",
           "Family-level identification available" = "#C0CA33",
           "Duplicates identified differently (conflict)" = "#E65100",
           "Duplicates found, none better identified" = "#90A4AE",
           "No duplicates found" = "#CFD8DC",
           "Cannot be matched (no collector or number)" = "#ECEFF1")

  os <- outcome_summary
  os$tooltip <- paste0(os$outcome, "<br>Specimens: ", scales::comma(os$n),
                       " (", os$pct, "%)")
  p_outcome <- ggplot2::ggplot(
    os, ggplot2::aes(x = stats::reorder(outcome, -match(outcome, outcome)),
                     y = n, fill = outcome, text = tooltip)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = paste0("Outcome for the indeterminate specimens \u2014 ", herbarium),
      subtitle = paste0("Total: ", scales::comma(n_indets), " (", taxon_label, ")"),
      x = NULL, y = "Number of specimens") +
    .jabot_theme(plot_title_color = col_main)

  p_source <- NULL
  if (nrow(source_summary) > 0) {
    ss <- utils::head(source_summary, 20)
    ss$tooltip <- paste0("Herbarium: ", ss$herbarium,
                         "<br>Specimens with a candidate identification: ",
                         scales::comma(ss$n_specimens))
    p_source <- ggplot2::ggplot(
      ss, ggplot2::aes(x = stats::reorder(herbarium, n_specimens),
                       y = n_specimens, text = tooltip)) +
      ggplot2::geom_col(fill = "#2E7D32", width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste0("Herbaria holding identified duplicates \u2014 ", herbarium),
        x = NULL, y = "Specimens with a candidate identification") +
      .jabot_theme(plot_title_color = col_main)
  }

  p_family <- NULL
  if (nrow(family_summary) > 0) {
    fs <- utils::head(family_summary, 20)
    fs$n_other <- fs$n_indets - fs$n_resolvable
    long <- rbind(
      data.frame(family = fs$family, status = "With candidate identification",
                 n = fs$n_resolvable, stringsAsFactors = FALSE),
      data.frame(family = fs$family, status = "Without candidate identification",
                 n = fs$n_other, stringsAsFactors = FALSE))
    long$tooltip <- paste0("Family: ", long$family, "<br>", long$status, ": ",
                           scales::comma(long$n))
    long$family <- factor(long$family, levels = rev(fs$family))
    p_family <- ggplot2::ggplot(
      long, ggplot2::aes(x = family, y = n, fill = status, text = tooltip)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c(
        "With candidate identification" = "#2E7D32",
        "Without candidate identification" = "#B0BEC5")) +
      ggplot2::labs(
        title = paste0("Families of the indeterminate specimens \u2014 ", herbarium),
        subtitle = "Top 20 (specimens without family are grouped as 'Unknown family')",
        x = NULL, y = "Number of specimens", fill = NULL) +
      .jabot_theme(plot_title_color = col_main)
  }

  list(outcome = p_outcome, source = p_source, family = p_family)
}

.save_identify_figures <- function(plots, fig_dir, format, width, height, verbose) {
  names_map <- list(outcome = "01_identify_outcome",
                    source = "02_identify_source_herbaria",
                    family = "03_identify_families")
  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (is.null(p)) next
    base <- file.path(fig_dir, names_map[[nm]])
    for (fmt in format) {
      out <- paste0(base, ".", fmt)
      ggplot2::ggsave(out, plot = p, width = width, height = height,
                      device = fmt, dpi = 300)
      if (verbose) message("  Saved: ", out)
    }
  }
}
