#' Retrieve indeterminate specimens from JABOT
#'
#' @author
#' Domingos Cardoso
#'
#' @description
#' Retrieves occurrence records for indeterminate specimens (e.g., identified
#' only to family or genus level) from the \href{https://ipt.jbrj.gov.br/jabot}{JABOT}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#' The function automatically downloads and parses Darwin Core Archive (DwC-A)
#' files, applies optional filters by taxon, herbarium, state, and year, and
#' exports the results if desired.
#'
#' @details
#' This function supports downloading and processing Darwin Core Archive (DwC-A)
#' files directly from the JABOT repository. It allows for flexible filtering
#' by taxon, herbarium, locality (Brazilian states), and collection year(s). The
#' `level` parameter enables filtering for indeterminate records such as those
#' identified only to FAMILY or GENUS rank. The function uses helper functions
#' like `.arg_check_herbarium()` and `.filter_occur_df()` to validate inputs and
#' refine the occurrence records. If `path` is not provided, the function will
#' automatically manage downloading and storing fresh DwC-A archives.
#'
#' @note
#' - This function automatically downloads and parses Darwin Core Archive (DwC-A)
#'   files for the specified herbarium collections using \code{jabot_download()}
#'   internally.
#' - If \code{path = NULL}, DwC-A files will be downloaded into a folder named
#'   \code{jabot_download} within your working directory.
#' - If \code{save = TRUE}, the filtered output will be saved as a CSV file inside
#'   the folder specified by \code{dir}. This folder will be created if it does
#'   not already exist.
#' - Ensure an active internet connection if downloading is required.
#' - Some herbarium codes may not have updated records. Use \code{verbose = TRUE}
#'   to monitor messages during execution.
#' - Filtering by \code{level} does not guarantee full coverage of indeterminate
#'   records due to possible inconsistencies in \code{taxonRank} values in JABOT
#'   source data.
#' - For reproducibility, consider recording your input parameters and saving all
#'   outputs.
#'
#' @usage
#' jabot_indets(level = NULL,
#'              herbarium = NULL,
#'              taxon = NULL,
#'              state = NULL,
#'              recordYear = NULL,
#'              reorder = c("herbarium", "taxa", "collector", "area", "year"),
#'              path = NULL,
#'              updates = TRUE,
#'              verbose = TRUE,
#'              save = TRUE,
#'              dir = "jabot_indets",
#'              filename = "jabot_indets_search")
#'
#' @param level Character vector. Filter by taxonomic level. Accepted values:
#' `"FAMILY"`, `"GENUS"`, or both. Defaults to `NULL` to include all
#' indeterminate ranks.
#'
#' @param herbarium Character vector. Herbarium codes (e.g., `"RB"`, `"SP"`) in
#' uppercase. Use `NULL` to include all herbaria.
#
#' @param taxon Character vector. Specific taxon names to filter by
#' (e.g., `"Fabaceae"`).
#'
#' @param state Character vector. Brazilian state full name or abbreviations
#' (e.g., `"BA"`, `"SP"`) to filter by locality.
#'
#' @param recordYear Character or numeric vector. A single year (e.g., `"2001"`)
#' or a range (e.g., `c("2000", "2022")`).
#'
#' @param reorder Character vector. Reorder output by columns. Defaults to:
#' `c("herbarium", "taxa", "collector", "area", "year")`.
#'
#' @param path Character. Path to existing JABOT dwca files. If `NULL`,
#' downloads fresh data.
#'
#' @param updates Logical. If `TRUE` (default), checks for updated DwC-A files
#' from JABOT.
#'
#' @param verbose Logical. If `TRUE` (default), prints progress messages to the
#' console.
#'
#' @param save Logical. If `TRUE` (default), saves the results to a CSV file.
#'
#' @param dir Character. Directory path to save output files. Default:
#' `"jabot_indets"`.
#'
#' @param filename Character. Name of the output file (without extension).
#' Default: `"jabot_indets_search"`.
#'
#' @return A `data.frame` containing filtered specimen records for the selected
#' indeterminate specimens and criteria. If `save = TRUE`, a CSV file with the
#' results will be written to the specified `dir`, and a `log.txt` file will be
#' created or appended in the same directory summarizing the download session and
#' key statistics (total records, breakdowns by herbarium, family, genus, country,
#' and state).
#'
#' @seealso \code{\link{jabot_download}}
#' @seealso \code{\link{jabot_parse}}
#'
#' @examples
#' \dontrun{
#' # Retrieve indeterminate records for Fabaceae and Ochnaceae from all herbaria
#' jabot_indets(taxon = c("Fabaceae", "Ochnaceae"),
#'              level = "FAMILY",
#'              save = TRUE,
#'              dir = "jabot_indets",
#'              filename = "fabaceae_ochnaceae_records")
#'
#' # Filter by specific herbarium and state
#' jabot_indets(taxon = "Fabaceae",
#'              herbarium = "RB",
#'              state = c("BA", "MG"),
#'              recordYear = c("1990", "2022"))
#' }
#'
#' @importFrom stringr str_split
#' @importFrom utils write.csv capture.output
#' @importFrom dplyr bind_rows filter
#' @importFrom magrittr "%>%"
#'
#' @export
#'

jabot_indets <- function(level = NULL,
                         herbarium = NULL,
                         taxon = NULL,
                         state = NULL,
                         recordYear = NULL,
                         reorder = c("herbarium", "taxa", "collector", "area", "year"),
                         path = NULL,
                         updates = TRUE,
                         verbose = TRUE,
                         save = TRUE,
                         dir = "jabot_indets",
                         filename = "jabot_indets_search") {

  # level check
  if (!is.null(level)) {
    if (verbose) {
      message("Checking whether the input level is correct...")
    }
    level <- .arg_check_level(level)
  }

  # herbarium check
  if (!is.null(herbarium)) {
    if (verbose) {
      message("Checking whether the input herbarium code exists in the JABOT...")
    }
    .arg_check_herbarium(herbarium)
  }

  # state check
  if (!is.null(state)) {
    if (verbose) {
      message("Checking whether the input state list exists in the JABOT...")
    }
    state <- .arg_check_state(state)
  }

  # recordYear check
  if (!is.null(recordYear)) {
    if (verbose) {
      message("Checking whether the input recordYear range exists in the JABOT...")
    }
    .arg_check_recordYear(recordYear)
  }

  # dir check
  dir <- .arg_check_dir(dir)

  # Create a new directory to save the dataframe
  # If there is no directory create one in the working directory
  if (!dir.exists(dir)) {
    if (verbose) {
      message(paste0("Creating directory '", dir, "' in working directory..."))
    }
    dir.create(dir)
  }

  if (!is.null(path)) {
    if (updates) {
      if (verbose) {
        message(paste0("Updating dwca files within '",
                       path, "'"))
      }

      # The jabot_download will get updated dwca files only if any of the current
      # versions differ from the JABOT IPT
      jabot_download(herbarium = herbarium,
                     verbose = verbose,
                     dir = path)
    }

    # Parse JABOT dwca files
    dwca_files <- jabot_parse(path = path,
                              herbarium = herbarium,
                              verbose = verbose)
  } else {

    # The jabot_download will get updated dwca files only if any of the current
    # versions differ from the JABOT IPT
    jabot_download(herbarium = herbarium,
                   verbose = verbose,
                   dir = "jabot_download")

    # Parse JABOT dwca files
    dwca_files <- jabot_parse(path = "jabot_download",
                              herbarium = herbarium,
                              verbose = verbose)
  }

  # Extract each "occurrence.txt" data frame and merge them
  occur_df <- .merge_occur_txt(dwca_files)

  if (is.null(level)) {
    # Keep only higher-rank indeterminate taxa
    indets <- c("family", "genus", "FAMILY", "GENERO", "FAMILIA", "SUB_FAMILIA",
                "TRIBO", "DIVISAO", "ORDEM", "CLASSE", "fam.", "gen.")

    tf <- occur_df$taxonRank %in% indets
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  } else {
    if (level == "FAMILY") {
      indets <- c("family", "FAMILY", "FAMILIA", "fam.")
      tf <- occur_df$taxonRank %in% indets
      if (any(tf)) {
        occur_df <- occur_df[tf, ]
      }
    }
    if (level == "GENUS") {
      indets <- c("genus", "GENERO", "gen.")
      tf <- occur_df$taxonRank %in% indets
      if (any(tf)) {
        occur_df <- occur_df[tf, ]
      }
    }
  }

  # Filter occurrence data
  occur_df <- .filter_occur_df(occur_df, taxon, state, recordYear, verbose)

  # Reorder the data by the order of specific columns
  occur_df <- .reorder_df(occur_df, reorder)

  # Save the search results if param save is TRUE
  if (save) {
    .save_csv(df = occur_df,
              verbose = verbose,
              filename = filename,
              dir = dir)
    .save_log(df = occur_df,
              herbarium = herbarium,
              filename = filename,
              dir = dir)
  }

  return(occur_df)
}
