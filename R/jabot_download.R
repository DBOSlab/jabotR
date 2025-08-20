#' Download plant specimen records from JABOT
#'
#' @author
#' Domingos Cardoso
#'
#' @description
#' Download plant specimen records in Darwin Core Format from any
#' herbarium collection at \href{https://ipt.jbrj.gov.br/jabot}{JABOT}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#'
#' @usage
#' jabot_download(herbarium = NULL,
#'                verbose = TRUE,
#'                dir = "jabot_download")
#'
#' @param herbarium A vector of specific herbarium acronyms (collection code) in
#' uppercase letters or leave it as NULL to download records for all JABOT herbaria.
#'
#' @param verbose Logical, if \code{FALSE}, a message showing steps when downloading
#' herbarium specimen records will not be printed in the console in full.
#'
#' @param dir Pathway to the computer's directory, where the file will be saved.
#' The default is to create a directory named \code{jabot_download}
#' and the results will be saved within a subfolder named by each searched
#' JABOT-associated herbarium collection.
#'
#' @return Folder with DwC-A files for an specific or all JABOT-associated herbaria.
#'
#' @seealso \code{\link{jabot_summary}}
#'
#' @examples
#' \dontrun{
#'
#' jabot_download(herbarium = c("AFR", "RB"),
#'                verbose = TRUE,
#'                dir = "jabot_download")
#'}
#'
#' @importFrom stringr str_split
#' @importFrom utils download.file unzip write.csv
#'
#' @export
#'

jabot_download <- function(herbarium = NULL,
                           verbose = TRUE,
                           dir = "jabot_download") {

  # herbarium check
  if (!is.null(herbarium)) {
    .arg_check_herbarium(herbarium, verbose = verbose)
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

  # Get raw metadata from JABOT repository
  ipt_info <- .get_ipt_info(herbarium)
  ipt_metadata = ipt_info[[1]]
  herb_URLs = ipt_info[[2]]
  herb_code = ipt_info[[3]]

  for (i in seq_along(herb_URLs)) {

    herb_info <- .get_herb_info(herb_URLs, ipt_metadata, i)

    n <- as.numeric(gsub(",", "", herb_info[[1]][3]))
    if (n == 0) {
      next
    }

    summary_df <- data.frame(collectionCode = herb_code[i],
                             rightsHolder = herb_info[[4]][1],
                             contactPoint = herb_info[[2]][1],
                             hasEmail = herb_info[[3]][1],
                             Version = herb_info[[1]][1],
                             Published.on = herb_info[[1]][2],
                             Records = n,
                             Jabot_URL = herb_info[[5]])

    vdest = gsub("[.].*", "", summary_df$Version)
    vlast = gsub(".*[.]", "", summary_df$Version)

    dwca_folder = paste0("dwca-", herb_URLs[i], "-v", vdest)

    ex_dwca_folder = paste0("dwca-", herb_URLs[i], "-v", vdest, "_", vlast)
    jabot_database = paste0(dir, "/", gsub("-", "_", ex_dwca_folder))

    if (!dir.exists(jabot_database) |
        !any(list.files(jabot_database) %in% "occurrence.txt")) {

      tf <- grepl(paste0("dwca_", herb_URLs[i]), list.files(dir))
      if (any(tf)) {
        unlink(paste0(dir,"/", list.files(dir)[tf]), recursive = TRUE)
      }

      dwca_file = paste0(ifelse(herb_URLs[i] %in% "jbrj_rb",
                                "https://ipt.jbrj.gov.br/jbrj/archive.do?r=",
                                "https://ipt.jbrj.gov.br/jabot/archive.do?r="),
                         herb_URLs[i],
                         "&v=",
                         summary_df$Version)

      destdirfile = paste0(dir, "/", dwca_folder)
      ex_dwca_folder <- paste0(dir, "/", gsub("-", "_", ex_dwca_folder))

      if (verbose) {
        message(paste0("Downloading DwC-A files for the collection... ",
                       herb_code[i], " ", i, "/",
                       length(herb_code)))
      }

      utils::download.file(url = dwca_file,
                           destfile = destdirfile,
                           method = "curl")

      utils::unzip(destdirfile, exdir = ex_dwca_folder)
      unlink(destdirfile)

      utils::write.csv(summary_df, paste0(ex_dwca_folder, "/", herb_code[i], "_Jabot.csv"),
                       row.names = FALSE)

      if (verbose) {
        message(paste0(herb_code[i], " collection sucessfully downloaded!"))
      }

    }

  }

}


