#' Open the jabotR field notebook
#'
#' @author
#' Giulia Cavalcanti Ottino
#' Domingos Cardoso
#'
#' @description
#' Opens the interactive jabotR field notebook in the user's default web
#' browser. The application is distributed with the package and works
#' locally without an internet connection.
#'
#' The field notebook allows users to organize collection-event and
#' specimen data, export a standardized JABOT spreadsheet, and generate
#' a printable field notebook in PDF format.
#'
#' @param browser Logical. If `TRUE`, opens the field notebook in the
#'   default web browser. If `FALSE`, only returns the local path to the
#'   application.
#'
#' @return
#' Invisibly returns the local path to the field notebook `index.html`.
#'
#' @details
#' The field notebook runs locally using HTML, CSS, and JavaScript files
#' distributed with jabotR. It does not require Shiny or an internet
#' connection. Spreadsheet generation is performed locally in the browser.
#'
#' @examples
#' \dontrun{
#' jabot_fieldbook()
#' }
#'
#' @export

jabot_fieldbook <- function(browser = TRUE) {

  if (!is.logical(browser) ||
      length(browser) != 1L ||
      is.na(browser)) {

    stop(
      "`browser` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  app <- system.file(
    "jabot_fieldbook",
    "index.html",
    package = "jabotR"
  )

  if (!nzchar(app) || !file.exists(app)) {

    stop(
      paste0(
        "The jabotR field notebook could not be found.\n",
        "Reinstall jabotR and try again."
      ),
      call. = FALSE
    )
  }

  app <- normalizePath(
    app,
    winslash = "/",
    mustWork = TRUE
  )

  if (isTRUE(browser)) {

    encoded_path <- utils::URLencode(
      app,
      reserved = FALSE
    )

    url <- if (.Platform$OS.type == "windows") {
      paste0("file:///", encoded_path)
    } else {
      paste0("file://", encoded_path)
    }

    utils::browseURL(url)
  }

  invisible(app)
}

