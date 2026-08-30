#' Missing species for a local herbarium within a target region
#'
#' @author Domingos Cardoso
#'
#' @description
#' Identifies species listed in the \href{https://floradobrasil.jbrj.gov.br/consulta/}{Flora e Funga do Brasil (FFB)}
#' that are expected to occur in a target Brazilian state and/or municipality
#' (\code{geo}) but are absent from a given JABOT-hosted herbarium (\code{herbarium}),
#' regardless of where in Brazil that herbarium's own specimens were
#' collected. For each missing species, the function searches the pooled
#' occurrence records of the entire JABOT network (all herbaria, or a
#' subset given by \code{network_herbaria}) for existing vouchers collected within
#' the target region, and produces:
#'
#' - Summary statistics (FFB species expected in the region, species already
#'   held by the herbarium, missing species, % missing, missing species with
#'   known collecting localities)
#'
#' - A ranked table of missing species, with the number of network vouchers
#'   and municipalities already known for each (collecting priority)
#'
#' - Missing species broken down by family
#'
#' - An interactive map of candidate collecting localities, built from
#'   network vouchers of the missing species within the region
#'
#' - A municipality-level heat map ranking collecting priority, i.e.
#'   which municipalities within the region already hold the most known
#'   vouchers of species still missing from the herbarium, to help target
#'   future collecting expeditions
#'
#' - A full, record-level table of every individual network voucher of
#'   the missing species found within the region (not aggregated by
#'   species), each linking out to the physical specimen (image/viewer page)
#'   when the source herbarium provides one
#'
#' - An HTML report (always generated) and optional PDF / PNG exports of individual
#'   figures
#'
#' @param herbarium Character. Acronym (in uppercase) of the target/local
#'   JABOT-hosted herbarium whose gaps are being assessed (e.g. \code{"HUEFS"}).
#' @param geo Character vector. One or more Brazilian state names/acronyms
#'   and/or municipality names delimiting the region of interest (e.g.
#'   \code{"Bahia"}, \code{"BA"}, or \code{c("Feira de Santana", "Tucano")}).
#' @param network_herbaria Character vector or \code{NULL}. Restricts the pooled
#'   network sample used to search for candidate collecting points to these
#'   herbarium acronyms. \code{NULL} (default) uses \code{all} JABOT-hosted herbaria.
#' @param jabot_path Character or \code{NULL}. Path to a directory holding
#'   previously downloaded JABOT DwC-A files (created by \code{\link{jabot_download}}),
#'   shared by both the target herbarium and the network pool. When \code{NULL}
#'   (default) records are downloaded automatically.
#' @param format Character vector. Static formats for saving individual
#'   figures: \code{"pdf"}, \code{"png"}, or both. The HTML report is always
#'   generated.
#' @param fig_width Numeric. Width of saved figures in inches. Default \code{10}.
#' @param fig_height Numeric. Height of saved figures in inches. Default \code{6}.
#' @param open_report Logical. If \code{TRUE} (default), opens the HTML report in
#'   the default browser after rendering.
#' @param verbose Logical. If \code{TRUE} (default), progress messages are printed.
#' @param dir Character. Output directory. Defaults to
#'   \code{"<herbarium>_missing_<geo>"}.
#'
#' @return Invisibly returns a named list with all computed data frames and
#'   ggplot2 objects. The HTML report (and optional figures) are written to
#'   \code{dir}.
#'
#' @details
#' Because the Flora e Funga do Brasil reports geographic distribution at the
#' state level, the expected species checklist for the region is always
#' resolved at the state(s) intersecting \code{geo} (a municipality is mapped to
#' its state via the JABOT records found within it). The candidate collecting
#' map, however, is restricted to actual vouchers falling within the
#' municipalities/states supplied in \code{geo}.
#'
#' The function follows the same synonym-resolution pipeline as
#' \code{\link{jabot_gaps}}: taxon names recorded as synonyms in FFB are resolved to
#' their accepted names via \code{\link{floraR::flora_search}} before comparison.
#'
#' @note
#' - Internet access is required unless `jabot_path` points to previously
#'   downloaded data.
#'
#' - Downloading the full JABOT network (\code{network_herbaria = NULL}) can be
#'   slow; supply \code{jabot_path} to avoid re-downloading on repeated runs.
#'
#' - The \code{rmarkdown}, \code{ggplot2}, \code{plotly}, \code{leaflet}, \code{DT},
#'   \code{stringi} packages
#'   must be installed.
#'
#' @seealso \code{\link{jabot_gaps}}
#' @seealso \code{\link{jabot_coverage}}
#' @seealso \code{\link{jabot_records}}
#' @seealso \code{\link{jabot_download}}
#'
#' @examples
#' \dontrun{
#' # Missing species expected in Bahia state but absent from HUEFS herbarium
#' jabot_missing(herbarium = "HUEFS",
#'                  geo = "Bahia",
#'                  format = "pdf")
#'
#' # Restrict the candidate-locality search to a couple of municipalities
#' jabot_missing(herbarium = "HUEFS",
#'                  geo = c("Feira de Santana", "Tucano"),
#'                  jabot_path = "jabot_download")
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise arrange desc n_distinct left_join relocate first bind_rows distinct
#' @importFrom tidyr replace_na
#' @importFrom magrittr "%>%"
#' @importFrom utils head browseURL
#' @importFrom stats reorder na.omit
#' @importFrom scales comma
#' @importFrom rmarkdown render
#' @importFrom floraR flora_search
#' @importFrom geobr read_municipality
#' @importFrom ggplot2 ggplot aes geom_col geom_sf coord_flip labs scale_fill_manual scale_fill_gradient theme theme_void element_text
#' @importFrom plotly ggplotly
#' @importFrom DT datatable
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addLegend markerClusterOptions
#' @importFrom stringi stri_trans_general
#' @importFrom htmltools tags p
#'
#' @export

jabot_missing <- function(herbarium = NULL,
                             geo = NULL,
                             network_herbaria = NULL,
                             jabot_path = NULL,
                             format = NULL,
                             fig_width = 10,
                             fig_height = 6,
                             open_report = TRUE,
                             verbose = TRUE,
                             dir = NULL) {

  # -- Input validation --------------------------------------------------------
  if (is.null(herbarium))
    stop("'herbarium' must be provided (e.g. herbarium = 'HUEFS').", call. = FALSE)
  herbarium <- toupper(trimws(herbarium))
  if (length(herbarium) > 1)
    stop("'herbarium' must be a single herbarium acronym.", call. = FALSE)
  .arg_check_herbarium(herbarium, verbose = FALSE)

  if (is.null(geo) || length(geo) == 0)
    stop("'geo' must be provided: one or more Brazilian state and/or ",
        "municipality names (e.g. geo = 'Bahia' or ",
        "geo = c('Feira de Santana', 'Ilheus')).", call. = FALSE)

  geo_split <- .classify_geo(geo)
  if (length(geo_split$state_full) == 0 && length(geo_split$municipalities) == 0)
    stop("Could not interpret 'geo'. Provide valid Brazilian state ",
        "names/acronyms or municipality names.", call. = FALSE)

  if (!is.null(network_herbaria)) {
    network_herbaria <- toupper(trimws(network_herbaria))
    .arg_check_herbarium(network_herbaria, verbose = FALSE)
  }

  if (!is.null(format)) {
    format <- tolower(trimws(format))
    bad <- setdiff(format, c("pdf", "png"))
    if (length(bad))
      stop("'format' must be 'pdf', 'png', or both. Unrecognised: ",
           paste(bad, collapse = ", "), call. = FALSE)
  }

  region_label <- paste(geo, collapse = "_")
  region_label <- gsub("[^A-Za-z0-9]+", "_", region_label)
  region_label <- gsub("_+", "_", region_label)
  region_label <- gsub("^_|_$", "", region_label)

  if (is.null(dir)) dir <- paste0(herbarium, "_missing_", region_label)
  dir <- .arg_check_dir(dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  fig_dir <- file.path(dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

  # -- 1. FFB data -------------------------------------------------------------
  if (verbose) message("\n[1/7] Loading Flora e Funga do Brasil data...")
  ffb <- .load_ffb_data(verbose)
  taxon <- ffb$taxon
  distribution <- ffb$distribution
  distribution$locationID <- gsub("BR-", "", distribution$locationID)

  taxon_accepted <- taxon %>%
    dplyr::filter(taxonomicStatus == "NOME_ACEITO",
                  taxonRank == "ESPECIE")

  taxon_non_accepted <- taxon %>%
    dplyr::filter(taxonomicStatus != "NOME_ACEITO",
                  taxonRank == "ESPECIE")

  # -- 2. Target herbarium holdings (network-wide) ------------------------------
  if (verbose) message("[2/7] Loading JABOT records for herbarium '", herbarium, "'...")
  herb_df <- .load_herb_data(herbarium, jabot_path, verbose)
  herb_df <- herb_df[!is.na(herb_df$genus) & !is.na(herb_df$species), ]

  # -- 3. Pooled JABOT network records -----------------------------------------
  if (verbose) message("[3/7] Loading pooled JABOT network records...")
  pool_df <- .load_pool_data(network_herbaria, jabot_path, verbose)
  pool_df <- pool_df[!is.na(pool_df$genus) & !is.na(pool_df$species), ]

  # Restrict the pool to the target region (state and/or municipality match)
  pool_df$municipality_norm <- toupper(
    stringi::stri_trans_general(pool_df$municipality, "Latin-ASCII"))
  geo_muni_norm <- toupper(
    stringi::stri_trans_general(trimws(geo_split$municipalities), "Latin-ASCII"))

  pool_geo <- pool_df[pool_df$stateProvince %in% geo_split$state_full |
                        pool_df$municipality_norm %in% geo_muni_norm, ]
  pool_geo <- pool_geo[pool_geo$collectionCode != herbarium, ]

  if (nrow(pool_geo) == 0)
    stop("No JABOT records were found for the given 'geo'. Check the ",
        "spelling of the state/municipality names.", call. = FALSE)

  # Attach a clickable link to the physical specimen (from the multimedia
  # DwC-A extension, when the source herbarium provides one). This re-parses
  # (offline, no re-download) only the dwca folders relevant to the region.
  multimedia_links <- tryCatch({
    dwca_path <- if (is.null(jabot_path)) "jabot_download" else jabot_path
    mm_dwca <- jabot_parse(path = dwca_path,
                           herbarium = unique(pool_geo$collectionCode),
                           verbose = FALSE)
    .merge_multimedia_txt(mm_dwca)
  }, error = function(e) {
    if (verbose)
      message("Note: specimen links could not be loaded. ", e$message)
    data.frame(occurrenceID = character(0), specimenLink = character(0))
  })

  pool_geo <- dplyr::left_join(pool_geo, multimedia_links, by = "occurrenceID")

  # States intersecting 'geo' (explicit states + states inferred from matched
  # municipalities), used to resolve the FFB checklist for the region
  br_states <- .br_states()
  region_abbrev <- unique(c(geo_split$state_abbrev,
                            unname(br_states[match(pool_geo$stateProvince,
                                                    names(br_states))])))
  region_abbrev <- region_abbrev[!is.na(region_abbrev)]

  if (length(region_abbrev) == 0)
    stop("Could not determine the Brazilian state(s) associated with 'geo'.",
        call. = FALSE)

  region_label_display <- paste(unique(c(geo_split$state_full,
                                         geo_split$municipalities)),
                                collapse = ", ")

  # -- 4. Synonym resolution ----------------------------------------------------
  if (verbose) message("[4/7] Resolving synonyms against FFB...")

  splist <- unique(herb_df$taxonName)
  syns <- splist[splist %in% taxon_non_accepted$taxonName]
  if (length(syns) > 0) {
    res <- floraR::flora_search(syns,
                                progress_bar = verbose,
                                rm_flora_database = FALSE)
    res <- res[!is.na(res$Accepted.taxon.Name), ]
    idx <- match(herb_df$taxonName, res$Search)
    tf <- !is.na(idx)
    herb_df$taxonName[tf] <- res$Accepted.taxon.Name[idx[tf]]
  }

  splist_pool <- unique(pool_geo$taxonName)
  syns_pool <- splist_pool[splist_pool %in% taxon_non_accepted$taxonName]
  if (length(syns_pool) > 0) {
    res_pool <- floraR::flora_search(syns_pool,
                                     progress_bar = verbose,
                                     rm_flora_database = FALSE)
    res_pool <- res_pool[!is.na(res_pool$Accepted.taxon.Name), ]
    idx <- match(pool_geo$taxonName, res_pool$Search)
    tf <- !is.na(idx)
    pool_geo$taxonName[tf] <- res_pool$Accepted.taxon.Name[idx[tf]]
  }

  # -- 5. Missing-species computation -------------------------------------------
  if (verbose) message("[5/7] Computing missing species for the region...")

  region_dist_ids <- distribution$id[distribution$locationID %in% region_abbrev]
  taxon_region <- taxon_accepted[taxon_accepted$id %in% region_dist_ids, ]

  if (nrow(taxon_region) == 0)
    stop("No FFB species are recorded for the Brazilian state(s) associated ",
        "with 'geo'.", call. = FALSE)

  taxon_in_herb <- taxon_region[taxon_region$taxonName %in% herb_df$taxonName, ]
  taxon_missing <- taxon_region[!taxon_region$taxonName %in% herb_df$taxonName, ]

  n_ffb_region <- nrow(taxon_region)
  n_in_herb <- nrow(taxon_in_herb)
  n_missing <- nrow(taxon_missing)
  pct_missing <- round(n_missing / n_ffb_region * 100, 1)

  # Candidate collecting points: network vouchers of the missing species,
  # within the target region, with valid coordinates
  missing_pool_records <- pool_geo[pool_geo$taxonName %in% taxon_missing$taxonName, ]
  missing_pool_points <- missing_pool_records[
    !is.na(missing_pool_records$decimalLatitude) &
      !is.na(missing_pool_records$decimalLongitude), ]

  species_pool_summary <- missing_pool_records %>%
    dplyr::group_by(taxonName) %>%
    dplyr::summarise(n_records = dplyr::n(),
                     n_municipalities = dplyr::n_distinct(municipality),
                     herbaria = paste(sort(unique(collectionCode)), collapse = ", "),
                     .groups = "drop")

  taxon_missing <- taxon_missing %>%
    dplyr::left_join(species_pool_summary, by = "taxonName") %>%
    dplyr::mutate(n_records = tidyr::replace_na(n_records, 0),
                  n_municipalities = tidyr::replace_na(n_municipalities, 0),
                  herbaria = tidyr::replace_na(herbaria, ""),
                  locatable = n_records > 0) %>%
    dplyr::arrange(dplyr::desc(n_records))

  n_missing_locatable <- sum(taxon_missing$locatable)

  # Full list of individual network records (one row per specimen, not
  # aggregated by species) for the missing species, each with a link to the
  # physical specimen when the source herbarium provides one
  record_cols <- c("family", "taxonName", "collectionCode", "recordedBy",
                   "recordNumber", "eventDate", "municipality", "stateProvince",
                   "decimalLatitude", "decimalLongitude", "specimenLink")
  record_cols <- record_cols[record_cols %in% names(missing_pool_records)]
  missing_pool_records_full <- missing_pool_records[, record_cols, drop = FALSE]
  names(missing_pool_records_full)[names(missing_pool_records_full) == "collectionCode"] <- "herbarium"
  missing_pool_records_full <- missing_pool_records_full[
    order(missing_pool_records_full$taxonName, missing_pool_records_full$herbarium), ]
  rownames(missing_pool_records_full) <- NULL

  family_gap <- taxon_missing %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(n_missing = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_missing))

  evidence_gap <- data.frame(
    status = c("Locatable via network pool", "No known locality in pool"),
    n = c(n_missing_locatable, n_missing - n_missing_locatable)
  )

  # -- 6. Municipality collecting priority --------------------------------------
  if (verbose) message("[6/7] Computing municipality collecting priority...")

  municipality_priority <- .compute_municipality_priority(pool_geo,
                                                           taxon_missing$taxonName,
                                                           herbarium)

  muni_sf <- tryCatch({
    sf_obj <- .load_municipality_sf(region_abbrev, verbose)
    sf_obj <- dplyr::left_join(sf_obj, municipality_priority,
                               by = c("muni_key", "abbrev_state"))
    sf_obj$n_missing_species[is.na(sf_obj$n_missing_species)] <- 0
    sf_obj$n_records[is.na(sf_obj$n_records)] <- 0
    sf_obj
  }, error = function(e) {
    if (verbose)
      message("Note: municipality polygons could not be loaded \u2014 ",
             "priority map skipped. ", e$message)
    NULL
  })

  # -- 7. Figures ----------------------------------------------------------------
  if (verbose) message("[7/7] Building figures...")

  plots <- .make_missing_plots(herbarium = herbarium,
                                  region_label = region_label_display,
                                  family_gap = family_gap,
                                  evidence_gap = evidence_gap,
                                  muni_sf = muni_sf,
                                  n_missing = n_missing)

  if (!is.null(format)) {
    .save_missing_figures(plots, fig_dir, format, fig_width, fig_height, verbose)
  }

  # Load jabotR logo
  logo <- system.file(
    "figures/jabotr_hex_sticker.png",
    package = "jabotR"
  )

  file.copy(
    logo,
    file.path(dir, "figures", "jabotr_hex_sticker.png"),
    overwrite = TRUE
  )

  # -- 8. Render HTML report ------------------------------------------------------
  analysis_list <- list(
    herbarium = herbarium,
    geo = geo,
    region_label = region_label_display,
    date_run = Sys.Date(),
    n_ffb_region = n_ffb_region,
    n_in_herb = n_in_herb,
    n_missing = n_missing,
    pct_missing = pct_missing,
    n_missing_locatable = n_missing_locatable,
    family_gap = family_gap,
    evidence_gap = evidence_gap,
    taxon_missing = taxon_missing,
    missing_pool_points = missing_pool_points,
    missing_pool_records = missing_pool_records_full,
    municipality_priority = municipality_priority,
    muni_sf = muni_sf,
    plots = plots
  )

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(analysis_list, tmp_rds)

  rmd_template <- system.file("rmd/jabot_missing_report.Rmd",
                              package = "jabotR")
  if (!nzchar(rmd_template))
    stop("Rmd template not found. Reinstall the jabotR package.", call. = FALSE)

  html_out <- file.path(normalizePath(dir),
                        paste0(herbarium, "_missing_", region_label, ".html"))

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

  if (verbose)
    message("Report saved: ", html_out)

  if (open_report && interactive())
    utils::browseURL(html_out)

  invisible(analysis_list)
}


# -- Internal helpers ----------------------------------------------------------

#' Split a 'geo' vector into recognised Brazilian states and municipalities
#'
#' @keywords internal
#' @noRd
.classify_geo <- function(geo) {
  valid_states <- .br_states()
  geo_ascii <- toupper(stringi::stri_trans_general(trimws(geo), "Latin-ASCII"))
  full_ascii <- toupper(stringi::stri_trans_general(names(valid_states), "Latin-ASCII"))
  acro_ascii <- toupper(stringi::stri_trans_general(unname(valid_states), "Latin-ASCII"))

  is_state <- logical(length(geo))
  state_full <- character(length(geo))
  for (i in seq_along(geo)) {
    mf <- match(geo_ascii[i], full_ascii)
    ma <- match(geo_ascii[i], acro_ascii)
    if (!is.na(mf)) {
      is_state[i] <- TRUE
      state_full[i] <- names(valid_states)[mf]
    } else if (!is.na(ma)) {
      is_state[i] <- TRUE
      state_full[i] <- names(valid_states)[ma]
    }
  }

  state_full <- unique(state_full[is_state])
  list(
    state_full = state_full,
    state_abbrev = unname(valid_states[state_full]),
    municipalities = geo[!is_state]
  )
}

.make_missing_plots <- function(herbarium,
                                   region_label,
                                   family_gap,
                                   evidence_gap,
                                   muni_sf = NULL,
                                   n_missing) {

  col_miss <- "#1D4E89"
  col_pres <- "#A3B18A"

  # 1 - Top 20 missing families
  top_fam <- utils::head(family_gap, 20)
  top_fam$tooltip <- paste0(
    "Family: ", top_fam$family,
    "<br>Missing species: ", scales::comma(top_fam$n_missing)
  )
  p_family <- ggplot2::ggplot(
    top_fam,
    ggplot2::aes(x = stats::reorder(family, n_missing),
                 y = n_missing,
                 text = tooltip,
                 fill = n_missing)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0("Top families with missing species \u2014 ", herbarium,
                     " (", region_label, ")"),
      subtitle = paste0("Total missing from region: ", n_missing),
      x = NULL, y = "Number of missing species") +
    .jabot_theme(plot_title_color = col_miss)

  # 2 - Evidence of known collecting localities
  evidence_gap$tooltip <- paste0(
    evidence_gap$status, "<br>Species: ", scales::comma(evidence_gap$n)
  )
  p_evidence <- ggplot2::ggplot(
    evidence_gap,
    ggplot2::aes(x = stats::reorder(status, n),
                 y = n,
                 fill = status,
                 text = tooltip)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.55) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(
      "Locatable via network pool" = col_pres,
      "No known locality in pool" = col_miss)) +
    ggplot2::labs(
      title = paste0("Missing species with known network localities \u2014 ",
                     herbarium),
      x = NULL, y = "Number of species") +
    .jabot_theme(plot_title_color = col_miss)

  # 3 - Municipality collecting priority (choropleth)
  p_priority_map <- NULL
  if (!is.null(muni_sf)) {
    p_priority_map <- tryCatch({
      ggplot2::ggplot(muni_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = n_missing_species),
                         color = "white", linewidth = 0.2) +
        ggplot2::scale_fill_gradient(low = "#F2D7C9", high = col_miss,
                                     name = "Missing species\nwith known vouchers") +
        ggplot2::labs(
          title = paste0("Priority municipalities for collecting expeditions \u2014 ",
                         herbarium),
          subtitle = paste0("Missing species already vouchered by other ",
                            "network herbaria, by municipality (", region_label, ")"),
          caption = "Source: Flora e Funga do Brasil / JABOT") +
        ggplot2::theme_void(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", color = col_miss,
                                             size = 13),
          plot.subtitle = ggplot2::element_text(color = "#6c757d", size = 10),
          plot.caption = ggplot2::element_text(color = "#adb5bd", size = 8,
                                               hjust = 1),
          legend.position = "right"
        )
    }, error = function(e) NULL)
  }

  list(
    family = p_family,
    evidence = p_evidence,
    priority_map = p_priority_map
  )
}

.save_missing_figures <- function(plots, fig_dir, format, width, height, verbose) {
  names_map <- list(
    family = "01_missing_family",
    evidence = "02_missing_evidence",
    priority_map = "03_priority_municipality_map"
  )
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
