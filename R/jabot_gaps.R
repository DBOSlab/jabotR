#' Herbarium gap analysis against the Flora e Funga do Brasil
#'
#' @author Domingos Cardoso
#'
#' @description
#' Identifies taxa listed in the
#' [Flora e Funga do Brasil (FFB)](https://floradobrasil.jbrj.gov.br/consulta/)
#' that are absent from a given JABOT-hosted herbarium collection. The function
#' downloads or reads pre-downloaded FFB and JABOT data, standardises synonyms,
#' and produces a rich set of analyses and visualisations:
#'
#' - Summary statistics (total FFB taxa, taxa in herbarium, missing, % coverage)
#' - Missing taxa broken down by **group**, **family**, **genus**, and
#'   **phytogeographic domain**
#' - Identification of **genera entirely absent** from the herbarium
#' - **Choropleth map** of missing taxa by Brazilian state
#' - Interactive searchable table of the top missing species
#' - Optionally, a **municipality-level heat map** ranking collecting
#'   priority nationwide (see `priority_map`)
#' - An **HTML report** (always generated) and optional **PDF** / **PNG** exports
#'   of every individual figure
#'
#' @param herbarium Character. Herbarium acronym in uppercase (e.g. `"RB"`).
#' @param jabot_path Character or `NULL`. Path to a directory holding previously
#'   downloaded JABOT DwC-A files (created by [jabot_download()]). When `NULL`
#'   (default) the records are downloaded automatically.
#' @param priority_map Logical. If `TRUE`, additionally downloads pooled
#'   occurrence records from the JABOT network (see `network_herbaria`) and
#'   builds a nationwide municipality-level heat map ranking collecting
#'   priority, i.e. which municipalities already hold the most known
#'   vouchers of species still missing from `herbarium` (see
#'   [jabot_missing()] for the region-scoped version of this analysis).
#'   Default `FALSE`, since it requires downloading the full network sample.
#' @param network_herbaria Character vector or `NULL`. Only used when
#'   `priority_map = TRUE`; restricts the pooled network sample to these
#'   herbarium acronyms. `NULL` (default) uses **all** JABOT-hosted herbaria.
#' @param format Character vector. Which static formats to save individual
#'   figures. Any combination of `"pdf"` and `"png"` is accepted. The HTML
#'   report is **always** generated regardless of this argument.
#' @param fig_width Numeric. Width of saved figures in inches. Default `10`.
#' @param fig_height Numeric. Height of saved figures in inches. Default `6`.
#' @param open_report Logical. If `TRUE` (default), the HTML report is opened
#'   in the default browser after rendering.
#' @param verbose Logical. If `TRUE` (default), progress messages are printed.
#' @param dir Character. Output directory. Defaults to
#'   `"<herbarium>_gap_analysis"`.
#'
#' @return Invisibly returns a named list with all computed data frames and
#'   ggplot2 objects. The HTML report (and optional PDF/PNG figures) are written
#'   to `dir`.
#'
#' @details
#' **Workflow**
#' 1. FFB accepted species are extracted from the DwC-A taxon table.
#' 2. JABOT records for the target herbarium are retrieved and filtered to
#'    determined specimens.
#' 3. Taxon names that appear as synonyms in FFB are resolved to their accepted
#'    names via [floraR::flora_search()].
#' 4. The intersection (`taxon_in_herb`) and difference (`taxon_not_in_herb`)
#'    between the two sets are computed.
#' 5. The missing taxa are joined with the FFB distribution table to obtain
#'    state- and domain-level breakdowns.
#' 6. All analyses are serialised to a temporary `.rds` file, which is read by
#'    the bundled Rmd template during rendering.
#'
#' @note
#' - Internet access is required unless `jabot_path` points to previously
#'   downloaded data.
#' - Synonym resolution via [floraR::flora_search()] can be slow for large
#'   herbaria; supply `jabot_path` to skip re-downloading JABOT records on
#'   repeated runs.
#' - The `rmarkdown`, `ggplot2`, `plotly`, `leaflet`, `DT`, `geobr`, and `sf`
#'   packages must be installed.
#'
#' @seealso [jabot_coverage()], [jabot_missing()], [jabot_records()], [jabot_download()]
#'
#' @examples
#' \dontrun{
#' # Full run (downloads everything) - renders HTML and saves PDF figures
#' jabot_gaps(herbarium = "RB",
#'            format = c("pdf", "png"),
#'            dir = "RB_gap_analysis")
#'
#' # Re-run using cached data to avoid re-downloading
#' jabot_gaps(herbarium = "RB",
#'            jabot_path = "jabot_download",
#'            format = "pdf")
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise arrange desc n_distinct left_join anti_join semi_join bind_rows relocate if_all across where
#' @importFrom tidyr separate_rows
#' @importFrom magrittr "%>%"
#' @importFrom utils head browseURL
#' @importFrom stats reorder na.omit
#' @importFrom scales comma
#' @importFrom rmarkdown render
#' @importFrom floraR flora_search
#' @importFrom geobr read_state read_municipality
#' @importFrom ggplot2 ggplot aes geom_col geom_sf coord_flip labs scale_fill_manual scale_fill_gradient scale_fill_gradientn geom_text scale_y_continuous expansion theme_void element_text element_rect margin unit
#' @importFrom plotly ggplotly
#' @importFrom DT datatable
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLegend colorNumeric
#' @importFrom sf st_as_sf
#' @importFrom stringi stri_trans_general
#' @importFrom htmltools tags p
#'
#' @export

jabot_gaps <- function(herbarium = NULL,
                       jabot_path = NULL,
                       priority_map = FALSE,
                       network_herbaria = NULL,
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

  if (is.null(dir)) dir <- paste0(herbarium, "_gap_analysis")
  dir <- .arg_check_dir(dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  fig_dir <- file.path(dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

  # -- 1. FFB data -------------------------------------------------------------
  if (verbose) message("\n[1/6] Loading Flora e Funga do Brasil data...")
  ffb <- .load_ffb_data(verbose)
  taxon <- ffb$taxon
  distribution <- ffb$distribution
  distribution$locationID <- gsub("BR-", "", distribution$locationID)
  speciesprofile <- ffb$speciesprofile
  typesandspecimen <- ffb$typesandspecimen

  taxon_accepted <- taxon %>%
    dplyr::filter(taxonomicStatus == "NOME_ACEITO",
                  taxonRank == "ESPECIE")

  taxon_non_accepted <- taxon %>%
    dplyr::filter(taxonomicStatus != "NOME_ACEITO",
                  taxonRank == "ESPECIE")

  # -- 2. JABOT herbarium records ----------------------------------------------
  if (verbose) message("[2/6] Loading JABOT records for herbarium '", herbarium, "'...")
  herb_df <- .load_herb_data(herbarium, jabot_path, verbose)

  # Keep only determined specimens at family and genus level
  herb_df <- herb_df[!is.na(herb_df$genus) & !is.na(herb_df$species), ]

  names(herb_df)[names(herb_df) %in% "division"] <- "phylum"
  idx <- match(herb_df$genus, taxon$genus)
  herb_df$group <- taxon$group[idx]
  herb_df$kingdom <- taxon$kingdom[idx]
  herb_df$phylum <- taxon$phylum[idx]
  herb_df$class <- taxon$class[idx]
  herb_df$order <- taxon$order[idx]
  herb_df$family <- taxon$family[idx]
  herb_df <- herb_df %>% dplyr::relocate(group, .before = kingdom)

  # -- 3. Synonym resolution ---------------------------------------------------
  if (verbose) message("[3/6] Resolving synonyms against FFB...")
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
    herb_df$family[tf] <- res$family[idx[tf]]
    if (verbose) message("   Resolved ", sum(tf), " synonym(s) to accepted names.")
  }

  # -- 4. Gap computation ------------------------------------------------------
  if (verbose) message("[4/6] Computing taxonomic gaps...")

  taxon_in_herb <- taxon_accepted[taxon_accepted$taxonName %in% herb_df$taxonName, ]
  taxon_not_in_herb <- taxon_accepted[!taxon_accepted$taxonName %in% herb_df$taxonName, ]

  missing_dist <- distribution %>%
    dplyr::semi_join(taxon_not_in_herb, by = "id") %>%
    dplyr::left_join(
      taxon_not_in_herb[, c("id", "kingdom", "phylum", "class", "order",
                            "family", "genus", "taxonName",
                            "scientificNameAuthorship", "references")],
      by = "id"
    )

  dist_summary <- distribution %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(locationID = paste(unique(na.omit(locationID)), collapse = " | "),
                     phytogeographicDomain =
                       paste(unique(unlist(strsplit(phytogeographicDomain, ",\\s*"))),
                             collapse = " | "),
                     endemism = paste(unique(na.omit(endemism)), collapse = " | "),
                     .groups = "drop") %>%
    dplyr::mutate(dplyr::across(
      where(is.character), ~ .x |> dplyr::na_if("") |> dplyr::na_if("NA")))

  taxon_not_in_herb <- taxon_not_in_herb %>%
    dplyr::left_join(dist_summary, by = "id")

  # Key statistics
  n_ffb_total <- nrow(taxon_accepted)
  n_in_herb <- nrow(taxon_in_herb)
  n_missing <- nrow(taxon_not_in_herb)
  pct_present <- round(n_in_herb / n_ffb_total * 100, 1)
  pct_missing <- round(n_missing  / n_ffb_total * 100, 1)

  n_endemic_missing <- missing_dist %>%
    dplyr::filter(endemism %in% c(TRUE, "TRUE", "Endemic", "Endemica",
                                  "End\u00eamica")) %>%
    dplyr::summarise(n = dplyr::n_distinct(id)) %>%
    dplyr::pull(n)

  # Summary by group
  group_gap <- taxon_not_in_herb %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(n_missing = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_missing))

  # Summary by family (top 30)
  family_gap <- taxon_not_in_herb %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(n_missing = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_missing))

  # Summary by genus - flag genera entirely absent from the herb
  genera_in_herb <- unique(taxon_in_herb$genus)
  genus_gap <- taxon_not_in_herb %>%
    dplyr::group_by(genus, family) %>%
    dplyr::summarise(n_missing = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(in_herb = genus %in% genera_in_herb) %>%
    dplyr::arrange(dplyr::desc(n_missing))

  genera_absent_all <- genus_gap %>%
    dplyr::filter(!in_herb) %>%
    dplyr::arrange(dplyr::desc(n_missing))

  n_genera_absent_all <- nrow(genera_absent_all)

  # Summary by state
  state_gap <- missing_dist %>%
    dplyr::mutate(abbrev_state = gsub("BR-", "", locationID)) %>%
    dplyr::group_by(abbrev_state) %>%
    dplyr::summarise(n_missing = dplyr::n_distinct(id), .groups = "drop") %>%
    dplyr::filter(!is.na(abbrev_state), nzchar(abbrev_state)) %>%
    dplyr::arrange(dplyr::desc(n_missing))

  # Summary by phytogeographic domain
  domain_gap <- missing_dist %>%
    tidyr::separate_rows(phytogeographicDomain, sep = ",\\s*") %>%
    dplyr::filter(!is.na(phytogeographicDomain),
                  nzchar(phytogeographicDomain)) %>%
    dplyr::group_by(phytogeographicDomain) %>%
    dplyr::summarise(n_missing = dplyr::n_distinct(id), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_missing))

  # Endemism breakdown
  endemism_gap <- missing_dist %>%
    dplyr::mutate(
      endemism_label = dplyr::case_when(
        endemism %in% c(TRUE, "TRUE", "Endemic",
                        "Endemica", "End\u00eamica") ~ "Brazilian endemic",
        TRUE                                    ~ "Non-endemic"
      )
    ) %>%
    dplyr::group_by(endemism_label) %>%
    dplyr::summarise(n_missing = dplyr::n_distinct(id), .groups = "drop")

  # -- 5. Municipality collecting priority (optional, nationwide) --------------
  municipality_priority <- NULL
  muni_sf <- NULL

  if (priority_map) {
    if (verbose) message("[5/6] Computing municipality collecting priority...")

    pool_df <- .load_pool_data(network_herbaria, jabot_path, verbose)
    pool_df <- pool_df[!is.na(pool_df$genus) & !is.na(pool_df$species), ]

    municipality_priority <- .compute_municipality_priority(
      pool_df, taxon_not_in_herb$taxonName, herbarium)

    muni_sf <- tryCatch({
      sf_obj <- .load_municipality_sf("all", verbose)
      sf_obj <- dplyr::left_join(sf_obj, municipality_priority,
                                 by = c("muni_key", "abbrev_state"))
      sf_obj$n_missing_species[is.na(sf_obj$n_missing_species)] <- 0
      sf_obj$n_records[is.na(sf_obj$n_records)] <- 0
      sf_obj
    }, error = function(e) {
      if (verbose)
        message("Note: municipality polygons could not be loaded — ",
               "priority map skipped. ", e$message)
      NULL
    })
  } else {
    if (verbose) message("[5/6] Skipping municipality collecting priority (priority_map = FALSE)...")
  }

  # -- 6. ggplot2 figures ------------------------------------------------------
  if (verbose) message("[6/6] Building figures...")

  plots <- .make_gap_plots(herbarium = herbarium,
                           group_gap = group_gap,
                           family_gap = family_gap,
                           genus_gap = genus_gap,
                           domain_gap = domain_gap,
                           endemism_gap = endemism_gap,
                           state_gap = state_gap,
                           muni_sf = muni_sf,
                           n_missing = n_missing)

  # Save individual figures if requested
  if (!is.null(format)) {
    .save_gap_figures(plots, fig_dir, format, fig_width, fig_height, verbose)
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

  # -- 6. Render HTML report ---------------------------------------------------
  analysis_list <- list(
    herbarium = herbarium,
    date_run = Sys.Date(),
    n_ffb_total = n_ffb_total,
    n_in_herb = n_in_herb,
    n_missing = n_missing,
    pct_present = pct_present,
    pct_missing = pct_missing,
    n_endemic_missing = n_endemic_missing,
    n_genera_absent_all = n_genera_absent_all,
    group_gap = group_gap,
    family_gap = family_gap,
    genus_gap = genus_gap,
    genera_absent_all = genera_absent_all,
    state_gap = state_gap,
    domain_gap = domain_gap,
    endemism_gap = endemism_gap,
    taxon_not_in_herb = taxon_not_in_herb,
    missing_dist = missing_dist,
    municipality_priority = municipality_priority,
    muni_sf = muni_sf,
    plots = plots
  )

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(analysis_list, tmp_rds)

  rmd_template <- system.file("rmd/jabot_gaps_report.Rmd",
                              package = "jabotR")
  if (!nzchar(rmd_template))
    stop("Rmd template not found. Reinstall the jabotR package.", call. = FALSE)

  html_out <- file.path(normalizePath(dir), paste0(herbarium, "_gap_analysis.html"))

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

.make_gap_plots <- function(herbarium,
                            group_gap,
                            family_gap,
                            genus_gap,
                            domain_gap,
                            endemism_gap,
                            state_gap,
                            muni_sf = NULL,
                            n_missing) {

  pal_terra <- c(
    "#6D071A",
    "#8A1538",
    "#A4243B",
    "#BC4749",
    "#CD5C5C",
    "#D98E73",
    "#E6B8A2",
    "#F2D7C9"
  )
  col_miss <- "#A4243B"
  col_pres <- "#A3B18A"

  # 1 - Group gap
  group_gap$tooltip <- paste0(
    "Group: ", group_gap$group,
    "<br>Missing taxa: ", scales::comma(group_gap$n_missing)
  )
  p_group <- ggplot2::ggplot(
    group_gap,
    ggplot2::aes(x = stats::reorder(group, n_missing),
                 y = n_missing,
                 text = tooltip,
                 fill = group)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = pal_terra, guide = "none") +
    ggplot2::labs(
      title = paste0("Missing taxa by taxonomic group — ", herbarium),
      subtitle = paste0("Total missing from FFB: ", n_missing),
      x = NULL, y = "Number of missing taxa") +
    .jabot_theme(plot_title_color = "#A4243B")

  # 2 - Top 20 missing families (lollipop)
  # top_fam <- utils::head(family_gap, 20)
  # top_fam$tooltip <- paste0(
  #   "Family: ", top_fam$family,
  #   "<br>Missing species: ", scales::comma(top_fam$n_missing)
  # )

  # 3 - Top 20 missing genera (bar, coloured by whether genus is absent at all)
  top_gen <- utils::head(genus_gap, 20)
  top_gen$tooltip <- paste0(
    "Genus: ", top_gen$genus,
    "<br>Missing species: ", scales::comma(top_gen$n_missing),
    "<br>Status: ",
    ifelse(top_gen$in_herb,
           "Partially represented",
           "Absent from herbarium")
  )
  p_genus <- ggplot2::ggplot(
    top_gen,
    ggplot2::aes(x = stats::reorder(genus, n_missing),
                 y = n_missing,
                 fill = in_herb,
                 text = tooltip)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = col_miss, "TRUE" = "#A3B18A"),
      labels = c("FALSE" = "Genus absent from herbarium",
                 "TRUE" = "Genus partially represented"),
      name = NULL,
      guide = "none"
    ) +
    ggplot2::labs(
      title = paste0("Top 20 genera with missing species — ", herbarium),
      subtitle = "Red = genus not represented at all in the herbarium",
      x = NULL, y = "Number of missing species"
    ) +
    .jabot_theme(plot_title_color = "#A4243B") +
    ggplot2::theme(legend.position = "top")

  # 4 - Phytogeographic domain gap
  domain_gap$tooltip <- paste0(
    "Domain: ", domain_gap$phytogeographicDomain,
    "<br>Missing taxa: ", scales::comma(domain_gap$n_missing)
  )
  p_domain <- ggplot2::ggplot(
    domain_gap,
    ggplot2::aes(x = stats::reorder(phytogeographicDomain, n_missing),
                 y = n_missing,
                 text = tooltip,
                 fill = n_missing)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient(low = "#F2D7C9",
                                 high = "#A4243B",
                                 guide = "none") +
    ggplot2::labs(
      title = paste0("Missing taxa by phytogeographic domain — ", herbarium),
      subtitle = "Based on FFB occurrence records",
      x = NULL,
      y = "Number of missing taxa") +
    .jabot_theme(plot_title_color = "#A4243B")

  # 5 - Endemism breakdown (bar)
  endemism_gap$tooltip <- paste0(
    endemism_gap$endemism_label,
    "<br>Missing taxa: ",
    scales::comma(endemism_gap$n_missing)
  )
  p_endemism <- ggplot2::ggplot(
    endemism_gap,
    ggplot2::aes(x = stats::reorder(endemism_label, n_missing),
                 y = n_missing,
                 fill = endemism_label,
                 text = tooltip)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.55) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(n_missing)),
      hjust = -0.2, size = 3.5, color = "#495057") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = c("Brazilian endemic" = col_miss,
                                          "Non-endemic" = "#A3B18A",
                                          guide = "none")) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = paste0("Missing taxa by endemism status — ", herbarium),
      x = NULL, y = "Number of missing taxa") +
    .jabot_theme(plot_title_color = "#A4243B")

  # 6 - Missing taxa by state (ggplot2 choropleth)
  p_state_map <- tryCatch({
    states_sf <- geobr::read_state(year = 2020, showProgress = FALSE)
    states_sf <- dplyr::left_join(states_sf, state_gap, by = "abbrev_state")
    states_sf$n_missing[is.na(states_sf$n_missing)] <- 0

    ggplot2::ggplot(states_sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = n_missing), color = "white",
                       linewidth = 0.3) +
      ggplot2::scale_fill_gradientn(colours = rev(pal_terra),
                                    name = "Missing taxa",
                                    labels = scales::comma) +
      ggplot2::labs(
        title = paste0("Missing taxa by Brazilian state — ", herbarium),
        subtitle = "Number of FFB species not represented in the herbarium",
        caption = "Source: Flora e Funga do Brasil / JABOT") +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", color = "#A4243B",
                                           size = 13),
        plot.subtitle = ggplot2::element_text(color = "#6c757d", size = 10),
        plot.caption = ggplot2::element_text(color = "#adb5bd", size = 8,
                                             hjust = 1),
        legend.position = "right"
      )
  }, error = function(e) {
    message("Note: geobr map could not be loaded — state map skipped. ", e$message)
    NULL
  })

  # 7 - Municipality collecting priority (choropleth, optional)
  p_priority_map <- NULL
  if (!is.null(muni_sf)) {
    p_priority_map <- tryCatch({
      ggplot2::ggplot(muni_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = n_missing_species),
                         color = "white", linewidth = 0.05) +
        ggplot2::scale_fill_gradient(low = "#F2D7C9", high = "#A4243B",
                                     name = "Missing species\nwith known vouchers") +
        ggplot2::labs(
          title = paste0("Priority municipalities for collecting expeditions — ",
                         herbarium),
          subtitle = "Missing species already vouchered by other network herbaria",
          caption = "Source: Flora e Funga do Brasil / JABOT") +
        ggplot2::theme_void(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", color = "#A4243B",
                                             size = 13),
          plot.subtitle = ggplot2::element_text(color = "#6c757d", size = 10),
          plot.caption = ggplot2::element_text(color = "#adb5bd", size = 8,
                                               hjust = 1),
          legend.position = "right"
        )
    }, error = function(e) NULL)
  }

  list(
    group = p_group,
    genus = p_genus,
    domain = p_domain,
    endemism = p_endemism,
    state = p_state_map,
    priority_map = p_priority_map
  )
}

.save_gap_figures <- function(plots, fig_dir, format, width, height, verbose) {
  names_map <- list(
    group = "01_gap_group",
    family = "02_gap_top_families",
    genus = "03_gap_top_genera",
    domain = "04_gap_domain",
    endemism = "05_gap_endemism",
    state = "06_gap_state_map",
    priority_map = "07_priority_municipality_map"
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
