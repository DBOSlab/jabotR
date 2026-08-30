#' Herbarium representativeness within the Flora e Funga do Brasil
#'
#' @author Domingos Cardoso
#'
#' @description
#' Measures how well a JABOT-hosted herbarium represents the
#' \href{https://floradobrasil.jbrj.gov.br/consulta/}{Flora e Funga do Brasil (FFB)}.
#' Unlike \code{\link{jabot_gaps}} (which focuses on what is missing), this
#' function characterises what is present, computing coverage ratios at
#' every taxonomic and geographic level:
#'
#' - Overall representativeness (% of FFB species held in the herbarium)
#'
#' - Coverage by group, family, and phytogeographic domain
#'
#' - Family-level bar charts ranked by coverage (best- and worst-covered)
#'
#' - Choropleth map of % coverage by Brazilian state
#'
#' - Summary statistics table
#'
#' - An HTML report (always generated) and optional PDF / PNG
#'   exports of every individual figure
#'
#' @param herbarium Character. Herbarium acronym in uppercase (e.g. \code{"RB"}).
#' @param jabot_path Character or \code{NULL}. Path to a directory holding previously
#'   downloaded JABOT DwC-A files. When \code{NULL} (default) the records are
#'   downloaded automatically via \code{\link{jabot_download}}.
#' @param format Character vector. Static formats for saving individual figures:
#'   \code{"pdf"}, \code{"png"}, or both. The HTML report is always generated.
#' @param fig_width Numeric. Width of saved figures in inches. Default \code{10}.
#' @param fig_height Numeric. Height of saved figures in inches. Default \code{6}.
#' @param open_report Logical. If \code{TRUE} (default), opens the HTML report in
#'   the default browser after rendering.
#' @param verbose Logical. If \code{TRUE} (default), progress messages are printed.
#' @param dir Character. Output directory. Defaults to \code{"<herbarium>_representativeness"}.
#'
#' @return Invisibly returns a named list with all computed data frames and
#'   ggplot2 objects. The HTML report (and optional figures) are written to
#'   \code{dir}.
#'
#' @details
#' Coverage is defined as:
#' \deqn{\text{coverage} = \frac{N_{\text{in herbarium}}}{N_{\text{FFB total}}} \times 100}
#'
#' The function follows the same data-preparation pipeline as \code{\link{jabot_gaps}}:
#' FFB data are parsed, JABOT records fetched, and synonym names resolved before
#' the comparison is made.
#'
#' @note
#' - Internet access is required unless both \code{ffb_path} and \code{jabot_path}
#'   are provided.
#'
#' - Synonym resolution can be slow for large herbaria; supply \code{jabot_path}
#'   to avoid re-downloading on repeated runs.
#'
#' @seealso \code{\link{jabot_gaps()}}
#' @seealso \code{\link{jabot_records()}}
#' @seealso \code{\link{jabot_download()}}
#'
#' @examples
#' \dontrun{
#' # Full run - renders HTML and saves PDF figures
#' jabot_coverage(herbarium = "RB",
#'                format = "pdf",
#'                dir = "RB_representativeness")
#'
#' # Re-run using cached data
#' jabot_coverage(herbarium = "RB",
#'                jabot_path = "jabot_download",
#'                format = c("pdf", "png"))
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise arrange desc n_distinct left_join anti_join semi_join if_all relocate full_join
#' @importFrom tidyr separate_rows
#' @importFrom magrittr "%>%"
#' @importFrom utils head
#' @importFrom stats reorder
#' @importFrom scales comma
#' @importFrom rmarkdown render
#' @importFrom floraR flora_search
#' @importFrom geobr read_state
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs scale_fill_manual scale_fill_gradient geom_text scale_y_continuous expansion theme_void element_text element_rect margin unit
#' @importFrom plotly ggplotly
#' @importFrom DT datatable formatStyle styleColorBar
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLegend colorNumeric
#' @importFrom sf st_as_sf
#' @importFrom htmltools tags p
#'
#' @export

jabot_coverage <- function(herbarium = NULL,
                           jabot_path = NULL,
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

  if (!is.null(format)) {
    format <- tolower(trimws(format))
    bad <- setdiff(format, c("pdf", "png"))
    if (length(bad))
      stop("'format' must be 'pdf', 'png', or both. Unrecognised: ",
           paste(bad, collapse = ", "), call. = FALSE)
  }

  if (is.null(dir)) dir <- paste0(herbarium, "_representativeness")
  dir <- .arg_check_dir(dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  fig_dir <- file.path(dir, "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

  # -- 1. FFB data -------------------------------------------------------------
  if (verbose) message("\n[1/5] Loading Flora e Funga do Brasil data...")
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

  # -- 2. JABOT records --------------------------------------------------------
  if (verbose) message("[2/5] Loading JABOT records for herbarium '", herbarium, "'...")
  herb_df <- .load_herb_data(herbarium, jabot_path, verbose)

  # Keep only determined specimens with family and species
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
  if (verbose) message("[3/5] Resolving synonyms against FFB...")
  splist <- unique(herb_df$taxonName)
  syns <- splist[splist %in% taxon_non_accepted$taxonName]
  if (length(syns) > 0) {
    res <- floraR::flora_search(syns, progress_bar = verbose,
                                rm_flora_database = FALSE)
    res <- res[!is.na(res$Accepted.taxon.Name), ]
    idx <- match(herb_df$taxonName, res$Search)
    tf <- !is.na(idx)
    herb_df$taxonName[tf] <- res$Accepted.taxon.Name[idx[tf]]
    herb_df$family[tf] <- res$family[idx[tf]]
  }

  # -- 4. Representativeness metrics -------------------------------------------
  if (verbose) message("[4/5] Computing representativeness metrics...")

  taxon_in_herb <- taxon_accepted[taxon_accepted$taxonName %in% herb_df$taxonName, ]
  taxon_not_in_herb <- taxon_accepted[!taxon_accepted$taxonName %in% herb_df$taxonName, ]

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

  taxon_in_herb <- taxon_in_herb %>%
    dplyr::left_join(dist_summary, by = "id")

  n_ffb_total <- nrow(taxon_accepted)
  n_in_herb <- nrow(taxon_in_herb)
  n_missing <- nrow(taxon_not_in_herb)
  coverage_pct <- round(n_in_herb / n_ffb_total * 100, 1)

  n_specimens <- nrow(herb_df)
  n_families <- dplyr::n_distinct(taxon_in_herb$family)
  n_genera <- dplyr::n_distinct(taxon_in_herb$genus)

  # Coverage by group
  group_total <- taxon_accepted %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(n_ffb = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_ffb))

  group_rep <- taxon_in_herb %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(n_in = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(group_total, by = "group") %>%
    dplyr::mutate(coverage_pct = round(n_in / n_ffb * 100, 1)) %>%
    dplyr::arrange(dplyr::desc(n_in))

  # Coverage by family
  family_total <- taxon_accepted %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(n_ffb = dplyr::n(), .groups = "drop")

  family_rep <- taxon_in_herb %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(n_in = dplyr::n(), .groups = "drop") %>%
    dplyr::full_join(family_total, by = "family") %>%
    dplyr::mutate(
      n_in = ifelse(is.na(n_in), 0L, n_in),
      coverage_pct = round(n_in / n_ffb * 100, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(coverage_pct), dplyr::desc(n_in))

  # Coverage by phytogeographic domain
  in_herb_dist <- distribution %>%
    dplyr::semi_join(taxon_in_herb, by = "id")

  total_dist <- distribution %>%
    dplyr::semi_join(taxon_accepted, by = "id")

  domain_total <- total_dist %>%
    tidyr::separate_rows(phytogeographicDomain, sep = ",\\s*") %>%
    dplyr::filter(!is.na(phytogeographicDomain), nzchar(phytogeographicDomain)) %>%
    dplyr::group_by(phytogeographicDomain) %>%
    dplyr::summarise(n_ffb = dplyr::n_distinct(id), .groups = "drop")

  domain_rep <- in_herb_dist %>%
    tidyr::separate_rows(phytogeographicDomain, sep = ",\\s*") %>%
    dplyr::filter(!is.na(phytogeographicDomain), nzchar(phytogeographicDomain)) %>%
    dplyr::group_by(phytogeographicDomain) %>%
    dplyr::summarise(n_in = dplyr::n_distinct(id), .groups = "drop") %>%
    dplyr::left_join(domain_total, by = "phytogeographicDomain") %>%
    dplyr::mutate(coverage_pct = round(n_in / n_ffb * 100, 1)) %>%
    dplyr::arrange(dplyr::desc(n_in))

  # Coverage by state
  state_total <- total_dist %>%
    dplyr::mutate(abbrev_state = gsub("BR-", "", locationID)) %>%
    dplyr::filter(!is.na(abbrev_state), nzchar(abbrev_state)) %>%
    dplyr::group_by(abbrev_state) %>%
    dplyr::summarise(n_ffb = dplyr::n_distinct(id), .groups = "drop")

  state_rep <- in_herb_dist %>%
    dplyr::mutate(abbrev_state = gsub("BR-", "", locationID)) %>%
    dplyr::filter(!is.na(abbrev_state), nzchar(abbrev_state)) %>%
    dplyr::group_by(abbrev_state) %>%
    dplyr::summarise(n_in = dplyr::n_distinct(id), .groups = "drop") %>%
    dplyr::left_join(state_total, by = "abbrev_state") %>%
    dplyr::mutate(coverage_pct = round(n_in / n_ffb * 100, 1)) %>%
    dplyr::arrange(dplyr::desc(coverage_pct))

  # Endemic species coverage
  endemic_ids <- distribution %>%
    dplyr::filter(
      endemism %in% c(
        TRUE, "TRUE",
        "Endemic", "Endemica", "End\u00eamica"
      )
    ) %>%
    dplyr::distinct(id)

  endemic_total <- taxon_accepted %>%
    dplyr::semi_join(endemic_ids, by = "id")

  endemic_in_herb <- taxon_in_herb %>%
    dplyr::semi_join(endemic_ids, by = "id")

  n_endemic_ffb <- nrow(endemic_total)

  n_endemic_in_herb <- nrow(endemic_in_herb)

  endemic_coverage_pct <- round(
    n_endemic_in_herb / n_endemic_ffb * 100,
    1
  )

  n_non_endemic_ffb <- n_ffb_total - n_endemic_ffb

  n_non_endemic_in_herb <- n_in_herb - n_endemic_in_herb

  non_endemic_coverage_pct <- round(
    n_non_endemic_in_herb / n_non_endemic_ffb * 100,
    1
  )

  # Type collections
  herb_types <- herb_df %>%
    dplyr::filter(
      !is.na(typeStatus),
      nzchar(typeStatus)
    )

  type_family_rep <- herb_types %>%
    dplyr::distinct(
      family,
      taxonName
    ) %>%
    dplyr::count(
      family,
      name = "n_type_taxa"
    ) %>%
    dplyr::arrange(
      dplyr::desc(n_type_taxa)
    )

  top_type_families <- utils::head(
    type_family_rep,
    20
  )

  type_group_rep <- herb_df %>%
    dplyr::distinct(group, taxonName) %>%
    dplyr::count(
      group,
      name = "n_species"
    ) %>%
    dplyr::left_join(
      herb_types %>%
        dplyr::distinct(
          group,
          taxonName
        ) %>%
        dplyr::count(
          group,
          name = "n_type_species"
        ),
      by = "group"
    ) %>%
    dplyr::mutate(
      n_type_species = ifelse(
        is.na(n_type_species),
        0L,
        n_type_species
      ),
      pct_type_species = round(
        n_type_species /
          n_species * 100,
        1
      )
    )

  # -- 5. ggplot2 figures ------------------------------------------------------
  if (verbose) message("[5/5] Building figures...")

  plots <- .make_rep_plots(
    herbarium = herbarium,
    group_rep = group_rep,
    family_rep = family_rep,
    domain_rep = domain_rep,
    state_rep = state_rep,
    type_group_rep = type_group_rep,
    type_family_rep = top_type_families,
    endemic_coverage_pct = endemic_coverage_pct,
    non_endemic_coverage_pct = non_endemic_coverage_pct,
    coverage_pct = coverage_pct,
    n_ffb_total = n_ffb_total,
    n_in_herb = n_in_herb
  )

  if (!is.null(format))
    .save_rep_figures(plots, fig_dir, format, fig_width, fig_height, verbose)

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
  rep_list <- list(
    herbarium = herbarium,
    date_run = Sys.Date(),
    n_ffb_total = n_ffb_total,
    n_in_herb = n_in_herb,
    n_missing = n_missing,
    coverage_pct = coverage_pct,
    n_specimens = n_specimens,
    n_families = n_families,
    n_genera = n_genera,
    group_rep = group_rep,
    family_rep = family_rep,
    domain_rep = domain_rep,
    state_rep = state_rep,
    taxon_in_herb = taxon_in_herb,
    n_endemic_ffb = n_endemic_ffb,
    n_endemic_in_herb = n_endemic_in_herb,
    endemic_coverage_pct = endemic_coverage_pct,
    n_non_endemic_ffb = n_non_endemic_ffb,
    n_non_endemic_in_herb = n_non_endemic_in_herb,
    non_endemic_coverage_pct = non_endemic_coverage_pct,
    n_type_specimens = nrow(herb_types),
    n_type_taxa = dplyr::n_distinct(herb_types$taxonName),
    type_group_rep = type_group_rep,
    type_family_rep = type_family_rep,
    plots = plots
  )

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(rep_list, tmp_rds)

  rmd_template <- system.file("rmd/jabot_coverage_report.Rmd",
                              package = "jabotR")
  if (!nzchar(rmd_template))
    stop("Rmd template not found. Reinstall the jabotR package.", call. = FALSE)

  html_out <- file.path(normalizePath(dir),
                        paste0(herbarium, "_representativeness.html"))

  if (verbose) message("Rendering HTML report...")
  rmarkdown::render(
    input = rmd_template,
    output_file = html_out,
    params = list(data_path = tmp_rds, herbarium = herbarium),
    envir = new.env(parent = globalenv()),
    quiet = !verbose
  )
  unlink(tmp_rds)

  if (verbose) message("Report saved: ", html_out)
  if (open_report && interactive()) utils::browseURL(html_out)

  invisible(rep_list)
}


# -- Internal helpers ----------------------------------------------------------

.make_rep_plots <- function(herbarium,
                            group_rep,
                            family_rep,
                            domain_rep,
                            state_rep,
                            type_group_rep,
                            type_family_rep,
                            endemic_coverage_pct,
                            non_endemic_coverage_pct,
                            coverage_pct,
                            n_ffb_total,
                            n_in_herb) {

  col_pres <- "#A3B18A"
  col_pres_dark <- "#588157"
  col_pres_light <- "#DAD7CD"
  col_miss <- "#e63946"

  # 1 - Overall gauge-style bar (group)
  group_rep$tooltip <- paste0(
    "<b>", group_rep$group, "</b>",
    "<br>Coverage: ", group_rep$coverage_pct, "%",
    "<br>Species represented: ", scales::comma(group_rep$n_in),
    "<br>FFB total: ", scales::comma(group_rep$n_ffb)
  )
  p_group <- ggplot2::ggplot(
    group_rep,
    ggplot2::aes(x = stats::reorder(group, coverage_pct),
                 y = coverage_pct,
                 text = tooltip,
                 fill = coverage_pct)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(coverage_pct, "%")),
      hjust = -0.15, size = 3.5, color = "#495057") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_gradient(low = col_pres_light,
                                 high = col_pres_dark,
                                 name = "Coverage %") +
    ggplot2::scale_y_continuous(
      limits = c(0, 110),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      title = paste0("Coverage by taxonomic group \u2014 ", herbarium),
      subtitle = paste0("Overall: ", coverage_pct, "% of FFB species represented"),
      x = NULL, y = "% of FFB taxa represented"
    ) +
    .jabot_theme(plot_title_color = "#2d6a4f")

  # 2 - Top 20 best-covered families
  top_cov <- utils::head(
    family_rep[family_rep$n_ffb >= 5, ], 20
  )
  top_cov$tooltip <- paste0(
    "<b>", top_cov$family, "</b>",
    "<br>Coverage: ", top_cov$coverage_pct, "%",
    "<br>Species represented: ", scales::comma(top_cov$n_in),
    "<br>FFB total: ", scales::comma(top_cov$n_ffb)
  )
  p_family_best <- ggplot2::ggplot(
    top_cov,
    ggplot2::aes(x = stats::reorder(family, coverage_pct),
                 y = coverage_pct,
                 text = tooltip,
                 fill = coverage_pct)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(coverage_pct, "%")),
      hjust = -0.15, size = 3, color = "#495057") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_gradient(low = col_pres_light, high = col_pres_dark,
                                 name = NULL, guide = "none") +
    ggplot2::scale_y_continuous(
      limits = c(0, 115),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      title = paste0("Top 20 best-covered families \u2014 ", herbarium),
      subtitle = "Families with \u2265 5 FFB species; ranked by % coverage",
      x = NULL, y = "% covered"
    ) +
    .jabot_theme(plot_title_color = "#2d6a4f")

  # 3 - Top 20 worst-covered families (with >= 10 FFB species)
  large_fam <- family_rep[family_rep$n_ffb >= 10, ]
  worst_cov <- utils::head(large_fam[order(large_fam$coverage_pct), ], 20)
  worst_cov$tooltip <- paste0(
    "<b>", worst_cov$family, "</b>",
    "<br>Coverage: ", worst_cov$coverage_pct, "%",
    "<br>Species represented: ", scales::comma(worst_cov$n_in),
    "<br>FFB total: ", scales::comma(worst_cov$n_ffb)
  )
  p_family_worst <- ggplot2::ggplot(
    worst_cov,
    ggplot2::aes(x = stats::reorder(family, -coverage_pct),
                 y = coverage_pct,
                 text = tooltip,
                 fill = coverage_pct)
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(coverage_pct, "%")),
      hjust = -0.15, size = 3, color = "#495057"
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_gradient(low = col_miss, high = "#fca789",
                                 name = NULL, guide = "none") +
    ggplot2::scale_y_continuous(
      limits = c(0, 55),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      title = paste0("Top 20 least-covered families \u2014 ", herbarium),
      subtitle = "Families with \u2265 10 FFB species; ranked by lowest % coverage",
      x = NULL, y = "% covered"
    ) +
    .jabot_theme(plot_title_color = "#2d6a4f")

  # 4 - Coverage by domain
  domain_rep$tooltip <- paste0(
    "<b>", domain_rep$phytogeographicDomain, "</b>",
    "<br>Coverage: ", domain_rep$coverage_pct, "%",
    "<br>Species represented: ", scales::comma(domain_rep$n_in),
    "<br>FFB total: ", scales::comma(domain_rep$n_ffb)
  )
  p_domain <- ggplot2::ggplot(
    domain_rep,
    ggplot2::aes(x = stats::reorder(phytogeographicDomain, coverage_pct),
                 y = coverage_pct,
                 text = tooltip,
                 fill = coverage_pct)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(coverage_pct, "%")),
                       hjust = -0.15, size = 3.5, color = "#495057") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_gradient(low = col_pres_light,
                                 high = col_pres_dark,
                                 name = "Coverage %") +
    ggplot2::scale_y_continuous(limits = c(0, 115),
                                labels = function(x) paste0(x, "%")) +
    ggplot2::labs(title = paste0("Coverage by phytogeographic domain \u2014 ", herbarium),
                  x = NULL, y = "% of FFB taxa represented") +
    .jabot_theme(plot_title_color = "#2d6a4f")

  # 6 - Type coverage
  type_group_rep$tooltip <- paste0(
    "<b>", type_group_rep$group, "</b>",
    "<br>Species represented: ",
    scales::comma(type_group_rep$n_species),
    "<br>Species with type material: ",
    scales::comma(type_group_rep$n_type_species),
    "<br>Representation: ",
    type_group_rep$pct_type_species,
    "%"
  )

  p_types <- ggplot2::ggplot(
    type_group_rep,
    ggplot2::aes(
      x = stats::reorder(
        group,
        pct_type_species
      ),
      y = pct_type_species,
      fill = pct_type_species,
      text = tooltip
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(pct_type_species,"%")),
      hjust = -0.15,
      color = "#495057") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient(low = col_pres_light,
                                 high = col_pres_dark,
                                 name = "Coverage %") +
    ggplot2::labs(
      title = paste0(
        "Species represented by type material \u2014 ",
        herbarium
      ),
      subtitle =
        "Percentage of represented species backed by type specimens",
      x = NULL,
      y = "%"
    ) +
    .jabot_theme(
      plot_title_color = "#2d6a4f"
    )

  type_family_rep$tooltip <- paste0(
    "<b>",
    type_family_rep$family,
    "</b>",
    "<br>Type taxa: ",
    scales::comma(
      type_family_rep$n_type_taxa
    )
  )

  p_type_families <- ggplot2::ggplot(
    type_family_rep,
    ggplot2::aes(
      x = stats::reorder(
        family,
        n_type_taxa
      ),
      y = n_type_taxa,
      fill = n_type_taxa,
      text = tooltip
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(n_type_taxa)),
                       hjust = -0.15, size = 3, color = "#495057") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_gradient(
      low = col_pres_light,
      high = col_pres_dark,
      name = "Coverage %",
      guide = "none") +
    ggplot2::labs(title = paste0("Top 20 families by type taxa \u2014 ", herbarium),
                  subtitle = "Families with the largest number of taxa represented by type specimens",
                  x = NULL, y = "Number of type taxa") +
    .jabot_theme(plot_title_color = "#2d6a4f")

  # 7 - Endemic coverage
  endemic_rep <- data.frame(
    category = c(
      "Endemic species",
      "Non-endemic species"
    ),
    coverage_pct = c(
      endemic_coverage_pct,
      non_endemic_coverage_pct
    )
  )

  endemic_rep$tooltip <- paste0(
    "<b>", endemic_rep$category, "</b>",
    "<br>Coverage: ",
    endemic_rep$coverage_pct,
    "%")

  p_endemic <- ggplot2::ggplot(
    endemic_rep,
    ggplot2::aes(x = category,
                 y = coverage_pct,
                 fill = coverage_pct,
                 text = tooltip)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(coverage_pct, "%")),
                       vjust = -0.3, color = "#495057") +
    ggplot2::scale_fill_gradient(low = col_pres_light,
                                 high = col_pres_dark,
                                 name = "Coverage %") +
    ggplot2::labs(title = paste0("Coverage of endemic species \u2014 ", herbarium),
                  x = NULL,
                  y = "% represented") +
    .jabot_theme(plot_title_color = "#2d6a4f")

  # 8 - State coverage map
  p_state_map <- tryCatch({
    states_sf <- geobr::read_state(year = 2020, showProgress = FALSE)
    states_sf <- dplyr::left_join(states_sf, state_rep, by = "abbrev_state")
    states_sf$coverage_pct[is.na(states_sf$coverage_pct)] <- 0

    states_sf$tooltip <- paste0(
      "<b>", states_sf$name_state, "</b>",
      "<br>Coverage: ", states_sf$coverage_pct, "%",
      "<br>Species represented: ", scales::comma(states_sf$n_in),
      "<br>FFB total: ", scales::comma(states_sf$n_ffb)
    )

    ggplot2::ggplot(states_sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = coverage_pct, text = tooltip),
                       color = "white",
                       linewidth = 0.3) +
      ggplot2::scale_fill_gradient(
        low = "#fff3e0",
        high = "#1b4332",
        name = "Coverage (%)",
        limits = c(0, 100)
      ) +
      ggplot2::labs(
        title = paste0("Herbarium coverage by Brazilian state \u2014 ", herbarium),
        subtitle = "% of FFB species with records in the herbarium",
        caption = "Source: Flora e Funga do Brasil / JABOT"
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", color = "#2d6a4f",
                                           size = 13),
        plot.subtitle = ggplot2::element_text(color = "#6c757d", size = 10),
        plot.caption = ggplot2::element_text(color = "#adb5bd", size = 8,
                                             hjust = 1),
        legend.position = "right"
      )
  }, error = function(e) {
    message("Note: geobr map could not be loaded \u2014 state map skipped. ",
            e$message)
    NULL
  })

  list(
    group = p_group,
    family_best = p_family_best,
    family_worst = p_family_worst,
    domain = p_domain,
    endemic = p_endemic,
    state = p_state_map,
    types = p_types,
    type_families = p_type_families
  )
}

.save_rep_figures <- function(plots, fig_dir, format, width, height, verbose) {
  names_map <- list(
    group = "01_rep_group",
    family_best = "02_rep_families_best",
    family_worst = "03_rep_families_worst",
    domain = "04_rep_domain",
    endemic = "05_rep_endemism",
    state = "06_rep_state_map",
    types = "07_rep_types",
    type_families = "08_rep_type_families"
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
