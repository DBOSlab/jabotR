#' Check determination conflicts among duplicates across JABOT herbaria
#'
#' @author
#' Domingos Cardoso & Giulia Ottino
#'
#' @description
#' Identifies determination conflicts for duplicate collections across JABOT herbaria,
#' using a duplication key built from normalized \code{recordedBy} + \code{recordNumber}.
#'
#' A focal specimen (from \code{focal_herbarium}) is included in the output only if at
#' least one duplicate in another herbarium has a different determination when compared
#' by a canonical taxon string that ignores authorship (genus + epithet; using structured
#' Darwin Core fields when available).
#'
#' If a focal specimen has at least one conflicting duplicate, the output includes
#' \emph{all} determinations found in other herbaria for that same duplication key,
#' including those equal to the focal determination, to support reconstruction of the
#' specimens determination history.
#'
#' @details
#' This function is fully offline. It processes Darwin Core Archive (DwC-A)
#' \code{occurrence.txt} files that must already exist locally under \code{path}.
#' It loads all \code{occurrence.txt} files into a DuckDB database, builds a duplication
#' key (\code{dup_key}) from cleaned collector and record number fields, and then compares
#' focal determinations against determinations from other herbaria.
#'
#' Canonical determination (\code{det_canon}) is computed as:
#' \itemize{
#'   \item binomial: \code{genus + specificEpithet}, or
#'   \item trinomial: \code{genus + specificEpithet + infraspecificEpithet}
#' }
#' Author strings are ignored. Rank markers such as \code{subsp.}, \code{ssp.}, \code{var.},
#' \code{infr.} are used only to detect the infraspecific epithet in fallback parsing and
#' are not included in the canonical output.
#'
#' Records with missing/invalid collector or collection number are excluded from the analysis
#'
#' @usage
#' jabot_crossdet(focal_herbarium,
#'                taxon = NULL,
#'                path = "jabot_download",
#'                save = TRUE,
#'                dir = "jabot_crossdet",
#'                filename = "jabot_crossdet",
#'                dbdir = NULL)
#'
#' @param focal_herbarium Character (length 1). Focal herbarium code.
#'
#' @param taxon Character vector or \code{NULL}. Optional filter applied to the focal records
#' (family, genus, species).
#'
#' @param path Character. Folder containing DwC-A directories with \code{occurrence.txt}
#' files (downloaded previously, e.g. via \code{jabot_download()}).
#'
#' @param save Logical. If \code{TRUE}, saves an \code{.xlsx} file with a single sheet.
#'
#' @param dir Character. Output directory (used only when \code{save = TRUE}).
#'
#' @param filename Character. Output filename without extension (used only when \code{save = TRUE}).
#'
#' @param dbdir Character or \code{NULL}. DuckDB file path. Use \code{NULL} for in-memory.
#'
#' @return A list with one element:
#' \describe{
#'   \item{matches}{A data.frame (long format): one row per focal specimen and other herbarium
#'   determination, for all focal specimens that have at least one conflicting determination elsewhere.}
#' }
#'
#' @seealso \code{\link{jabot_download}}
#' @seealso \code{\link{jabot_parse}}
#'
#' @examples
#' \dontrun{
#' # i) Focal herbarium (RB) across all taxa (offline: path must exist locally)
#' out1 <- jabot_crossdet(focal_herbarium = "RB",
#'                        path = "jabot_download",
#'                        save = FALSE)
#'
#' # ii) Filter focal records by family
#' out2 <- jabot_crossdet(focal_herbarium = "RB",
#'                        taxon = "Fabaceae",
#'                        path = "jabot_download",
#'                        save = FALSE)
#'
#' # iii) Filter focal records by genus
#' out3 <- jabot_crossdet(focal_herbarium = "RB",
#'                        taxon = "Inga",
#'                        path = "jabot_download",
#'                        save = FALSE)
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom openxlsx write.xlsx
#'
#' @export
#'

jabot_crossdet <- function(focal_herbarium,
                           taxon = NULL,
                           path = "jabot_download",
                           save = TRUE,
                           dir = "jabot_crossdet",
                           filename = "jabot_crossdet",
                           dbdir = NULL) {

  `%||%` <- function(x, y) if (is.null(x)) y else x

  .save_xlsx_one_sheet <- function(df, dir, filename) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required when save=TRUE. Install it with install.packages('openxlsx').",
           call. = FALSE)
    }
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    out <- file.path(dir, paste0(filename, ".xlsx"))
    openxlsx::write.xlsx(list(Determinations = df), out, overwrite = TRUE)
    message("Saved: ", out)
    invisible(out)
  }

  for (p in c("DBI", "duckdb")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is required. Install it with install.packages('", p, "').",
           call. = FALSE)
    }
  }

  focal_herbarium <- toupper(focal_herbarium)
  if (length(focal_herbarium) != 1 || is.na(focal_herbarium) || !nzchar(focal_herbarium)) {
    stop("Argument 'focal_herbarium' must be a single non-empty herbarium code (e.g., 'RB').",
         call. = FALSE)
  }

  # Only validate/create output dir when saving
  if (isTRUE(save)) {
    if (exists(".arg_check_dir", mode = "function")) {
      dir <- .arg_check_dir(dir)
    } else {
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
  }

  if (is.null(path) || !nzchar(path)) path <- "jabot_download"

  files <- list.files(path, pattern = "^occurrence\\.txt$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) stop("No 'occurrence.txt' found under: ", path, call. = FALSE)

  con <- tryCatch(
    DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir %||% ":memory:"),
    error = function(e) DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  )
  # Disable DuckDB progress bar
  try(DBI::dbExecute(con, "PRAGMA enable_progress_bar=false;"), silent = TRUE)
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  std <- c(
    "collectionCode","catalogNumber","occurrenceID",
    "recordedBy","recordNumber",
    "family","genus","specificEpithet","infraspecificEpithet","taxonRank",
    "scientificName","acceptedScientificName","verbatimScientificName",
    "identifiedBy","dateIdentified"
  )

  DBI::dbExecute(con, paste0(
    "CREATE OR REPLACE TABLE occ AS SELECT ",
    paste0("CAST(NULL AS VARCHAR) AS ", std, collapse = ", "),
    " WHERE 1=0;"
  ))

  # parser helpers (no verbose)
  delim1 <- function(f) {
    l <- tryCatch(readLines(f, n = 1, warn = FALSE), error = function(e) "")
    if (!nzchar(l)) return("\t")
    ct <- c(
      tab   = lengths(regmatches(l, gregexpr("\t", l, fixed = TRUE))),
      pipe  = lengths(regmatches(l, gregexpr("|",  l, fixed = TRUE))),
      semi  = lengths(regmatches(l, gregexpr(";",  l, fixed = TRUE))),
      comma = lengths(regmatches(l, gregexpr(",",  l, fixed = TRUE)))
    )
    k <- names(ct)[which.max(ct)]
    switch(k, tab = "\t", pipe = "|", semi = ";", comma = ",")
  }

  header_fields <- function(f, d) {
    x <- tryCatch(readLines(f, n = 1, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) character(0))
    if (!length(x)) x <- tryCatch(readLines(f, n = 1, warn = FALSE, encoding = "latin1"),
                                  error = function(e) character(0))
    if (!length(x)) return(character(0))
    strsplit(x, d, fixed = TRUE)[[1]]
  }

  sel <- function(f, d) {
    h <- header_fields(f, d)
    lk <- setNames(h, tolower(h))
    hs <- names(lk)
    vapply(std, function(x) {
      k <- tolower(x)
      if (k %in% hs) paste0(lk[[k]], " AS ", x) else paste0("NULL AS ", x)
    }, character(1))
  }

  ins <- function(f, d, fn, encoding, quote, escape) {
    f2 <- gsub("'", "''", normalizePath(f, winslash = "/", mustWork = FALSE))
    common <- paste0(
      "delim='", gsub("\\\\", "\\\\\\\\", d), "', header=TRUE, nullstr='', ",
      "all_varchar=true, strict_mode=false, ignore_errors=true, null_padding=true, ",
      "max_line_size=10000000, encoding='", encoding, "', quote='", quote, "', escape='", escape, "'"
    )
    DBI::dbExecute(con, paste0(
      "INSERT INTO occ SELECT ", paste(sel(f, d), collapse = ", "),
      " FROM ", fn, "('", f2, "', ", common,
      if (identical(fn, "read_csv")) ", auto_detect=false" else "",
      ");"
    ))
  }

  # load all files; silently skip unreadable ones
  for (f in files) {
    d <- delim1(f)
    last_err <- NULL
    try1 <- function(expr) tryCatch({ expr; TRUE },
                                    error = function(e) { last_err <<- conditionMessage(e); FALSE })

    ok <- FALSE
    ok <- ok || try1(ins(f, d, "read_csv_auto", "utf-8", "\"", "\""))
    ok <- ok || try1(ins(f, d, "read_csv_auto", "latin-1", "\"", "\""))
    ok <- ok || try1(ins(f, d, "read_csv",      "utf-8", "\"", "\""))
    ok <- ok || try1(ins(f, d, "read_csv",      "latin-1", "\"", "\""))
    ok <- ok || try1(ins(f, d, "read_csv_auto", "utf-8", "", ""))
    ok <- ok || try1(ins(f, d, "read_csv",      "utf-8", "", ""))
    invisible(last_err)
  }

  n_focal <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS n FROM occ WHERE upper(collectionCode) = '", gsub("'", "''", focal_herbarium), "';"
  ))$n

  if (!isTRUE(n_focal > 0)) {
    if (isTRUE(save)) .save_xlsx_one_sheet(data.frame(), dir, filename)
    stop(
      "No records for focal_herbarium='", focal_herbarium, "' were loaded.\n",
      "Most likely the focal occurrence.txt was unreadable or had an unexpected delimiter/encoding.\n",
      "Tip: inspect the local directory for that herbarium under path='", path, "'.",
      call. = FALSE
    )
  }

  # Normalize keys + pick best determination string (det_name)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE occ3 AS
    SELECT
      collectionCode, catalogNumber, occurrenceID,
      recordedBy, recordNumber,
      family, genus, specificEpithet, infraspecificEpithet, taxonRank,
      scientificName, acceptedScientificName, verbatimScientificName,
      identifiedBy, dateIdentified,

      COALESCE(NULLIF(acceptedScientificName,''), NULLIF(scientificName,''), NULLIF(verbatimScientificName,'')) AS det_name,

      lower(
        regexp_replace(
          regexp_replace(trim(COALESCE(recordedBy,'')), '\\\\s+', ' ', 'g'),
          '[^a-zA-Z0-9 ]', '', 'g'
        )
      ) AS collector_key_raw,

      lower(regexp_replace(trim(COALESCE(recordNumber,'')), '[^a-zA-Z0-9]', '', 'g')) AS recnum_key_raw,
      lower(trim(COALESCE(recordNumber,''))) AS recordNumber_lc
    FROM occ;
  ")

  # Drop invalid collector / invalid numbers (broad s/n coverage)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE occ_ok_base AS
    SELECT
      *,
      CASE
        WHEN collector_key_raw IS NULL OR collector_key_raw = '' THEN ''
        WHEN collector_key_raw IN ('semcoletor','sem coletor','unknown','desconhecido','na','n/a') THEN ''
        ELSE collector_key_raw
      END AS collector_key,

      CASE
        WHEN recnum_key_raw IS NULL OR recnum_key_raw = '' THEN ''
        WHEN recnum_key_raw IN ('sn','snn','na','n/a','nd','sindado','sindados') THEN ''
        WHEN recordNumber_lc LIKE '%s/n%' THEN ''
        WHEN recordNumber_lc LIKE '%sem numero%' THEN ''
        WHEN recordNumber_lc LIKE '%sem n.%' THEN ''
        WHEN recordNumber_lc LIKE '%sem n %' THEN ''
        ELSE recnum_key_raw
      END AS recnum_key
    FROM occ3;
  ")

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE occ_ok AS
    SELECT
      *,
      collector_key || '|' || recnum_key AS dup_key
    FROM occ_ok_base
    WHERE collector_key <> ''
      AND recnum_key <> ''
      AND det_name IS NOT NULL
      AND det_name <> '';
  ")

  # Canonical determination
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE occ_id AS
    SELECT
      *,
      regexp_replace(trim(det_name), '\\\\s+', ' ', 'g') AS det_name_clean,
      regexp_replace(
        regexp_replace(
          lower(regexp_replace(trim(det_name), '\\\\s+', ' ', 'g')),
          '\\\\b(cf|aff)\\\\.?\\\\b', '', 'g'
        ),
        '\\\\s+', ' ', 'g'
      ) AS det_fallback_base,

      CASE
        WHEN genus IS NOT NULL AND trim(genus) <> ''
         AND specificEpithet IS NOT NULL AND trim(specificEpithet) <> ''
         AND infraspecificEpithet IS NOT NULL AND trim(infraspecificEpithet) <> '' THEN

          CASE
            WHEN lower(trim(specificEpithet)) = lower(trim(infraspecificEpithet)) THEN
              CASE
                WHEN split_part(det_fallback_base, ' ', 1) = '' THEN
                  lower(trim(genus)) || ' ' || lower(trim(specificEpithet)) || ' ' || lower(trim(infraspecificEpithet))
                WHEN split_part(det_fallback_base, ' ', 2) = '' THEN
                  lower(trim(genus)) || ' ' || lower(trim(specificEpithet)) || ' ' || lower(trim(infraspecificEpithet))
                WHEN split_part(det_fallback_base, ' ', 2) IN ('sp','sp.','spp','spp.') THEN
                  lower(trim(genus)) || ' ' || lower(trim(specificEpithet)) || ' ' || lower(trim(infraspecificEpithet))
                ELSE
                  CASE
                    WHEN split_part(det_fallback_base, ' ', 3) IN (
                      'subsp','subsp.','ssp','ssp.',
                      'var','var.','f','f.','forma',
                      'infr','infr.','infrasp','infrasp.'
                    ) THEN
                      CASE
                        WHEN split_part(det_fallback_base, ' ', 4) = '' THEN
                          split_part(det_fallback_base, ' ', 1) || ' ' || split_part(det_fallback_base, ' ', 2)
                        ELSE
                          split_part(det_fallback_base, ' ', 1) || ' ' ||
                          split_part(det_fallback_base, ' ', 2) || ' ' ||
                          split_part(det_fallback_base, ' ', 4)
                      END
                    ELSE
                      split_part(det_fallback_base, ' ', 1) || ' ' || split_part(det_fallback_base, ' ', 2)
                  END
              END
            ELSE
              lower(trim(genus)) || ' ' || lower(trim(specificEpithet)) || ' ' || lower(trim(infraspecificEpithet))
          END

        WHEN genus IS NOT NULL AND trim(genus) <> ''
         AND specificEpithet IS NOT NULL AND trim(specificEpithet) <> '' THEN
          lower(trim(genus)) || ' ' || lower(trim(specificEpithet))

        ELSE
          CASE
            WHEN split_part(det_fallback_base, ' ', 1) = '' THEN NULL
            WHEN split_part(det_fallback_base, ' ', 2) = '' THEN NULL
            WHEN split_part(det_fallback_base, ' ', 2) IN ('sp','sp.','spp','spp.') THEN NULL
            ELSE
              CASE
                WHEN split_part(det_fallback_base, ' ', 3) IN (
                  'subsp','subsp.','ssp','ssp.',
                  'var','var.','f','f.','forma',
                  'infr','infr.','infrasp','infrasp.'
                ) THEN
                  CASE
                    WHEN split_part(det_fallback_base, ' ', 4) = '' THEN
                      split_part(det_fallback_base, ' ', 1) || ' ' || split_part(det_fallback_base, ' ', 2)
                    ELSE
                      split_part(det_fallback_base, ' ', 1) || ' ' ||
                      split_part(det_fallback_base, ' ', 2) || ' ' ||
                      split_part(det_fallback_base, ' ', 4)
                  END
                ELSE
                  split_part(det_fallback_base, ' ', 1) || ' ' || split_part(det_fallback_base, ' ', 2)
              END
          END
      END AS det_canon
    FROM occ_ok;
  ")

  # Optional taxon filter applied ONLY to focal subset
  taxon_sql <- ""
  if (!is.null(taxon)) {
    taxon <- unique(taxon)

    fam <- taxon[grepl("aceae$", taxon, ignore.case = TRUE) |
                   toupper(taxon) %in% c("LEGUMINOSAE","FABACEAE","MIMOSACEAE","CAESALPINIACEAE")]
    gen <- taxon[grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon, ignore.case = TRUE)]
    spp <- taxon[grepl("\\\\s", taxon)]

    cl <- character(0)
    if (length(fam)) cl <- c(cl, paste0("upper(trim(family)) IN ('", paste(gsub("'", "''", toupper(fam)), collapse="','"), "')"))
    if (length(gen)) cl <- c(cl, paste0("upper(trim(genus)) IN ('", paste(gsub("'", "''", toupper(gen)), collapse="','"), "')"))
    if (length(spp)) cl <- c(cl, paste0("lower(trim(det_canon)) IN ('", paste(gsub("'", "''", tolower(spp)), collapse="','"), "')"))
    if (length(cl)) taxon_sql <- paste0(" AND (", paste(cl, collapse=" OR "), ") ")
  }

  # Conflict logic
  q <- paste0("
    WITH focal AS (
      SELECT
        collectionCode AS focal_collectionCode,
        catalogNumber  AS focal_catalogNumber,
        occurrenceID   AS focal_occurrenceID,
        recordedBy     AS focal_recordedBy,
        recordNumber   AS focal_recordNumber,
        family         AS focal_family,
        genus          AS focal_genus,
        det_name       AS focal_det_name,
        det_canon      AS focal_det_canon,
        identifiedBy   AS focal_identifiedBy,
        dateIdentified AS focal_dateIdentified,
        dup_key
      FROM occ_id
      WHERE upper(collectionCode) = '", gsub("'", "''", focal_herbarium), "'
        AND det_canon IS NOT NULL
        AND trim(det_canon) <> ''
        ", taxon_sql, "
    ),
    others AS (
      SELECT
        dup_key,
        collectionCode AS other_collectionCode,
        catalogNumber  AS other_catalogNumber,
        occurrenceID   AS other_occurrenceID,
        family         AS other_family,
        genus          AS other_genus,
        det_name       AS other_det_name,
        det_canon      AS other_det_canon,
        identifiedBy   AS other_identifiedBy,
        dateIdentified AS other_dateIdentified
      FROM occ_id
      WHERE upper(collectionCode) <> '", gsub("'", "''", focal_herbarium), "'
        AND det_canon IS NOT NULL
        AND trim(det_canon) <> ''
    ),
    conflict_keys AS (
      SELECT DISTINCT f.dup_key
      FROM focal f
      JOIN others o USING (dup_key)
      WHERE lower(trim(o.other_det_canon)) <> lower(trim(f.focal_det_canon))
    )
    SELECT
      f.*,
      o.other_collectionCode,
      o.other_catalogNumber,
      o.other_occurrenceID,
      o.other_family,
      o.other_genus,
      o.other_det_name,
      o.other_det_canon,
      o.other_identifiedBy,
      o.other_dateIdentified,
      CASE
        WHEN lower(trim(o.other_det_canon)) = lower(trim(f.focal_det_canon)) THEN 'igual_ao_focal'
        ELSE 'diferente_do_focal'
      END AS comparacao_ao_focal
    FROM focal f
    JOIN others o USING (dup_key)
    JOIN conflict_keys ck USING (dup_key)
    ORDER BY
      f.focal_recordedBy,
      f.focal_recordNumber,
      f.focal_catalogNumber,
      o.other_collectionCode,
      comparacao_ao_focal,
      o.other_det_canon;
  ")

  matches <- DBI::dbGetQuery(con, q)

  if (isTRUE(save)) .save_xlsx_one_sheet(matches, dir, filename)

  list(matches = matches)
}
