test_that("jabot_crossdet (offline) canonicalizes ssp. correctly and returns conflicts", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")

  write_occurrence <- function(dir, rows) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(dir, "occurrence.txt")

    headers <- c(
      "collectionCode","catalogNumber","occurrenceID",
      "recordedBy","recordNumber",
      "family","genus","specificEpithet","infraspecificEpithet","taxonRank",
      "scientificName","acceptedScientificName","verbatimScientificName",
      "identifiedBy","dateIdentified"
    )

    to_chr1 <- function(x) {
      if (is.null(x) || length(x) == 0) return("")
      if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
      if (length(x) == 0) return("")
      x <- x[1]
      if (is.na(x)) return("")
      as.character(x)
    }

    mat <- do.call(rbind, lapply(rows, function(x) {
      v <- setNames(rep("", length(headers)), headers)
      for (nm in intersect(names(x), headers)) {
        v[[nm]] <- to_chr1(x[[nm]])
      }
      v
    }))

    df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    df[] <- lapply(df, as.character) # garantia: sem list-columns

    utils::write.table(
      df, f,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      fileEncoding = "UTF-8"
    )

    f
  }

  tmp <- tempfile("jabot_crossdet_offline_")
  dir.create(tmp, recursive = TRUE)

  rec_by <- "G.S.Z. Resenda; C. Farney & J.C. Gomes"
  rec_no <- "57"

  # Focal RB: structured fields "wrong" (specific==infra),
  # but det_name has "subnuda ssp. luschnathiana"
  rb_row <- list(
    collectionCode = "RB",
    catalogNumber = "RB00333505",
    occurrenceID = "urn:catalog:JBRJ:RB:333505",
    recordedBy = rec_by,
    recordNumber = rec_no,
    family = "Leguminosae",
    genus = "Inga",
    specificEpithet = "luschnathiana",
    infraspecificEpithet = "luschnathiana",
    acceptedScientificName = "Inga subnuda ssp. luschnathiana (Benth.) T.D.Penn.",
    identifiedBy = "R.D. Ribeiro",
    dateIdentified = "2004-06-08"
  )

  # Other HUENF: same determination (equal to focal after canonical fix)
  huenf_row <- list(
    collectionCode = "HUENF",
    catalogNumber = "HUENF00015120",
    occurrenceID = "urn:catalog:HUENF:15120",
    recordedBy = rec_by,
    recordNumber = rec_no,
    family = "FABACEAE",
    genus = "Inga",
    specificEpithet = "subnuda",
    infraspecificEpithet = "luschnathiana",
    acceptedScientificName = "Inga subnuda ssp. luschnathiana",
    identifiedBy = "R.D. Ribeiro",
    dateIdentified = "2004-06-08"
  )

  # Other R: different determination -> forces conflict_keys
  r_row <- list(
    collectionCode = "R",
    catalogNumber = "R010098445",
    occurrenceID = "urn:catalog:R:10098445",
    recordedBy = rec_by,
    recordNumber = rec_no,
    family = "LEGUMINOSAE",
    genus = "Inga",
    specificEpithet = "subnuda",
    infraspecificEpithet = "",
    acceptedScientificName = "Inga subnuda",
    identifiedBy = "R.D. Ribeiro",
    dateIdentified = "2004-06-08"
  )

  write_occurrence(file.path(tmp, "RB"),    list(rb_row))
  write_occurrence(file.path(tmp, "HUENF"), list(huenf_row))
  write_occurrence(file.path(tmp, "R"),     list(r_row))

  out <- jabot_crossdet(
    focal_herbarium = "RB",
    path = tmp,
    save = FALSE,
    dbdir = NULL
  )

  testthat::expect_true(is.list(out))
  testthat::expect_true("matches" %in% names(out))
  m <- out$matches
  testthat::expect_s3_class(m, "data.frame")
  testthat::expect_true(nrow(m) >= 2)

  # focal canon repaired via fallback; marker ssp. must not appear
  testthat::expect_true(all(m$focal_det_canon == "inga subnuda luschnathiana"))
  testthat::expect_false(any(grepl("\\bssp\\b|\\bsubsp\\b|\\bvar\\b|\\binfr\\b", m$focal_det_canon)))

  # HUENF equals focal; R differs
  testthat::expect_true(any(m$other_collectionCode == "HUENF" & m$comparacao_ao_focal == "igual_ao_focal"))
  testthat::expect_true(any(m$other_collectionCode == "R"     & m$comparacao_ao_focal == "diferente_do_focal"))
})


test_that("jabot_crossdet (offline) taxon filter works for family and genus", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")

  write_occurrence <- function(dir, rows) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(dir, "occurrence.txt")

    headers <- c(
      "collectionCode","catalogNumber","occurrenceID",
      "recordedBy","recordNumber",
      "family","genus","specificEpithet","infraspecificEpithet","taxonRank",
      "scientificName","acceptedScientificName","verbatimScientificName",
      "identifiedBy","dateIdentified"
    )

    to_chr1 <- function(x) {
      if (is.null(x) || length(x) == 0) return("")
      if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
      if (length(x) == 0) return("")
      x <- x[1]
      if (is.na(x)) return("")
      as.character(x)
    }

    mat <- do.call(rbind, lapply(rows, function(x) {
      v <- setNames(rep("", length(headers)), headers)
      for (nm in intersect(names(x), headers)) {
        v[[nm]] <- to_chr1(x[[nm]])
      }
      v
    }))

    df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    df[] <- lapply(df, as.character)

    utils::write.table(
      df, f,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      fileEncoding = "UTF-8"
    )

    f
  }

  tmp <- tempfile("jabot_crossdet_taxon_offline_")
  dir.create(tmp, recursive = TRUE)

  rec_by <- "Collector A"
  rec_no <- "1"

  rb <- list(
    collectionCode="RB", catalogNumber="RB1", occurrenceID="RB:1",
    recordedBy=rec_by, recordNumber=rec_no,
    family="Fabaceae", genus="Inga",
    specificEpithet="subnuda", infraspecificEpithet="luschnathiana",
    acceptedScientificName="Inga subnuda ssp. luschnathiana",
    identifiedBy="X", dateIdentified="2001-01-01"
  )

  other_diff <- list(
    collectionCode="R", catalogNumber="R1", occurrenceID="R:1",
    recordedBy=rec_by, recordNumber=rec_no,
    family="Fabaceae", genus="Inga",
    specificEpithet="subnuda", infraspecificEpithet="",
    acceptedScientificName="Inga subnuda",
    identifiedBy="Y", dateIdentified="2001-01-02"
  )

  write_occurrence(file.path(tmp,"RB"), list(rb))
  write_occurrence(file.path(tmp,"R"),  list(other_diff))

  out_fam <- jabot_crossdet("RB", taxon="Fabaceae", path=tmp, save=FALSE)
  testthat::expect_true(nrow(out_fam$matches) > 0)

  out_gen <- jabot_crossdet("RB", taxon="Inga", path=tmp, save=FALSE)
  testthat::expect_true(nrow(out_gen$matches) > 0)

  out_other <- jabot_crossdet("RB", taxon="Poaceae", path=tmp, save=FALSE)
  testthat::expect_equal(nrow(out_other$matches), 0)
})


test_that("jabot_crossdet (offline) errors when no occurrence.txt is found", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")

  tmp <- tempfile("jabot_crossdet_empty_")
  dir.create(tmp, recursive = TRUE)

  testthat::expect_error(
    jabot_crossdet("RB", path = tmp, save = FALSE),
    "No 'occurrence.txt' found",
    fixed = FALSE
  )
})

