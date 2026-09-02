
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jabotR <img src="inst/figures/jabotr_hex_sticker.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/DBOSlab/jabotR/graph/badge.svg)](https://app.codecov.io/gh/DBOSlab/jabotR)
[![Test
Coverage](https://github.com/DBOSlab/jabotR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/DBOSlab/jabotR/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/DBOSlab/jabotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DBOSlab/jabotR/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
<!-- badges: end -->

`jabotR` is an R package for accessing and analyzing plant specimen data
from the [JABOT online herbarium
collections](https://jabot.jbrj.gov.br/v3/consulta.php), hosted by the
[Rio de Janeiro Botanical Garden](https://www.gov.br/jbrj/pt-br). It
provides tools for summarizing JABOT collections, downloading and
parsing specimen records in Darwin Core Archive (DwC-A) format,
retrieving and filtering occurrence records, identifying indeterminate
specimens, evaluating taxonomic gaps, collection representativeness, and
regionally missing species with collecting-priority maps against the
Flora e Funga do Brasil (FFB), and organizing new field data with an
offline field notebook that exports ready-to-use JABOT spreadsheets.

## Installation

You can install the development version of `jabotR` from
[GitHub](https://github.com/DBOSlab/jabotR) with:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) 
install.packages("BiocManager") 

# Install the development version of jabotR from GitHub, 
# together with its required dependencies 
BiocManager::install("DBOSlab/jabotR", dependencies = TRUE)
```

``` r
library(jabotR)
```

  
  

## Usage

A general description of the main functions available in `jabotR` is
provided below. These functions support a workflow from summarizing and
downloading original JABOT collections to parsing specimen records,
filtering occurrence data, retrieving indeterminate specimens,
evaluating taxonomic gaps, herbarium representativeness, and regionally
missing species with collecting-priority maps, and organizing new field
data for submission to JABOT.  
  

#### *1. `jabot_summary`: Summarizing JABOT collections*

The following code can be used to extract a summary of all
JABOT-associated collections, including herbarium acronym, curator’s
email contact, number of records and a direct link to the original JABOT
Integrated Publishing Toolkit ([IPT](https://ipt.jbrj.gov.br/jabot)).  

``` r
library(jabotR)

summary_df <- jabot_summary(verbose = TRUE,
                            save = TRUE,
                            dir = "jabot_summary")
```

  
By specifying a vector of herbarium acronyms, the user can extract a
summary for just the specific herbarium collections.  

``` r
summary_some_df <- jabot_summary(herbarium = c("AFR", "R", "RB"),
                                 verbose = TRUE,
                                 save = TRUE,
                                 dir = "jabot_summary")
```

  
  

#### *2. `jabot_download`: Downloading JABOT specimen records*

The following code can be used to download original specimen records in
DwC-A format and associated metadata for all JABOT collections.  

``` r
library(jabotR)

jabot_download(verbose = TRUE,
               dir = "jabot_download")
```

  
By specifying a vector of herbarium acronyms, the user can download
specimen records for just the specific herbarium collections.  

``` r
jabot_download(herbarium = c("AFR", "RB", "R"),
               verbose = TRUE,
               dir = "jabot_download")
```

  
The downloaded DwC-A folders can subsequently be parsed with
`jabot_parse()` or reused by other `jabotR` functions.  
  

#### *3. `jabot_parse`: Parsing downloaded DwC-A files*

`jabot_parse()` reads JABOT Darwin Core Archive folders and converts
their occurrence records and associated metadata into R objects. It is
useful when DwC-A files have already been downloaded using
`jabot_download()` and the user wants to inspect or manipulate the
original data directly.  

The following code parses all DwC-A folders located in the
`"jabot_download"` directory.  

``` r
dwca <- jabot_parse(path = "jabot_download",
                    verbose = TRUE)
```

  
Specific herbarium collections can also be selected.  

``` r
dwca_some <- jabot_parse(path = "jabot_download",
                         herbarium = c("AFR", "RB"),
                         verbose = TRUE)
```

  
The function returns a named list containing parsed DwC-A data and
associated metadata for each collection.  
  

#### *4. `jabot_records`: Retrieving and filtering specimen records*

`jabot_records()` retrieves occurrence records from JABOT and allows
users to filter specimens by herbarium, taxon, Brazilian state and
collection year. The function can automatically download and parse the
required DwC-A files or reuse files that have already been downloaded.  

For example, Fabaceae records from selected JABOT collections can be
retrieved with:  

``` r
fabaceae_records <- jabot_records(herbarium = c("AFR", "R", "RB"),
                                  taxon = "Fabaceae",
                                  verbose = TRUE,
                                  save = FALSE)
```

  
Taxonomic, geographic and temporal filters can be combined.  

``` r
filtered_records <- jabot_records(herbarium = "RB",
                                  taxon = "Fabaceae",
                                  state = c("Bahia", "Minas Gerais"),
                                  recordYear = c("2000", "2024"),
                                  verbose = TRUE,
                                  save = FALSE)
```

  
Previously downloaded DwC-A files can be reused by defining `path` and
setting `updates = FALSE`.  

``` r
filtered_records <- jabot_records(herbarium = "RB",
                                  taxon = "Fabaceae",
                                  path = "jabot_download",
                                  updates = FALSE,
                                  verbose = TRUE,
                                  save = FALSE)
```

  
By default, indeterminate specimens are retained. Setting
`indets = FALSE` removes records that are not identified to species
level.  

``` r
species_records <- jabot_records(herbarium = "RB",
                                 taxon = "Fabaceae",
                                 indets = FALSE,
                                 verbose = TRUE,
                                 save = FALSE)
```

  
When `save = TRUE`, the retrieved records are saved as a CSV file and a
`log.txt` file containing summary information is generated in the output
directory.  
  

#### *5. `jabot_indets`: Retrieving indeterminate specimens*

`jabot_indets()` retrieves JABOT occurrence records for specimens that
are not identified to species level. The function can be used to
identify specimens determined only to higher taxonomic ranks,
particularly family or genus, which can be useful for collection
management and taxonomic curation.  

For example, family-level indeterminate Fabaceae records can be
retrieved with:  

``` r
family_indets <- jabot_indets(level = "FAMILY",
                              herbarium = "RB",
                              taxon = "Fabaceae",
                              verbose = TRUE,
                              save = FALSE)
```

  
Genus-level indeterminate records can be retrieved with:  

``` r
genus_indets <- jabot_indets(level = "GENUS",
                             herbarium = "RB",
                             taxon = "Fabaceae",
                             verbose = TRUE,
                             save = FALSE)
```

  
Geographic and temporal filters can also be applied.  

``` r
filtered_indets <- jabot_indets(level = "FAMILY",
                                herbarium = "RB",
                                taxon = "Fabaceae",
                                state = c("Bahia", "Minas Gerais"),
                                recordYear = c("2000", "2024"),
                                verbose = TRUE,
                                save = FALSE)
```

  
When `level = NULL`, all supported higher-rank indeterminate records are
retained. Previously downloaded JABOT DwC-A files can also be reused.  

``` r
family_indets <- jabot_indets(level = "FAMILY",
                              herbarium = "RB",
                              taxon = "Fabaceae",
                              path = "jabot_download",
                              updates = FALSE,
                              verbose = TRUE,
                              save = FALSE)
```

  
  

#### *6. `jabot_gaps`: Identifying collection gaps against Flora e Funga do Brasil*

`jabot_gaps()` identifies species listed in the [Flora e Funga do Brasil
(FFB)](https://floradobrasil.jbrj.gov.br/consulta/) that are absent from
a JABOT-hosted herbarium collection. The function retrieves or reads
previously downloaded JABOT data, standardizes synonym names against
FFB, and compares the species represented in the target herbarium with
the accepted species recognized by FFB.  

The analysis includes overall summary statistics, missing taxa by
taxonomic group, family and genus, genera entirely absent from the
herbarium, missing taxa by phytogeographic domain, missing endemic
species, geographic gaps by Brazilian state, graphical summaries and an
HTML report.  

A gap analysis can be performed with:  

``` r
gap_analysis <- jabot_gaps(herbarium = "RB",
                           verbose = TRUE,
                           open_report = TRUE,
                           dir = "RB_gap_analysis")
```

  
Individual figures can additionally be exported as PDF, PNG, or both.  

``` r
gap_analysis <- jabot_gaps(herbarium = "RB",
                           format = c("pdf", "png"),
                           verbose = TRUE,
                           open_report = TRUE,
                           dir = "RB_gap_analysis")
```

  
Previously downloaded JABOT DwC-A files can be reused with
`jabot_path`.  

``` r
gap_analysis <- jabot_gaps(herbarium = "RB",
                           jabot_path = "jabot_download",
                           format = "pdf",
                           verbose = TRUE,
                           open_report = TRUE,
                           dir = "RB_gap_analysis")
```

  
Setting `priority_map = TRUE` additionally downloads pooled JABOT
network records (see `network_herbaria` to restrict the sample) and adds
a nationwide municipality-level heat map ranking collecting priority for
the herbarium’s missing species — i.e. which municipalities already hold
the most known vouchers of species still missing from the herbarium (see
`jabot_missing()` below for a region-scoped version of this analysis).  

``` r
gap_analysis <- jabot_gaps(herbarium = "RB",
                           priority_map = TRUE,
                           verbose = TRUE,
                           open_report = TRUE,
                           dir = "RB_gap_analysis")
```

  
The HTML report is always generated. When `format` is specified,
individual figures are additionally saved in the requested static
format.  
  

#### *7. `jabot_coverage`: Measuring herbarium representativeness*

`jabot_coverage()` measures how well a JABOT-hosted herbarium represents
the species diversity recognized by the Flora e Funga do Brasil. Unlike
`jabot_gaps()`, which focuses on species that are missing from a
collection, `jabot_coverage()` characterizes the species that are
represented.  

Overall coverage is calculated as the number of FFB species represented
in the herbarium divided by the total number of FFB species, multiplied
by 100.  

The analysis includes overall FFB species coverage, coverage by
taxonomic group, family, phytogeographic domain and Brazilian state,
coverage of endemic and non-endemic species, representation of type
material, graphical summaries and an HTML report.  

A representativeness analysis can be performed with:  

``` r
coverage_analysis <- jabot_coverage(herbarium = "RB",
                                    verbose = TRUE,
                                    open_report = TRUE,
                                    dir = "RB_representativeness")
```

  
Individual figures can additionally be exported as PDF, PNG, or both.  

``` r
coverage_analysis <- jabot_coverage(herbarium = "RB",
                                    format = c("pdf", "png"),
                                    verbose = TRUE,
                                    open_report = TRUE,
                                    dir = "RB_representativeness")
```

  
Previously downloaded JABOT DwC-A files can be reused with
`jabot_path`.  

``` r
coverage_analysis <- jabot_coverage(herbarium = "RB",
                                    jabot_path = "jabot_download",
                                    format = "pdf",
                                    verbose = TRUE,
                                    open_report = TRUE,
                                    dir = "RB_representativeness")
```

  
The HTML report is always generated. When `format` is specified,
individual figures are additionally saved as PDF or PNG files.  
  

#### *8. `jabot_missing`: Locating missing species and collecting priorities within a target region*

`jabot_missing()` identifies species listed in the Flora e Funga do
Brasil (FFB) that are expected to occur in a target Brazilian state
and/or municipality (`geo`) but are absent from a given JABOT-hosted
herbarium (`herbarium`), regardless of where in Brazil that herbarium’s
own specimens were collected. Unlike `jabot_gaps()`, which compares a
herbarium against the whole of Brazil, `jabot_missing()` narrows the
comparison to a specific region and searches the pooled occurrence
records of the JABOT network (all herbaria, or a subset given by
`network_herbaria`) for existing vouchers of the missing species
collected within that region — helping prioritize where new collecting
expeditions could target them.

The analysis includes summary statistics, a ranked table of missing
species with known network vouchers and municipalities, missing species
by family, an interactive map of candidate collecting localities, a
municipality-level heat map ranking collecting priority, a full
record-level table of every individual network voucher (each linking out
to the physical specimen when the source herbarium provides one),
graphical summaries and an HTML report.

A missing-species analysis for Bahia state can be performed with:  

``` r
missing_analysis <- jabot_missing(herbarium = "RB",
                                  geo = "Bahia",
                                  verbose = TRUE,
                                  open_report = TRUE,
                                  dir = "RB_missing_Bahia")
```

  
The search can instead be narrowed to one or more municipalities; FFB’s
state-level checklist is resolved from the state(s) intersecting `geo`,
while the candidate collecting map stays restricted to the given
municipalities.  

``` r
missing_analysis <- jabot_missing(herbarium = "RB",
                                  geo = c("Feira de Santana", "Ilhéus"),
                                  verbose = TRUE,
                                  open_report = TRUE)
```

  
By default, the pooled network sample includes all JABOT-hosted
herbaria. `network_herbaria` restricts the candidate collecting-locality
search to a specific subset, individual figures can additionally be
exported as PDF, PNG, or both, and previously downloaded JABOT DwC-A
files can be reused with `jabot_path`.  

``` r
missing_analysis <- jabot_missing(herbarium = "RB",
                                  geo = "Bahia",
                                  network_herbaria = c("ALCB", "VIES"),
                                  jabot_path = "jabot_download",
                                  format = "pdf",
                                  verbose = TRUE,
                                  open_report = TRUE)
```

  
The HTML report is always generated. When `format` is specified,
individual figures are additionally saved in the requested static
format.  
  

#### *9. `jabot_fieldbook`: Organizing field data and generating JABOT spreadsheets*

`jabot_fieldbook()` opens an interactive field notebook, distributed
with the package, in the user’s default web browser. It works entirely
offline (no Shiny, no internet connection) and lets users organize
collection-event and specimen data, export a standardized 58-column
JABOT spreadsheet ready for submission, and generate a printable field
notebook in PDF format. The interface is available in Portuguese and
English.

``` r
library(jabotR)

jabot_fieldbook()
```

  
Setting `browser = FALSE` returns the local path to the application
instead of opening it.

``` r
app_path <- jabot_fieldbook(browser = FALSE)
```

  
Within the field notebook, data are organized by collection event, each
with a shared header (date, locality, coordinates, habitat, collectors)
and one or more specimens. Drafts can be saved to and reloaded from the
browser at any time, and the completed data can be exported as an empty
template, a filled JABOT spreadsheet, or a printable field notebook
(PDF). See the [How-To
article](https://dboslab.github.io/jabotR-website/articles/jabot_fieldbook.html)
for a complete walkthrough.  
  

## Documentation

Full function documentation and articles are available at the `jabotR`
[website](https://dboslab.github.io/jabotR-website/).  
  

## Citation

Cardoso, D., Ottino, G.C., Versiane, A.F., Forzza, R.C. & Silva, L.A.E.
2026. *jabotR*: An R Package for Exploring JABOT Online Plant Specimen
Collections. <https://github.com/dboslab/jabotR>
