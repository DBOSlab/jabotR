# jabotR 1.0.0

## Initial Release

The first official release of the `jabotR` R package, designed to streamline access to plant specimen data from the [JABOT online herbarium collections](https://jabot.jbrj.gov.br/v3/consulta.php), hosted by the [Rio de Janeiro Botanical Garden](https://www.gov.br/jbrj/pt-br).

### Features

- `jabot_summary()`: Retrieve metadata and summary info from all or specific JABOT herbaria.
- `jabot_download()`: Download original specimen data in Darwin Core Archive (DwC-A) format.
- `jabot_parse()`: Read and parse downloaded Darwin Core Archive files into R.
- `jabot_records()`: Retrieve, filter, and organize JABOT specimen records by taxon, herbarium, region, and year.
- `jabot_indets()`: Retrieve indeterminate specimens (e.g., identified only to family or genus rank).
- `jabot_gaps()`: Identify taxa missing from a herbarium relative to the Flora e Funga do Brasil, with an optional nationwide, municipality-level collecting-priority heat map (`priority_map`).
- `jabot_coverage()`: Measure how well a herbarium represents the Flora e Funga do Brasil, by taxonomic group, family, phytogeographic domain, and Brazilian state.
- `jabot_missing()`: Identify species expected in a target Brazilian state/municipality that are missing from a local herbarium, with an interactive map of candidate collecting localities and a municipality-level collecting-priority heat map drawn from the pooled JABOT network — including direct links to physical specimen images when available.
- `jabot_identify()`: Suggest identifications for the indeterminate specimens of a herbarium (lacking family, genus, or specific epithet) by finding their duplicates in the whole JABOT network and in REFLORA (via `refloraR`) with `barRoso`, with an optional `taxon` filter, support for REFLORA-only herbaria, and an interactive HTML report listing the suggested identification, its source herbarium and specimen link, and conflicts between duplicates.
- `jabot_fieldbook()`: Open an offline field notebook in the browser to organize collection events and specimens and export a standardized 58-column JABOT spreadsheet and a printable field notebook in PDF format.
- `jabot_gaps()`, `jabot_coverage()`, and `jabot_missing()` each produce a self-contained, interactive HTML report.
- Optional filters by `taxon`, `herbarium`, `state`, `recordYear`, and `level`.
- Supports integration with tidyverse workflows for downstream analyses.
- Comprehensive testthat suite, continuous integration via GitHub Actions.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check and test coverage.
- Hosted documentation: [jabotR-website](https://dboslab.github.io/jabotR-website/), with How-To articles for every function and a full Portuguese translation (EN/PT language switcher).

### Feedback

Please report bugs or issues at:  
<https://github.com/DBOSlab/jabotR/issues>
