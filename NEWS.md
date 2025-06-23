# jabotR 1.0.0

## Initial Release

The first official release of the `jabotR` R package, designed to streamline access to plant specimen data from the JABOT hosted by the Rio de Janeiro Botanical Garden.

### Features

- `jabot_summary()`: Retrieve metadata and summary info from all or specific JABOT herbaria.
- `jabot_download()`: Download original specimen data in Darwin Core Archive (DwC-A) format.
- `jabot_records()`: Parse, filter, and organize JABOT records based on taxon, herbarium, region, and year.
- `jabot_indets()`: Retrieve indeterminate specimens (e.g., identified only to family or genus rank).
- Optional filters by `taxon`, `herbarium`, `state`, `recordYear`, and `level`.
- Supports integration with tidyverse workflows for downstream analyses.
- Test coverage >95%, continuous integration via GitHub Actions.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check and test coverage.
- Hosted documentation: [jabotR-website](https://dboslab.github.io/jabotR-website/)

### Feedback

Please report bugs or issues at:  
<https://github.com/DBOSlab/refloraR/issues>
