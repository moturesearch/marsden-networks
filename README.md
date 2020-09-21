# marsden-networks

This repository contains the source material for "Research Funding and Collaboration."

## Workflow

We generate figures and tables by running [`code/analysis.R`](code/analysis.R) in an `marsden-networks.Rproj` instance within [RStudio](https://www.rstudio.com/).

### Data

Our analysis combines [Scopus](https://www.scopus.com) publication records with data on [Marsden Fund](https://www.royalsociety.org.nz/what-we-do/funds-and-opportunities/marsden) proposals.
We exclude our data from this repository because they contain sensitive information about Marsden Fund applicants.

Our combined ("linked") data form a relational database.
We describe the files in that database in the tables below.

#### Files

File | Variables | Description
--- | --- | ---
`asjc_crosswalk` | `field`, `field_desc`, `field_group`, `field_group_desc`, `subject_area`, `subject_area_desc` | Crosswalk between All Science Journal Classification (ASJC) fields, field groups, and subject areas
`authorships` | `aff_country`, `prsn`, `pub` | Table linking researchers in linked data to their publications and countries of affiliation
`authorships_full` | `aff_country`, `auth`, `pub` | Table linking Scopus authors in raw data to their publications and countries of affiliation
`countries` | `aff_country`, `aff_country_desc`, `aff_country_tag` | Table of country of affiliation codes, names, and tags
`matches` | `auth`, `prsn` | Table of applicant-author pairs included in linked data
`matches_full` | `auth`, `prsn`, `source` | Table linking matched applicant-author pairs to match sources
`mean_citations` | `field`, `mean_n_citations`, `pub_year` | Global mean citations accrued to Scopus publications, aggregated by publication year and ASJC field
`people` | `prsn`, `prsn_forename`, `prsn_gender`, `prsn_initials`, `prsn_is_maori` `prsn_is_nzl_based`, `prsn_surname` | Table of attributes of researchers included in linked data
`people_full` | See `people` | Table of attributes of Marsden Fund applicants included in raw data
`proposals` | `prop`, `prop_is_contracted`, `prop_is_fast_start`, `prop_max_round`, `prop_year` | Table of attributes of Marsden Fund proposals linked to researchers in linked data
`proposals_full` | See `proposals` | Table of attributes of Marsden Fund proposals in raw data
`publications` | `pub`, `pub_n_auth`, `pub_n_citations`, `pub_n_nzl_auth` `pub_type`, `pub_year` | Table of attributes of Scopus publications included in linked data
`publications_full` | See `publications` | Table of attributes of Scopus publications included in raw data
`publication_fields` | `field`, `pub` | Crosswalk between Scopus publication IDs and ASJC field codes for publications included in linked data
`publication_fields_full` | See `publication_fields` | Crosswalk between Scopus publication IDs and ASJC field codes for all publications in raw data
`publication_types` | `pub_type`, `pub_type_code`, `pub_type_desc` | Table of publication type descriptors
`teams` | `fte`, `prop`, `prsn`, `role`, `round` | Table linking researchers in linked data to Marsden Fund proposals
`teams_full` | See `teams` | Table linking Marsden Fund applicants in raw data to proposals

#### Variables

Variable | Type | Description
--- | --- | ---
`aff_country` | int | Country of affiliation ID
`aff_country_desc` | int | Country of affiliation name
`aff_country_tag` | int | Country of affiliation tag
`auth` | dbl | Scopus author ID
`field` | int | ASJC field ID
`field_desc` | chr | ASJC field description
`field_group` | int | ASJC field group ID
`field_group_desc` | chr | ASJC field group description
`fte` | dbl | Person's FTE budget on funding contract
`mean_n_citations` | int | Mean number of citations for publications in field `field` and year `pub_year`
`prop` | chr | Proposal ID
`prop_is_contracted` | int | Binary indicator for whether proposal led to a funding contract
`prop_is_fast_start` | int | Binary indicator for whether proposal was for a Fast Start grant
`prop_max_round` | int | Maximum round reached by proposal (see `round`)
`prop_year` | int | Proposal year
`prsn` | chr | Person ID
`prsn_forename` | chr | Person forename
`prsn_gender` | chr | Person gender
`prsn_initials` | chr | Person initials
`prsn_is_maori` | int | Binary indicator for whether person identifies as Māori
`prsn_is_nzl_based` | int | Binary indicator for whether person was based at a New Zealand institution at time of most recent proposal
`prsn_surname` | chr | Person surname
`pub` | dbl | Publication ID
`pub_n_auth` | int | Number of Scopus author IDs linked to publication
`pub_n_citations` | int | Number of citations accrued to publication
`pub_n_nzl_auth` | int | Number of Scopus author IDs with New Zealand affiliations linked to publication
`pub_type` | int | Publication type ID
`pub_type_code` | chr | Publication type code
`pub_type_desc` | chr | Publication type description
`pub_year` | int | Publication year
`role` | chr | Person's role on proposal team
`round` | int | Marsden application round (0 = First, 1 = Second, 2 = Funded)
`source` | chr | Match source
`subject_area` | int | ASJC subject area ID
`subject_area_desc` | chr | Subject area description

### Dependencies

Our analysis uses several R packages.
We identify these packages at the beginning of `code/analysis.R` and in `logs/analysis.log`.
All dependencies can be installed by running

```r
pkgs <- c(
  'alpaca',
  'conflicted',
  'feather',
  'knitr',
  'margins',
  'MASS',
  'Matrix',
  'sandwich',
  'sessioninfo',
  'tidyverse',
  'texreg',
  'widyr'
)
install.packages(pkgs)
```

at the R console.

## License

[MIT](LICENSE)
