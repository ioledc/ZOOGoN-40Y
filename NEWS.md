# ZooGoN 5.2.0

## Automation

* **New CI job `ingest-legacy-data`**: Added a GitHub Actions job that runs
  `ingest_legacy_84_15()` and `ingest_legacy_16_20()` on every pipeline
  trigger. The `merge-data` job now depends on both `ingest-legacy-data` and
  `process-kobo-surveys`, ensuring legacy and ongoing data are always refreshed
  before merging.

## Storage refactor

* **`ingest_legacy_84_15()` and `ingest_legacy_16_20()`**: Outputs are now
  written to the `automation_bucket` using versioned file prefixes defined in
  `inst/config.yml` (`legacy-84-15` and `legacy-16-20`). Previously, files
  were written locally and then uploaded to the separate
  `legacy_data_processed` bucket with hardcoded filenames (`McZoo_84-15`,
  `McZoo_16-20`). The upload now uses the standard pipe-walk pattern
  consistent with the rest of the pipeline.

* **`format_to_tidy()`**: Now reads legacy datasets from the `automation_bucket`
  using the config-defined prefixes, replacing the previous approach of reading
  hardcoded filenames from the `legacy_bucket_processed` bucket.

* **`inst/config.yml`**: Added `ingestion.legacy_84_15.file_prefix`
  (`"legacy-84-15"`) and `ingestion.legacy_16_20.file_prefix`
  (`"legacy-16-20"`). Removed the now-unused `legacy_bucket_processed` bucket
  entry.

# ZooGoN 5.1.0

## Automation

* **New workflow `gbif-register-test.yaml`**: Monthly GitHub Actions workflow
  (1st of each month) that calls `register_gbif_dataset_test()` with all
  credentials supplied as GitHub secrets.

## GBIF registration refactor

* **`register_gbif_dataset()` and `register_gbif_dataset_test()`**: All
  credentials, org/install keys, and endpoint URL are now read from
  `inst/config.yml` via `read_config()`, consistent with the rest of the
  pipeline. The corresponding parameters (`endpoint_url`, `organization_key`,
  `installation_key`, `username`, `password`) have been removed from both
  function signatures. Set `GBIF_ENDPOINT_URL`, `GBIF_ORG_KEY`,
  `GBIF_INSTALL_KEY`, `GBIF_USERNAME`, `GBIF_PASSWORD` (production) or
  `GBIF_TEST_*` equivalents (test) as environment variables.

* **`register_gbif_dataset()`**: Aligned with `register_gbif_dataset_test()`
  to use the two-call pattern — dataset creation followed by a separate
  endpoint registration call. Both functions now return
  `list(dataset_key, registration, endpoint)`.

* **`inst/config.yml`**: Added `gbif.test` and `gbif.production` sections,
  each reading base URL, credentials, org/install keys, and endpoint URL from
  environment variables via `!expr Sys.getenv(...)`.

# ZooGoN 5.0.0

## Report

* **Seasonal Pattern tab**: Added a new chart tab in the Taxon Explorer showing
  mean abundance (ind/m³) by week of year, with ±1 standard deviation shading
  to represent inter-annual variability in seasonal timing and intensity.

* **Temporal Signals section**: New section presenting time-series feature
  analysis (STL decomposition components, spectral entropy, Hurst exponent,
  lumpiness, stability, and maximum level shift) for taxa present in ≥ 50% of
  monthly time points from 1995 onwards. Results are displayed as an
  interactive `reactable` table with heatmap-style column colouring.

## Bug fixes

* **`render_report()`**: The rendered HTML is now uploaded to the SharePoint
  `reports` bucket. Previously the function only saved the file locally; the
  reports bucket was never written to.

* **`render_report()`**: Output filename now uses `add_version()` (timestamp +
  git SHA), consistent with all other pipeline outputs. Previously `Sys.Date()`
  was used, producing a different naming convention.

* **`render_report()`**: Removed `execute_params` from the `quarto_render()`
  call. These were silently ignored because the report reads configuration
  directly via `read_config()`; one of the keys also pointed to a non-existent
  config path (`$buckets$reports` instead of `$buckets$reports_bucket`).

* **`inst/config.yml`**: Fixed the `production` override. The `buckets:`
  nesting level was missing, so `automation_bucket` was never switched to
  `zoogon_mc-prod` on the main branch — all production runs were silently using
  the dev bucket.

* **`R/kobo-processing.R`**: Column selection in `preprocess_surveys()` now
  uses `dplyr::any_of()`, avoiding errors when optional columns are absent from
  a given survey batch.

## Configuration

* **`inst/config.yml`**: Added `ingestion.reports.file_prefix`
  (`"interactive-report"`) to give the rendered report a configurable prefix,
  consistent with other pipeline artefacts.

* **`inst/config.yml`**: Removed the unused `hot_bucket` entry.

# ZooGoN 4.1.0

## Bug fixes and data quality improvements

* **`preprocess_surveys()`**: Added an explicit AphiaID remapping table that
  replaces 16 unaccepted WoRMS taxon identifiers with their accepted equivalents
  before any downstream processing. Previously, unaccepted IDs from KoboToolbox
  submissions were passed through unchanged and resolved only later (or not at
  all).

* **`preprocess_surveys()`**: NA AphiaIDs now raise an informative error listing
  the affected `event_id` values instead of silently dropping those rows. This
  makes data-quality issues in incoming survey submissions immediately visible.

* **`ingest_legacy_84_15()`** and **`ingest_legacy_16_20()`**: Improved
  resolution of unaccepted WoRMS names. The functions now retain `valid_AphiaID`
  and `valid_name` from the WoRMS match and explicitly overwrite `AphiaID`,
  `scientificName`, and `lsid` with accepted values for any taxon whose status
  is `"unaccepted"`. Deduplication is then performed by `slice_min(AphiaID)`
  within each (sample, taxon, life-stage) group, replacing the previous strategy
  of sorting by `AphiaID` and taking the first row.

# ZooGoN 4.0.0

## Breaking changes

* **`raw_to_tidy()`** renamed to **`format_to_tidy()`**.
* **`raw_to_dc()`** renamed to **`format_to_dc()`**.
* **`dc_to_archive()`** renamed to **`format_to_DC_archive()`**.

## New functions

* **`ingest_legacy_84_15()`**: downloads, validates, and harmonises the
  1984–2015 historical zooplankton dataset, including WoRMS taxonomic lookups
  and life-stage code standardisation. Outputs `McZoo_84-15` to SharePoint.
* **`ingest_legacy_16_20()`**: same workflow as above for the 2016–2020 legacy
  dataset. Outputs `McZoo_16-20` to SharePoint.
* **`upload_sharepoint_file()`**: uploads an arbitrary local file to SharePoint
  (complements the existing `upload_sharepoint_df()` for data frames).

## Code organisation

* Survey ingestion logic moved to `R/kobo-ingestion.R`.
* Survey preprocessing logic moved to `R/kobo-processing.R`.
* Legacy data ingestion logic consolidated in `R/legacy-data-ingestion.R`.

## Documentation

* Vignettes updated to reflect renamed functions and new legacy-ingestion steps.
* Added plain-language introductions to both vignettes for a non-technical
  (biology/ecology) audience.
* pkgdown reference index reorganised: added "Legacy Data Ingestion",
  "Pipeline & Reporting" sections; all renamed functions updated.

# ZooGoN 3.1.0

## Full Automated Pipeline

This release completes the end-to-end data pipeline from field survey collection to Darwin Core Archive publication.

### New Functions

* **`raw_to_tidy()`**: Merges legacy datasets (1984-2020) with preprocessed ongoing surveys into a single analysis-ready dataset. Uploads the merged data in both CSV and Parquet formats to SharePoint.

### Pipeline & Automation

* Complete 6-step GitHub Actions pipeline: build container → ingest surveys → preprocess → merge tidy data → Darwin Core conversion → build archive.
* `raw_to_dc()` now reads from the tidy data output (produced by `raw_to_tidy()`) instead of merging legacy files internally. Results are uploaded to SharePoint as versioned RDS.
* `dc_to_archive()` no longer takes arguments — it downloads Darwin Core tables from SharePoint automatically.

### Storage Improvements

* **Large file upload support**: Files larger than 4 MB are uploaded via Microsoft Graph API resumable upload sessions with chunked transfer (10 MB chunks).
* **RDS format support** in `upload_sharepoint_df()`, `download_sharepoint_file()`, and related helpers.

### Production Hardening

* Fixed Dockerfile.prod syntax (`install2.r` does not accept version specs) and added a library validation step.
* Bumped minimum R version to `R (>= 4.1)` (required for the native pipe `|>`).
* Fixed scalar logical operators (`||` instead of `|`) in KoboToolbox validation.
* Internal helpers (`flatten_row`, `flatten_field`, `rename_child`) are no longer exported.

### Documentation

* Updated README, data-processing vignette, and darwin-core vignette to reflect the full pipeline workflow.
* Reorganised pkgdown reference index with a "Data Processing" section.

# ZooGoN 3.0.0

- Added automated GBIF publishing helpers: `register_gbif_dataset()` for production use and `register_gbif_dataset_test()` for the GBIF-Test demo flow; both take a public DwC-A URL and handle dataset registration.
- Simplified Darwin Core export: `dc_to_archive()` now builds the DwC-A + EML from `raw_to_dc()` output and uploads the archive to SharePoint.
- Clarified EML generation (`get_metadata()`) and cleaned up pkgdown reference sections.

# ZooGoN 2.0.0

## Automation & Cloud Integration

This release introduces cloud storage connectivity and survey data ingestion capabilities for the ZooGoN package. The focus is on building infrastructure for automated workflows, with new functions for SharePoint integration, KoboToolbox data retrieval, and streamlined Darwin Core conversion.

### New Functions

#### Cloud Storage Functions
* **`upload_sharepoint_df()`**: Upload data frames to Microsoft SharePoint document libraries
  - Automatic file versioning with timestamps
  - Support for CSV, TSV, and Parquet formats
  - Authentication via Microsoft Graph API

* **`download_sharepoint_file()`**: Download files from SharePoint
  - Retrieve latest versioned files by prefix
  - Support for multiple file formats

#### Survey Data Ingestion
* **`ingest_surveys()`**: Retrieve survey data from KoboToolbox and upload to SharePoint
  - Connects to KoboToolbox API to download survey submissions
  - Flattens nested JSON survey data into tabular format
  - Validates data integrity and checks for duplicates
  - Uploads processed data to cloud storage

* **`get_kobo_data()`**: Low-level function to retrieve data from KoboToolbox API
  - Handles pagination for large datasets
  - Supports JSON and XML formats

#### Darwin Core Conversion
* **`raw_to_dc()`**: Convert preprocessed legacy data to Darwin Core format
  - Processes parquet files with WoRMS-validated taxonomic data
  - Creates Event, Occurrence, and eMoF (Extended Measurement or Fact) tables
  - Applies BODC NERC Vocabulary standards
  - Currently processes `McZoo_84-13.parquet` (1984-2013 data)

### Infrastructure Changes

#### Configuration System
* New YAML-based configuration (`inst/config.yml`) for managing:
  - SharePoint connection details and credentials
  - KoboToolbox API settings
  - Data bucket organization (`aut_bucket`, `hot_bucket`)
  - File naming conventions

#### Code Organization
* Modular file structure:
  - `R/storage.R`: SharePoint upload/download operations
  - `R/ingestion-surveys.R`: KoboToolbox data retrieval
  - `R/preprocessing-surveys.R`: Survey data transformation
  - `R/processing.R`: Darwin Core conversion

### New Dependencies
* `httr2`: HTTP API interactions
* `arrow`: Parquet file handling
* `config`: Configuration management
* `logger`: Logging functionality
* `purrr`: Functional programming utilities

### Documentation
* Updated function documentation with roxygen2
* Revised `data-processing.Rmd` vignette to document `raw_to_dc()` workflow
* Updated `raw_to_dc.Rd` manual page

### Breaking Changes
* Removed `process_lter_data()` function (replaced by `raw_to_dc()`)
* Requires `inst/config.yml` configuration file with credentials
* `raw_to_dc()` expects preprocessed parquet files instead of raw Excel files

### Notes
* This release establishes the foundation for automated data workflows
* Survey data ingestion is functional but preprocessing steps are still in development
* Darwin Core conversion currently handles legacy data (1984-2013); additional time periods will be added in future releases

# ZooGoN 1.0.0

## Major Release - Comprehensive Data Processing Pipeline

This major release introduces a complete data processing workflow for the LTER-MareChiara zooplankton dataset, transforming ZooGoN from a basic taxonomic standardization tool into a comprehensive biodiversity data processing package.

### New Features

* **`process_lter_data()` Function**: Complete integrated workflow from raw Excel files to Darwin Core format
* **WoRMS Taxonomic Validation**: Optional integration with World Register of Marine Species for taxonomic accuracy
* **Flexible Output Options**: Support for both R list objects and direct CSV export
* **Enhanced Error Handling**: Comprehensive validation and graceful error recovery
* **Geographic Metadata Integration**: Automatic LTER-MareChiara station coordinates and sampling information
* **Processing Metadata**: Detailed workflow information and data quality metrics

### Technical Improvements

* **Enhanced Dependencies**: Added `readr` and `worrms` packages for improved functionality
* **Comprehensive Documentation**: Extensive function documentation with examples and parameter descriptions
* **Quality Control**: Built-in data validation and processing verification
* **Performance Optimization**: Efficient handling of large taxonomic datasets (40 years of data)

### Data Processing Capabilities

* **Complete Workflow**: Single function handles entire Excel → Darwin Core pipeline
* **Taxonomic Standardization**: Enhanced `extract_genus_species()` integration with WoRMS validation
* **Darwin Core Extensions**: Automatic generation of Event, Occurrence, and eMoF tables
* **Temporal Data Handling**: Robust Excel date conversion and temporal metadata management
* **Sample Identification**: Automated mapping between dates and standardized sample IDs

### Documentation Updates

* **Enhanced Vignettes**: Updated data processing workflow to demonstrate `process_lter_data()`
* **README Improvements**: Showcases new comprehensive processing capabilities
* **CLAUDE.md Updates**: Reflects new package architecture and main functions
* **Function Examples**: Comprehensive usage examples for all processing scenarios

### Breaking Changes

* **Package Version**: Updated to 1.0.0 to reflect major functionality additions
* **Enhanced Dependencies**: New required packages for full functionality

### Performance

* **Large Dataset Support**: Efficiently processes 1,506 samples with 148+ taxa
* **Memory Management**: Optimized for 40-year temporal datasets
* **Optional Processing**: WoRMS validation can be disabled for faster execution

# ZooGoN 0.1.0

## Initial Release

This is the initial release of ZooGoN as part of the DTO-BioFlow project (Gulf of Naples - 40 Years of Zooplankton Biodiversity Assessment).

### Features

* **Taxonomic Standardization**: Core `extract_genus_species()` function for standardizing Mediterranean zooplankton taxonomic names
* **Darwin Core Compliance**: Full support for converting LTER-MareChiara data to Darwin Core Archive format
* **FAIR Data Principles**: Ensures Findable, Accessible, Interoperable, Reusable data output
* **EMODnet Biology Integration**: Quality-controlled data ready for European marine biodiversity infrastructure

### Dataset Coverage

* **Temporal Scope**: 1984-2024 (40 years of continuous monitoring)
* **Sample Size**: 1,506 zooplankton samples from Gulf of Naples
* **Taxonomic Diversity**: 148 copepod species + 61 other zooplankton taxa
* **Geographic Coverage**: LTER-MareChiara station (40°81'N, 14°25'E), Tyrrhenian Sea

### Documentation

* Comprehensive package documentation with `pkgdown` website
* Data processing workflow vignette
* Darwin Core integration guide
* EMODnet Biology compliance documentation

### Project Context

This package represents the technical "workbase" for the ZOOGoN-40Y project, funded by a €60,000 DTO-BioFlow FSTP grant under the EU Horizon Mission "Restore our Ocean & Waters by 2030". The work is conducted by Stazione Zoologica Anton Dohrn (Naples, Italy) as part of the broader Digital Twin of the Ocean initiative.

### Standards Compliance

* **Darwin Core Archive**: International biodiversity data standard
* **WoRMS Integration**: World Register of Marine Species validation
* **BODC NERC Vocabulary**: Standardized measurement terminology  
* **ISO19115 Metadata**: International metadata standards
* **Creative Commons Licensing**: CC-BY-NC for open science

### Acknowledgments

This work builds on 40 years of dedicated zooplankton monitoring at the LTER-MareChiara research station and contributes to the European Digital Twin of the Ocean for enhanced marine ecosystem understanding and management.

# ZooGoN 0.0.1

- Initial commit
