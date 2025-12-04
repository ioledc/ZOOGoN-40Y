# Changelog

## ZooGoN 3.0.0

- Added automated GBIF publishing helpers:
  [`register_gbif_dataset()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset.md)
  for production use and
  [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  for the GBIF-Test demo flow; both take a public DwC-A URL and handle
  dataset registration.
- Simplified Darwin Core export:
  [`dc_to_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/dc_to_archive.md)
  now builds the DwC-A + EML from
  [`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
  output and uploads the archive to SharePoint.
- Clarified EML generation
  ([`get_metadata()`](https://ioledc.github.io/ZOOGoN-40Y/reference/get_metadata.md))
  and cleaned up pkgdown reference sections.

## ZooGoN 2.0.0

### Automation & Cloud Integration

This release introduces cloud storage connectivity and survey data
ingestion capabilities for the ZooGoN package. The focus is on building
infrastructure for automated workflows, with new functions for
SharePoint integration, KoboToolbox data retrieval, and streamlined
Darwin Core conversion.

#### New Functions

##### Cloud Storage Functions

- **[`upload_sharepoint_df()`](https://ioledc.github.io/ZOOGoN-40Y/reference/upload_sharepoint_df.md)**:
  Upload data frames to Microsoft SharePoint document libraries
  - Automatic file versioning with timestamps
  - Support for CSV, TSV, and Parquet formats
  - Authentication via Microsoft Graph API
- **[`download_sharepoint_file()`](https://ioledc.github.io/ZOOGoN-40Y/reference/download_sharepoint_file.md)**:
  Download files from SharePoint
  - Retrieve latest versioned files by prefix
  - Support for multiple file formats

##### Survey Data Ingestion

- **[`ingest_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_surveys.md)**:
  Retrieve survey data from KoboToolbox and upload to SharePoint
  - Connects to KoboToolbox API to download survey submissions
  - Flattens nested JSON survey data into tabular format
  - Validates data integrity and checks for duplicates
  - Uploads processed data to cloud storage
- **[`get_kobo_data()`](https://ioledc.github.io/ZOOGoN-40Y/reference/get_kobo_data.md)**:
  Low-level function to retrieve data from KoboToolbox API
  - Handles pagination for large datasets
  - Supports JSON and XML formats

##### Darwin Core Conversion

- **[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)**:
  Convert preprocessed legacy data to Darwin Core format
  - Processes parquet files with WoRMS-validated taxonomic data
  - Creates Event, Occurrence, and eMoF (Extended Measurement or Fact)
    tables
  - Applies BODC NERC Vocabulary standards
  - Currently processes `McZoo_84-13.parquet` (1984-2013 data)

#### Infrastructure Changes

##### Configuration System

- New YAML-based configuration (`inst/config.yml`) for managing:
  - SharePoint connection details and credentials
  - KoboToolbox API settings
  - Data bucket organization (`aut_bucket`, `hot_bucket`)
  - File naming conventions

##### Code Organization

- Modular file structure:
  - `R/storage.R`: SharePoint upload/download operations
  - `R/ingestion-surveys.R`: KoboToolbox data retrieval
  - `R/preprocessing-surveys.R`: Survey data transformation
  - `R/processing.R`: Darwin Core conversion

#### New Dependencies

- `httr2`: HTTP API interactions
- `arrow`: Parquet file handling
- `config`: Configuration management
- `logger`: Logging functionality
- `purrr`: Functional programming utilities

#### Documentation

- Updated function documentation with roxygen2
- Revised `data-processing.Rmd` vignette to document
  [`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
  workflow
- Updated `raw_to_dc.Rd` manual page

#### Breaking Changes

- Removed `process_lter_data()` function (replaced by
  [`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md))
- Requires `inst/config.yml` configuration file with credentials
- [`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
  expects preprocessed parquet files instead of raw Excel files

#### Notes

- This release establishes the foundation for automated data workflows
- Survey data ingestion is functional but preprocessing steps are still
  in development
- Darwin Core conversion currently handles legacy data (1984-2013);
  additional time periods will be added in future releases

## ZooGoN 1.0.0

### Major Release - Comprehensive Data Processing Pipeline

This major release introduces a complete data processing workflow for
the LTER-MareChiara zooplankton dataset, transforming ZooGoN from a
basic taxonomic standardization tool into a comprehensive biodiversity
data processing package.

#### New Features

- **`process_lter_data()` Function**: Complete integrated workflow from
  raw Excel files to Darwin Core format
- **WoRMS Taxonomic Validation**: Optional integration with World
  Register of Marine Species for taxonomic accuracy
- **Flexible Output Options**: Support for both R list objects and
  direct CSV export
- **Enhanced Error Handling**: Comprehensive validation and graceful
  error recovery
- **Geographic Metadata Integration**: Automatic LTER-MareChiara station
  coordinates and sampling information
- **Processing Metadata**: Detailed workflow information and data
  quality metrics

#### Technical Improvements

- **Enhanced Dependencies**: Added `readr` and `worrms` packages for
  improved functionality
- **Comprehensive Documentation**: Extensive function documentation with
  examples and parameter descriptions
- **Quality Control**: Built-in data validation and processing
  verification
- **Performance Optimization**: Efficient handling of large taxonomic
  datasets (40 years of data)

#### Data Processing Capabilities

- **Complete Workflow**: Single function handles entire Excel → Darwin
  Core pipeline
- **Taxonomic Standardization**: Enhanced `extract_genus_species()`
  integration with WoRMS validation
- **Darwin Core Extensions**: Automatic generation of Event, Occurrence,
  and eMoF tables
- **Temporal Data Handling**: Robust Excel date conversion and temporal
  metadata management
- **Sample Identification**: Automated mapping between dates and
  standardized sample IDs

#### Documentation Updates

- **Enhanced Vignettes**: Updated data processing workflow to
  demonstrate `process_lter_data()`
- **README Improvements**: Showcases new comprehensive processing
  capabilities
- **CLAUDE.md Updates**: Reflects new package architecture and main
  functions
- **Function Examples**: Comprehensive usage examples for all processing
  scenarios

#### Breaking Changes

- **Package Version**: Updated to 1.0.0 to reflect major functionality
  additions
- **Enhanced Dependencies**: New required packages for full
  functionality

#### Performance

- **Large Dataset Support**: Efficiently processes 1,506 samples with
  148+ taxa
- **Memory Management**: Optimized for 40-year temporal datasets
- **Optional Processing**: WoRMS validation can be disabled for faster
  execution

## ZooGoN 0.1.0

### Initial Release

This is the initial release of ZooGoN as part of the DTO-BioFlow project
(Gulf of Naples - 40 Years of Zooplankton Biodiversity Assessment).

#### Features

- **Taxonomic Standardization**: Core `extract_genus_species()` function
  for standardizing Mediterranean zooplankton taxonomic names
- **Darwin Core Compliance**: Full support for converting
  LTER-MareChiara data to Darwin Core Archive format
- **FAIR Data Principles**: Ensures Findable, Accessible, Interoperable,
  Reusable data output
- **EMODnet Biology Integration**: Quality-controlled data ready for
  European marine biodiversity infrastructure

#### Dataset Coverage

- **Temporal Scope**: 1984-2024 (40 years of continuous monitoring)
- **Sample Size**: 1,506 zooplankton samples from Gulf of Naples
- **Taxonomic Diversity**: 148 copepod species + 61 other zooplankton
  taxa
- **Geographic Coverage**: LTER-MareChiara station (40°81’N, 14°25’E),
  Tyrrhenian Sea

#### Documentation

- Comprehensive package documentation with `pkgdown` website
- Data processing workflow vignette
- Darwin Core integration guide
- EMODnet Biology compliance documentation

#### Project Context

This package represents the technical “workbase” for the ZOOGoN-40Y
project, funded by a €60,000 DTO-BioFlow FSTP grant under the EU Horizon
Mission “Restore our Ocean & Waters by 2030”. The work is conducted by
Stazione Zoologica Anton Dohrn (Naples, Italy) as part of the broader
Digital Twin of the Ocean initiative.

#### Standards Compliance

- **Darwin Core Archive**: International biodiversity data standard
- **WoRMS Integration**: World Register of Marine Species validation
- **BODC NERC Vocabulary**: Standardized measurement terminology  
- **ISO19115 Metadata**: International metadata standards
- **Creative Commons Licensing**: CC-BY-NC for open science

#### Acknowledgments

This work builds on 40 years of dedicated zooplankton monitoring at the
LTER-MareChiara research station and contributes to the European Digital
Twin of the Ocean for enhanced marine ecosystem understanding and
management.

## ZooGoN 0.0.1

- Initial commit
