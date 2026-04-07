# Package index

## Survey Data Ingestion

KoboToolbox integration for field survey data collection

- [`ingest_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_surveys.md)
  : Download MC Surveys from Kobotoolbox
- [`get_kobo_data()`](https://ioledc.github.io/ZOOGoN-40Y/reference/get_kobo_data.md)
  : Retrieve Data from Kobotoolbox API
- [`preprocess_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/preprocess_surveys.md)
  : Preprocess MC Surveys

## Legacy Data Ingestion

Download, validate, and harmonize legacy zooplankton datasets

- [`ingest_legacy_84_15()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_legacy_84_15.md)
  : Ingest Legacy Zooplankton Data (1984-2015)
- [`ingest_legacy_16_20()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_legacy_16_20.md)
  : Ingest Legacy Zooplankton Data (2016-2020)

## Data Processing

Merge, harmonize, and convert zooplankton data

- [`format_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_tidy.md)
  : Merge all zooplankton datasets into an analysis-ready tidy table
- [`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
  : Convert legacy LTER-MareChiara zooplankton data to Darwin Core
  format
- [`build_emof_occurrence()`](https://ioledc.github.io/ZOOGoN-40Y/reference/build_emof_occurrence.md)
  : Build occurrence-level eMoF table
- [`build_emof_events()`](https://ioledc.github.io/ZOOGoN-40Y/reference/build_emof_events.md)
  : Build event-level eMoF sampling metadata

## Cloud Storage

SharePoint integration for data storage and retrieval

- [`upload_sharepoint_df()`](https://ioledc.github.io/ZOOGoN-40Y/reference/upload_sharepoint_df.md)
  : Upload a data frame to SharePoint
- [`upload_sharepoint_file()`](https://ioledc.github.io/ZOOGoN-40Y/reference/upload_sharepoint_file.md)
  : Upload a local file to SharePoint
- [`download_sharepoint_file()`](https://ioledc.github.io/ZOOGoN-40Y/reference/download_sharepoint_file.md)
  : Download a file from SharePoint

## Pipeline & Reporting

Orchestrate the full pipeline and render monitoring reports

- [`run_pipeline()`](https://ioledc.github.io/ZOOGoN-40Y/reference/run_pipeline.md)
  : Run the ZooGoN survey-to-report pipeline
- [`render_report()`](https://ioledc.github.io/ZOOGoN-40Y/reference/render_report.md)
  : Render ZooGoN MC Survey Report

## Utility Functions

Helper functions for data processing

- [`reshape_kobo_repeat()`](https://ioledc.github.io/ZOOGoN-40Y/reference/reshape_kobo_repeat.md)
  : Prepare repeat answers from Kobo survey forms
- [`add_version()`](https://ioledc.github.io/ZOOGoN-40Y/reference/add_version.md)
  : Add timestamp and sha string to a file name
- [`read_config()`](https://ioledc.github.io/ZOOGoN-40Y/reference/read_config.md)
  : Read configuration file
- [`load_dotenv()`](https://ioledc.github.io/ZOOGoN-40Y/reference/load_dotenv.md)
  : Load environment variables from .env file

## Publishing

Build and register Darwin Core Archives

- [`format_to_DC_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_DC_archive.md)
  : Build a Darwin Core Archive and upload to SharePoint
- [`register_gbif_dataset()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset.md)
  : Register a hosted archive on GBIF
- [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  : GBIF-Test registration flow
- [`add_gbif_license_block()`](https://ioledc.github.io/ZOOGoN-40Y/reference/add_gbif_license_block.md)
  : Add GBIF-style license block to an EML file
