# Package index

## Darwin Core Conversion

Convert legacy data to Darwin Core format

- [`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
  : Convert legacy LTER-MareChiara zooplankton data to Darwin Core
  format

## Cloud Storage

SharePoint integration for data storage and retrieval

- [`upload_sharepoint_df()`](https://ioledc.github.io/ZOOGoN-40Y/reference/upload_sharepoint_df.md)
  : Upload a data frame to SharePoint
- [`download_sharepoint_file()`](https://ioledc.github.io/ZOOGoN-40Y/reference/download_sharepoint_file.md)
  : Download a file from SharePoint

## Survey Data Ingestion

KoboToolbox integration for field survey data collection

- [`ingest_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_surveys.md)
  : Download MC Surveys from Kobotoolbox
- [`get_kobo_data()`](https://ioledc.github.io/ZOOGoN-40Y/reference/get_kobo_data.md)
  : Retrieve Data from Kobotoolbox API
- [`preprocess_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/preprocess_surveys.md)
  : Preprocess MC Surveys

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

- [`dc_to_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/dc_to_archive.md)
  : Build a Darwin Core Archive and upload to SharePoint
- [`register_gbif_dataset()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset.md)
  : Register a hosted archive on GBIF
- [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  : GBIF-Test demo flow (fixed keys and credentials)
