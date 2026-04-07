# ZooGoN

**Gulf of Naples - 40 Years of Zooplankton Biodiversity Assessment**

ZooGoN standardizes taxonomic names in Mediterranean zooplankton
datasets spanning four decades (1984-2024) from the LTER-MareChiara
station in the Gulf of Naples. This R package is part of the
[**DTO-BioFlow project**](https://dto-bioflow.eu) (Digital Twin Ocean -
Biodiversity Flow Integration) under the EU Horizon Mission “Restore our
Ocean & Waters by 2030”.

## Project Context

This package processes the most comprehensive long-term zooplankton
dataset from the Western Mediterranean Sea, including:

- 📊 **1,506 zooplankton samples** (1984-2024)  
- 🦐 **148 copepod species** + 61 other taxa
- 🌍 Integration with **European Digital Twin of the Ocean**

## Key Features

- **Cloud Storage Integration**: Microsoft SharePoint connectivity for
  collaborative data management
- **Survey Data Ingestion**: KoboToolbox integration for automated field
  survey collection
- **Darwin Core Conversion**: Transform legacy zooplankton data to
  OBIS-compliant format
- **Automated Workflows**: Infrastructure for continuous biodiversity
  monitoring
- **FAIR Data Principles**: Ensures Findable, Accessible, Interoperable,
  Reusable data
- **EMODnet Biology Ready**: Standards-compliant data for European
  marine biodiversity infrastructure

## Installation

You can install the development version of ZooGoN from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ioledc/ZOOGoN-40Y")
```

## Usage

### Automated Pipeline

ZooGoN provides a fully automated data pipeline that processes
zooplankton data from field collection to Darwin Core Archive. Each step
reads its input from SharePoint and uploads results back automatically:

``` r
library(ZooGoN)

# 1. Ingest field surveys from KoboToolbox
ingest_surveys()

# 2. Preprocess and standardize survey data
preprocess_surveys()

# 3. Merge legacy + ongoing data into an analysis-ready dataset
format_to_tidy()

# 4. Convert tidy data to Darwin Core format (Event, Occurrence, eMoF)
format_to_dc()

# 5. Build Darwin Core Archive with EML metadata and upload
format_to_DC_archive()
```

The pipeline produces OBIS-compliant Darwin Core tables: - **Event
Extension**: Sampling event metadata with geographic coordinates -
**Occurrence Extension**: Species occurrences with WoRMS LSIDs - **eMoF
Extension**: Measurements with BODC NERC Vocabulary standards

All steps are also run automatically via GitHub Actions on a scheduled
basis.

### Cloud Storage Operations

Upload and download data to/from Microsoft SharePoint:

``` r
# Upload data with automatic versioning
upload_sharepoint_df(
  data = my_data,
  prefix = "processed",
  options = config$storage$sharepoint,
  format = "parquet"
)

# Download latest version
data <- download_sharepoint_file(
  prefix = "processed",
  options = config$storage$sharepoint,
  format = "parquet"
)
```

### Survey Data Ingestion

Retrieve field survey data from KoboToolbox:

``` r
# Ingest surveys from KoboToolbox and upload to SharePoint
ingest_surveys()

# Or retrieve data directly
survey_data <- get_kobo_data(
  assetid = "your_asset_id",
  uname = "username",
  pwd = "password"
)
```

## Dataset Overview

The LTER-MareChiara zooplankton dataset represents one of the longest
continuous time series in the Mediterranean Sea:

| Period    | Frequency      | Samples | Net Type                    | Fixation          |
|-----------|----------------|---------|-----------------------------|-------------------|
| 1984-1990 | Biweekly       | 156     | Indian Ocean (200μm, 113cm) | Formaldehyde 2-4% |
| 1991-1994 | *Interruption* | \-      | \-                          | \-                |
| 1995-2015 | Weekly         | 1,092   | Indian Ocean (200μm, 113cm) | Formaldehyde 2-4% |
| 2016-2024 | Weekly         | 258     | WP2 (200μm, 70cm)           | Ethanol 96%       |

**Total: 1,506 samples • 148 copepod species • 61 other taxa**

## Data Standards & Compliance

ZooGoN ensures compatibility with international biodiversity data
standards:

- **🗂️ Darwin Core Archive**: International standard for biodiversity
  data
- **🌐 WoRMS Integration**: World Register of Marine Species taxonomic
  validation  
- **📊 BODC NERC Vocabulary**: Standardized measurement terminology
- **⚡ EMODnet Biology**: European marine biodiversity data
  infrastructure
- **🏷️ ISO19115 Metadata**: International metadata standards
- **📄 FAIR Principles**: Findable, Accessible, Interoperable, Reusable
  data

## Publishing to GBIF (optional)

- Build a Darwin Core Archive and EML with
  [`format_to_DC_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_DC_archive.md)
  (writes the zip and uploads it to SharePoint). This reads the Darwin
  Core tables produced by
  [`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md).
- Production registration: use
  `register_gbif_dataset(title, description)` — credentials, org/install
  keys, and endpoint URL are read from environment variables via
  `inst/config.yml` (`GBIF_ENDPOINT_URL`, `GBIF_ORG_KEY`,
  `GBIF_INSTALL_KEY`, `GBIF_USERNAME`, `GBIF_PASSWORD`).
- Test registration: use
  [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  — same pattern targeting the GBIF-Test sandbox; set the `GBIF_TEST_*`
  environment variables (see `.env.example`).

## Contributing

This package is part of the DTO-BioFlow project timeline (2025-2026):

- **May 2025**: Project initiation, Paris workshop
- **August 2025**: First interim report, EMODnet training completion
- **December 2025**: Second interim report
- **April 2026**: Final deliverables and EMODnet Biology publication

## Citation

``` r
citation("ZooGoN")
```

## Funding

This work is supported by the DTO-BioFlow project
(HORIZON-MISS-2022-OCEAN-01-07) under the EU Mission “Restore our Ocean
& Waters by 2030”.

## Contact

- **Principal Investigator**: Iole Di Capua (<iole.dicapua@szn.it>)

**Institution**: Stazione Zoologica Anton Dohrn, Naples, Italy

**ORCID**: [0000-0003-2959-8977](https://orcid.org/0000-0003-2959-8977)

- **Principal Analyst**: Lorenzo Longobardi
  (<lorenzo.longobardi@gmail.com>)

**Institution**: WorldFish

**ORCID**: [0000-0003-3126-7341](https://orcid.org/0000-0003-3126-7341)

## Acknowledgments

- LTER-MareChiara research station
- DTO-BioFlow project consortium  
- EMODnet Biology data infrastructure
- European Digital Twin of the Ocean initiative
