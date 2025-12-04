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

### Darwin Core Conversion

Convert preprocessed legacy zooplankton data to Darwin Core format:

``` r
library(ZooGoN)

# Convert legacy data to Darwin Core format
dc_data <- raw_to_dc()

# Access Darwin Core extension tables
event_table <- dc_data$event              # Sampling events
occurrence_table <- dc_data$occurrence    # Species occurrences
measurements <- dc_data$emof              # Extended measurements

# View processing summary
dc_data$processing_info
```

The function creates OBIS-compliant tables with: - **Event Extension**:
Sampling event metadata with geographic coordinates - **Occurrence
Extension**: Species occurrences with WoRMS LSIDs - **eMoF Extension**:
Measurements with BODC NERC Vocabulary standards

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

- Build a Darwin Core Archive and EML with `dc_to_archive(raw_to_dc())`
  (writes the zip and uploads it to SharePoint).
- Production registration: use
  [`register_gbif_dataset()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset.md)
  with your real GBIF organization key, installation key, credentials,
  and a public DwC-A URL.
- Test registration: use
  [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  with the GBIF-Test demo credentials (ws_client_demo/Demo123) and your
  public DwC-A URL to exercise the flow safely.

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

**ORCID**: [0000-0003-2959-8977](https://orcid.org/0000-0003-3126-7341)

- ## Acknowledgments

- LTER-MareChiara research station

- DTO-BioFlow project consortium

- EMODnet Biology data infrastructure

- European Digital Twin of the Ocean initiative
