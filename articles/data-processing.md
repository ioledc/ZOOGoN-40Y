# LTER-MareChiara Data Processing Workflow

``` r
library(ZooGoN)
library(dplyr)
```

## Overview

This vignette describes the full data processing pipeline for the
LTER-MareChiara zooplankton dataset. The pipeline ingests field surveys,
merges them with legacy data, converts everything to Darwin Core format,
and publishes a Darwin Core Archive. All data flows through Microsoft
SharePoint — each step downloads its input and uploads results
automatically.

### Dataset Background

The LTER-MareChiara station (40°81’N, 14°25’E) has been monitoring
zooplankton communities since 1984 as part of the Long-Term Ecological
Research network. This represents one of the longest continuous time
series in the Mediterranean Sea.

**Key Dataset Characteristics:** - **Temporal Coverage**: 1984-2024 (40
years) - **Total Samples**: 1,506 - **Taxonomic Diversity**: 148 copepod
species + 61 other taxa - **Sampling Method**: Vertical tows (0-50m
depth) - **Mesh Size**: 200 μm - **Location**: Gulf of Naples,
Tyrrhenian Sea, Western Mediterranean

## The Pipeline

The pipeline consists of five steps, each implemented as a standalone
function. The same sequence runs on GitHub Actions for fully automated
processing.

``` r
# 1. Ingest field surveys from KoboToolbox
ingest_surveys()

# 2. Preprocess and standardize survey data
preprocess_surveys()

# 3. Merge legacy + ongoing data into an analysis-ready dataset
raw_to_tidy()

# 4. Convert tidy data to Darwin Core format
raw_to_dc()

# 5. Build Darwin Core Archive with EML metadata
dc_to_archive()
```

### Step 1: Ingest Surveys

[`ingest_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_surveys.md)
connects to the KoboToolbox API, downloads the latest field survey
submissions, flattens the nested JSON into a tabular format, and uploads
the result to SharePoint.

### Step 2: Preprocess Surveys

[`preprocess_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/preprocess_surveys.md)
downloads the raw survey data from SharePoint, applies data cleaning and
transformation, and uploads the cleaned dataset back to SharePoint.

### Step 3: Merge into Tidy Data

[`raw_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_tidy.md)
merges the legacy datasets (`McZoo_84-15.parquet` and
`McZoo_16-20.parquet` from the hot bucket) with the preprocessed ongoing
surveys into a single analysis-ready dataset. The merged data is
uploaded to SharePoint in both CSV and Parquet formats.

### Step 4: Darwin Core Conversion

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
downloads the tidy dataset from SharePoint and builds the three Darwin
Core extension tables (Event, Occurrence, eMoF). The tables are uploaded
to SharePoint as a versioned RDS file.

See the [Darwin Core
vignette](https://ioledc.github.io/ZOOGoN-40Y/articles/darwin-core.md)
for details on the table structure and standards applied.

### Step 5: Build Archive

[`dc_to_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/dc_to_archive.md)
downloads the Darwin Core tables, builds an EML metadata file, assembles
a Darwin Core Archive zip, and uploads the archive to the
`darwin_core_bucket` on SharePoint.

## Expected Input Data Format

The pipeline expects legacy parquet files containing at least:

``` r
# Example of expected input data structure:
#
# # A tibble: 350,112 × 6
#    eventID eventDate  scientificname                 lsid                                      individualCount lifeStage
#    <chr>   <date>     <chr>                          <chr>                                               <dbl> <chr>
#  1 mc_1    1984-01-26 Acartia (Acartia) danae        urn:lsid:marinespecies.org:taxname:346026             0   f
#  2 mc_1    1984-01-26 Acartia (Acartia) danae        urn:lsid:marinespecies.org:taxname:346026             0   m
#  3 mc_1    1984-01-26 Acartia (Acartia) danae        urn:lsid:marinespecies.org:taxname:346026             0   j
#  4 mc_1    1984-01-26 Acartia negligens              urn:lsid:marinespecies.org:taxname:104259             0   f
#  5 mc_1    1984-01-26 Acartia clausi                 urn:lsid:marinespecies.org:taxname:104251             3.9 f
#
# Required columns:
# - eventID: Unique sampling event identifier (e.g., "mc_1", "mc_2")
# - eventDate: Sampling date (Date format)
# - scientificname: Full scientific name with WoRMS validation
# - lsid: WoRMS Life Science Identifier URN
# - individualCount: Abundance measurement (ind/m³)
# - lifeStage: Life stage code ("f"=female, "m"=male, "j"=juvenile, "fm"=both sexes, "fmj"=all stages)
```

## Darwin Core Output

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
produces three tables following OBIS standards:

#### Event Extension

- One row per unique sampling event
- Station metadata: LTER-MareChiara coordinates (40.81°N, 14.25°E)
- Sampling protocol, depth range, locality

#### Occurrence Extension

- Links species occurrences to events via `eventID`
- `occurrenceStatus` derived from `individualCount` (present/absent)
- WoRMS LSIDs for taxonomic validation

#### eMoF (Extended Measurement or Fact) Extension

- Measurements standardised with BODC NERC Vocabulary
- `individualCount`, `sex`, and `lifeStage` mapped to controlled
  vocabulary URIs

## Data Standards Applied

#### Taxonomic Validation

The legacy input data already includes WoRMS validation:

``` r
# WoRMS LSIDs are already present in the input data:
# - scientificNameID contains WoRMS LSID URNs
# - Format: "urn:lsid:marinespecies.org:taxname:XXXXXX"
# - Ensures taxonomic consistency with international databases
# - Links to accepted names and taxonomic hierarchy
```

#### Measurement Standardization

The eMoF table uses BODC NERC Vocabulary Server standards:

``` r
# 1. Individual counts:
#    measurementTypeID: "https://vocab.nerc.ac.uk/collection/S06/current/S0600002/"
#    measurementUnitID: "http://vocab.nerc.ac.uk/collection/P06/current/UPMM/"

# 2. Sex information:
#    measurementTypeID: "http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/"
#    measurementValueID: S10 collection codes (e.g., S102=female, S103=male)

# 3. Life stage information:
#    measurementTypeID: "http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/"
#    measurementValueID: S11 collection codes (e.g., S1127=juvenile)
```

#### OBIS Compliance

``` r
# The three-table structure is OBIS-compliant:
# 1. Event table (core): Sampling event metadata
# 2. Occurrence table (extension): Links to events via eventID
# 3. eMoF table (extension): Links to occurrences via occurrenceID
#
# Geographic coordinates use decimal degrees (WGS84)
# Dates follow ISO 8601 format (YYYY-MM-DD)
#
# For OBIS publication guidance, see:
# https://manual.obis.org/darwin_core.html
```

## Integration with EMODnet Biology

The processed Darwin Core data is ready for integration with EMODnet
Biology. Before submission, verify that:

- All required Darwin Core terms are present (`eventID`, `eventDate`,
  `decimalLatitude`, `decimalLongitude`, `occurrenceID`,
  `scientificName`, `occurrenceStatus`)
- Taxonomic identifiers are valid WoRMS LSIDs
- Geographic coordinates are within valid ranges
- Contact EMODnet Biology data manager for submission

## Conclusion

This workflow converts 40 years of LTER-MareChiara zooplankton data into
FAIR-compliant, Darwin Core-formatted datasets suitable for:

- **EMODnet Biology** publication and quality control
- **European Digital Twin of the Ocean** integration
- **OBIS** (Ocean Biodiversity Information System) compatibility
- **International biodiversity databases** interoperability
- **Long-term ecological research** and climate change studies

The standardized dataset contributes to the EU Horizon Mission “Restore
our Ocean & Waters by 2030” by providing 40 years of essential
biodiversity monitoring data from the LTER-MareChiara station in the
Gulf of Naples, Mediterranean Sea.
