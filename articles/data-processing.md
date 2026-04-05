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

## Before you start

To run the pipeline you need:

- **Access to the project SharePoint workspace** — all intermediate
  files are read from and written to SharePoint automatically.
- **`inst/config.yml` configured** with your SharePoint credentials and
  KoboToolbox API key. A template is included in the repository.
- **R with the ZooGoN package installed** — see the README for
  installation instructions.

The pipeline does not require any manual file transfers between steps;
each function downloads its input and uploads its output automatically.

## The Pipeline

The pipeline consists of six steps, each implemented as a standalone
function. The same sequence runs on GitHub Actions for fully automated
processing. The two legacy-ingestion steps (0a and 0b) are one-time or
on-demand operations. After step 3, the archive build (4a) and the
report render (4b) run in parallel — both start as soon as the merged
dataset is available.

``` r
# 0a. Ingest legacy dataset 1984-2015 (run once or when source data changes)
ingest_legacy_84_15()

# 0b. Ingest legacy dataset 2016-2020 (run once or when source data changes)
ingest_legacy_16_20()

# 1. Ingest field surveys from KoboToolbox
ingest_surveys()

# 2. Preprocess and standardize survey data
preprocess_surveys()

# 3. Merge legacy + ongoing data into an analysis-ready dataset
format_to_tidy()

# 4a. Build Darwin Core Archive with EML metadata and upload to SharePoint
#     (format_to_dc() is called internally — no need to call it separately)
format_to_DC_archive()

# 4b. Render the monitoring report (runs in parallel with 4a in GitHub Actions)
render_report()
```

### Steps 0a–0b: Ingest Legacy Data

[`ingest_legacy_84_15()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_legacy_84_15.md)
and
[`ingest_legacy_16_20()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_legacy_16_20.md)
migrate the historical zooplankton records (1984–2015 and 2016–2020)
into the cloud infrastructure. They validate and harmonise the data —
including taxonomic lookups against WoRMS and standardisation of
life-stage codes — and store the results in a dedicated SharePoint
folder for validated historical data. These steps are run once, or
whenever the source data are corrected or updated.

### Step 1: Ingest Surveys

[`ingest_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_surveys.md)
connects to the KoboToolbox API, downloads the latest field survey
submissions, flattens the nested form structure into a tabular format,
and uploads the result to SharePoint. This captures all new samples
submitted by the field team since the last run.

### Step 2: Preprocess Surveys

[`preprocess_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/preprocess_surveys.md)
downloads the raw survey data from SharePoint, applies data cleaning and
transformation (including WoRMS taxonomic lookups and life-stage code
harmonisation), and uploads the standardised dataset back to SharePoint.
This ensures new records use the same species names and codes as the
legacy data.

### Step 3: Merge into Tidy Data

[`format_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_tidy.md)
merges the legacy datasets with the preprocessed ongoing surveys into a
single analysis-ready dataset covering the full 1984–present time
series. The result is saved to SharePoint in both CSV and Parquet (a
compact columnar) format.

### Step 4a: Build Darwin Core Archive

[`format_to_DC_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_DC_archive.md)
handles the full Darwin Core conversion and archiving in one call: it
internally runs
[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
to build the Event, Occurrence, and eMoF tables, then assembles them
into a submission-ready zip archive with embedded EML metadata, and
uploads it to SharePoint. This is the file that is registered with and
downloaded by GBIF or EMODnet Biology.

[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
can also be called on its own to inspect the Darwin Core tables without
building the archive (e.g., for quality checks before submission). See
the [Darwin Core
vignette](https://ioledc.github.io/ZOOGoN-40Y/articles/darwin-core.md)
for details on the table structure and standards applied.

### Step 4b: Render Report

[`render_report()`](https://ioledc.github.io/ZOOGoN-40Y/reference/render_report.md)
renders the Quarto monitoring report using the merged tidy dataset and
uploads the resulting HTML to the SharePoint `reports` bucket with a
versioned filename (timestamp + git SHA). A copy is also retained as a
GitHub Actions artifact for 90 days. In the automated pipeline this step
runs in parallel with the archive build (step 4a), so both outputs are
produced from the same merged dataset in the same pipeline run.

The report includes an interactive Taxon Explorer (seasonal heatmap,
seasonal pattern, long-term trend, and phenology charts) and a Temporal
Signals section with time-series feature analysis for the most
frequently observed taxa.

## Expected Input Data Format

This section is a reference for the column names expected in the legacy
files. The ingestion functions
([`ingest_legacy_84_15()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_legacy_84_15.md),
[`ingest_legacy_16_20()`](https://ioledc.github.io/ZOOGoN-40Y/reference/ingest_legacy_16_20.md))
produce files in this format automatically, so you only need this if you
are adapting the pipeline to a different dataset.

The pipeline expects legacy files containing at least:

``` r
# Example of expected input data structure:
#
# # A tibble: 350,112 × 6
#    eventID eventDate  scientificname                 lsid                                      Abundance lifeStage
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
# - Abundance: Abundance measurement (ind/m³)
# - lifeStage: Life stage code ("f"=female, "m"=male, "j"=juvenile, "fm"=both sexes, "fmj"=all stages)
```

## Darwin Core Output

[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
produces three tables following OBIS standards:

#### Event Extension

- One row per unique sampling event
- Station metadata: LTER-MareChiara coordinates (40.81°N, 14.25°E)
- Sampling protocol, depth range, locality

#### Occurrence Extension

- Links species occurrences to events via `eventID`
- `occurrenceStatus` derived from `Abundance` (present/absent)
- WoRMS LSIDs for taxonomic validation

#### eMoF (Extended Measurement or Fact) Extension

- Measurements standardised with BODC NERC Vocabulary
- `Abundance`, `sex`, and `lifeStage` mapped to controlled vocabulary
  URIs

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
