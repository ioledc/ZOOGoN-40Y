# LTER-MareChiara Data Processing Workflow

``` r
library(ZooGoN)
library(dplyr)
```

## Overview

This vignette demonstrates the data processing workflow for converting
legacy LTER-MareChiara zooplankton datasets into Darwin Core-compliant
format for integration with EMODnet Biology and the European Digital
Twin of the Ocean. The workflow uses the
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
function to transform preprocessed legacy data (with WoRMS taxonomic
validation) into standardized Darwin Core extensions.

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

## Data Processing Workflow

### Converting Legacy Data to Darwin Core with `raw_to_dc()`

The ZooGoN package provides the
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
function to convert preprocessed legacy zooplankton datasets into Darwin
Core-compliant format. This function expects **preprocessed** legacy
data stored as parquet files with WoRMS taxonomic validation already
applied.

#### Expected Input Data Format

The function processes legacy data files (e.g., `McZoo_84-13.parquet`)
with the following structure:

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

#### Basic Usage

``` r
# Convert legacy data to Darwin Core format
dc_data <- raw_to_dc()

# Access the Darwin Core formatted results
event_table <- dc_data$event
occurrence_table <- dc_data$occurrence
emof_table <- dc_data$emof
processing_info <- dc_data$processing_info
metadata <- dc_data$metadata
```

#### What the Function Does

The
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
function performs the following transformations:

##### 1. Downloads Legacy Data

- Retrieves preprocessed parquet files from SharePoint storage
- Currently processes: `McZoo_84-13.parquet` (1984-2013 data)
- Future support for: `McZoo_16.parquet`, `McZoo_17.parquet`, etc.

##### 2. Creates Darwin Core Event Extension

- Extracts unique sampling events with temporal information
- Assigns LTER-MareChiara station coordinates automatically:
  - Latitude: 40.81°N
  - Longitude: 14.25°E
  - Location: Gulf of Naples, Tyrrhenian Sea
  - Sampling protocol: Vertical tow 0-50m depth

``` r
# Event extension structure:
# - eventID: Unique event identifier
# - eventDate: Sampling date
# - decimalLatitude: 40.81
# - decimalLongitude: 14.25
# - locality: "LTER-MareChiara station"
# - country: "Italy"
# - stateProvince: "Campania"
# - waterBody: "Mediterranean Sea"
# - maximumDepthInMeters: 50
# - minimumDepthInMeters: 0
# - samplingProtocol: "Vertical tow 0-50m depth"
```

##### 3. Creates Darwin Core Occurrence Extension

- Links species occurrences to sampling events via `eventID`
- Determines occurrence status (present/absent) based on abundance
- Includes WoRMS LSIDs for taxonomic validation

``` r
# Occurrence extension structure:
# - eventID: Links to Event table
# - occurrenceID: Unique occurrence identifier
# - scientificName: Full scientific name
# - scientificNameID: WoRMS LSID
# - occurrenceStatus: "present" or "absent" (based on individualCount > 0)
```

##### 4. Creates Darwin Core eMoF (Extended Measurement or Fact) Extension

- Transforms measurements into standardized format
- Uses BODC NERC Vocabulary Server controlled vocabulary
- Includes measurement types, values, units, and their URIs

``` r
# eMoF extension structure:
# - eventID: Links to Event table
# - occurrenceID: Links to Occurrence table
# - measurementType: Type of measurement (e.g., "individualCount", "sex", "lifeStage")
# - measurementTypeID: NERC vocabulary URI for measurement type
# - measurementValue: Measured value
# - measurementValueID: NERC vocabulary URI for value (e.g., sex codes, life stages)
# - measurementUnitID: NERC vocabulary URI for units (e.g., ind/m³)
```

#### Silent Processing

``` r
# Run without console messages
dc_data <- raw_to_dc(verbose = FALSE)
```

#### Accessing Processing Information

``` r
# View summary statistics
dc_data$processing_info
# Returns:
# - processing_date: Timestamp of processing
# - total_events: Number of sampling events
# - total_occurrences: Number of occurrence records
# - total_measurements: Number of measurements in eMoF table
# - date_range: Start and end dates of sampling
# - unique_taxa: Number of unique taxa

# View dataset metadata
dc_data$metadata
# Returns tibble with:
# - dataset_title: Full dataset title
# - contact: Principal investigator contact
# - institution: Stazione Zoologica Anton Dohrn
# - license: CC-BY-NC
# - project: DTO-BioFlow FSTP Grant
# - processing_date, total_events, date_range_start, date_range_end
```

## Quality Control and Validation

### Verifying Output Data Quality

After processing, you can verify the quality and completeness of the
Darwin Core data:

``` r
# Process the data
dc_data <- raw_to_dc()

# Verify sample counts using processing_info
print(dc_data$processing_info)

# Check for data completeness
missing_coords <- dc_data$event %>%
  filter(is.na(decimalLatitude) | is.na(decimalLongitude))
cat("Events missing coordinates:", nrow(missing_coords), "\n")

# Verify occurrence status assignment
occurrence_summary <- dc_data$occurrence %>%
  count(occurrenceStatus)
print(occurrence_summary)

# Check taxonomic coverage
unique_taxa <- dc_data$occurrence %>%
  distinct(scientificName, scientificNameID) %>%
  arrange(scientificName)
cat("Total unique taxa:", nrow(unique_taxa), "\n")

# Verify measurement types in eMoF
measurement_types <- dc_data$emof %>%
  count(measurementType, measurementTypeID) %>%
  arrange(measurementType)
print(measurement_types)
```

### Data Export

Export the processed Darwin Core data for EMODnet Biology submission:

``` r
# Export Darwin Core tables to CSV
output_dir <- "processed_data/darwin_core"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Export Event extension
readr::write_csv(
  dc_data$event,
  file.path(output_dir, "event.csv")
)

# Export Occurrence extension
readr::write_csv(
  dc_data$occurrence,
  file.path(output_dir, "occurrence.csv")
)

# Export eMoF extension
readr::write_csv(
  dc_data$emof,
  file.path(output_dir, "emof.csv")
)

# Export metadata
readr::write_csv(
  dc_data$metadata,
  file.path(output_dir, "metadata.csv")
)

# Export raw preprocessed data (for reference)
readr::write_csv(
  dc_data$raw_data,
  file.path(output_dir, "raw_legacy_data.csv")
)

cat("Darwin Core data exported to:", output_dir, "\n")
```

## Integration with EMODnet Biology

The processed Darwin Core data is ready for integration with EMODnet
Biology. The
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
function ensures compliance with international biodiversity data
standards.

### Data Standards Applied

#### Taxonomic Validation

The legacy input data already includes WoRMS validation:

``` r
# WoRMS LSIDs are already present in the input data:
# - scientificNameID contains WoRMS LSID URNs
# - Format: "urn:lsid:marinespecies.org:taxname:XXXXXX"
# - Ensures taxonomic consistency with international databases
# - Links to accepted names and taxonomic hierarchy

# Example from the data:
# scientificName: "Acartia clausi"
# scientificNameID: "urn:lsid:marinespecies.org:taxname:104251"
```

#### Measurement Standardization

The
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
function applies BODC NERC Vocabulary Server standards:

``` r
# eMoF table uses standardized NERC vocabulary terms:

# 1. Individual counts:
#    measurementType: "individualCount"
#    measurementTypeID: "https://vocab.nerc.ac.uk/collection/S06/current/S0600002/"
#    measurementUnitID: "http://vocab.nerc.ac.uk/collection/P06/current/UPMM/"
#    (units: individuals per cubic meter)

# 2. Sex information:
#    measurementType: "sex"
#    measurementTypeID: "http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/"
#    measurementValueID: S10 collection codes (e.g., S102=female, S103=male)

# 3. Life stage information:
#    measurementType: "lifeStage"
#    measurementTypeID: "http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/"
#    measurementValueID: S11 collection codes (e.g., S1127=juvenile)

# This enables interoperability with other marine biodiversity datasets
```

### OBIS Compliance

The Darwin Core structure follows OBIS (Ocean Biodiversity Information
System) standards:

``` r
# The three-table structure is OBIS-compliant:
# 1. Event table (core): Sampling event metadata
# 2. Occurrence table (extension): Links to events via eventID
# 3. eMoF table (extension): Links to occurrences via occurrenceID

# Geographic coordinates use decimal degrees (WGS84)
# Dates follow ISO 8601 format (YYYY-MM-DD)
# All tables include proper foreign key relationships

# For OBIS publication guidance, see:
# https://manual.obis.org/darwin_core.html
```

### Data Submission Checklist

Before submitting to EMODnet Biology:

``` r
# 1. Verify all required Darwin Core terms are present
dc_data <- raw_to_dc()
required_event_terms <- c("eventID", "eventDate", "decimalLatitude", "decimalLongitude")
required_occurrence_terms <- c("eventID", "occurrenceID", "scientificName", "occurrenceStatus")

# 2. Check for missing values in critical fields
summary(dc_data$event)
summary(dc_data$occurrence)

# 3. Validate taxonomic identifiers (WoRMS LSIDs)
all(grepl("^urn:lsid:marinespecies.org:taxname:", dc_data$occurrence$scientificNameID))

# 4. Verify geographic coordinates are within valid ranges
all(dc_data$event$decimalLatitude >= -90 & dc_data$event$decimalLatitude <= 90)
all(dc_data$event$decimalLongitude >= -180 & dc_data$event$decimalLongitude <= 180)

# 5. Export data as CSV files
# (See Data Export section above)

# 6. Prepare metadata document (EML or ISO19115)
# 7. Contact EMODnet Biology data manager for submission
```

## Future Development

### Support for Additional Legacy Files

The current implementation processes `McZoo_84-13.parquet`. Future
versions will support:

``` r
# Planned support for additional legacy files:
# - McZoo_16.parquet (2016 data)
# - McZoo_17.parquet (2017 data)
# - McZoo_18.parquet (2018 data)
# - ... continuing through 2024

# All files will follow the same standardized format:
# - eventID, eventDate, scientificname, lsid, individualCount, lifeStage

# The raw_to_dc() function will be extended to:
# 1. Accept file path as parameter
# 2. Process multiple files in batch
# 3. Combine datasets with proper eventID management
```

## Conclusion

This workflow converts preprocessed legacy LTER-MareChiara zooplankton
data into FAIR-compliant, Darwin Core-formatted datasets suitable for:

- **EMODnet Biology** publication and quality control
- **European Digital Twin of the Ocean** integration
- **OBIS** (Ocean Biodiversity Information System) compatibility
- **International biodiversity databases** interoperability
- **Long-term ecological research** and climate change studies

The standardized dataset contributes to the EU Horizon Mission “Restore
our Ocean & Waters by 2030” by providing 40 years of essential
biodiversity monitoring data from the LTER-MareChiara station in the
Gulf of Naples, Mediterranean Sea.

### Key Features

- **Standardized taxonomic names** with WoRMS LSIDs
- **OBIS-compliant Darwin Core structure** (Event, Occurrence, eMoF)
- **BODC NERC vocabulary** for measurement standardization
- **Geographic metadata** with proper coordinates and locality
  information
- **Quality control** with comprehensive validation checks
- **Ready for EMODnet Biology** submission and publication
