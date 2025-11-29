# Darwin Core Output for LTER-MareChiara Legacy Data

``` r
library(ZooGoN)
library(dplyr)
```

## Overview

This vignette shows how the package builds Darwin Core tables from the
legacy LTER-MareChiara zooplankton dataset. The workflow is implemented
in
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md),
which downloads a preprocessed parquet file from SharePoint (see
`inst/config.yml` for buckets and credentials), adds the required Darwin
Core fields, and returns the tables as a list. The function uses data
that already contain WoRMS LSIDs and does not perform additional
taxonomic queries.

### Prerequisites

- `inst/config.yml` configured with SharePoint credentials and bucket
  names.
- The file `McZoo_84-13.parquet` available in the `hot_bucket` defined
  in the config. This file should contain preprocessed records with
  WoRMS LSIDs.
- Optional: set `verbose = FALSE` to silence log messages.

## Expected Input Structure

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
expects the parquet file to contain at least:

- `eventID`: sampling event identifier (for example `"mc_1"`).
- `eventDate`: sampling date (`Date` column).
- `scientificname`: WoRMS validated taxon name.
- `lsid`: WoRMS Life Science Identifier.
- `individualCount`: abundance measurement (individuals per cubic
  meter).
- `lifeStage`: life stage codes (`f`, `m`, `j`, `fm`, `fmj`).

Any extra columns are carried through and pivoted into the measurement
table.

## Running the conversion

``` r
dc_data <- raw_to_dc()

# Darwin Core tables
event_table <- dc_data$event
occurrence_table <- dc_data$occurrence
emof_table <- dc_data$emof

# Processing metadata
dc_data$processing_info
dc_data$metadata
```

The function fetches the parquet file from SharePoint, builds the
tables, and returns them without writing to disk.

## Darwin Core tables

### Event

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
builds one row per unique event with fixed station metadata.

``` r
event_table %>%
  select(eventID, eventDate, decimalLatitude, decimalLongitude, samplingProtocol) %>%
  head()
```

Columns include:

- `eventID`, `eventDate`
- `decimalLatitude` = 40.81, `decimalLongitude` = 14.25
- `locality`, `country`, `stateProvince`, `waterBody`
- `maximumDepthInMeters` = 50, `minimumDepthInMeters` = 0
- `samplingProtocol`, `sampleSizeValue` (= 1), `sampleSizeUnit` (=
  “sample”)

### Occurrence

Occurrences are derived from the legacy data with an automatically
generated `occurrenceID` and presence flag.

``` r
occurrence_table %>%
  select(eventID, occurrenceID, scientificName, scientificNameID, occurrenceStatus) %>%
  head()
```

- `occurrenceStatus` is set to `"present"` when `individualCount > 0`,
  otherwise `"absent"`.
- `scientificName` and `scientificNameID` come directly from the input
  (no new validation is run).

### eMoF (Extended Measurement or Fact)

Measurements are produced by pivoting the remaining columns per
occurrence.

``` r
emof_table %>%
  count(measurementType, measurementTypeID) %>%
  arrange(measurementType)
```

Mapping logic implemented in
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md):

- Measurements with values `f`, `m`, `fm`, `fmj` are labelled as
  `measurementType = "sex"` with
  `measurementTypeID = http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/`.
- Measurements with value `j` are labelled
  `measurementType = "lifeStage"` with
  `measurementTypeID = http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/`.
- `individualCount` keeps `measurementType = "individualCount"` with
  `measurementTypeID = https://vocab.nerc.ac.uk/collection/S06/current/S0600002/`
  and
  `measurementUnitID = http://vocab.nerc.ac.uk/collection/P06/current/UPMM/`.
- `measurementValueID` is set for the coded values above (S10/S11 URIs);
  other measurements keep `NA`.

## Processing metadata

The function returns a small metadata bundle alongside the tables.

``` r
dc_data$processing_info
dc_data$metadata
```

`processing_info` summarises counts (events, occurrences, measurements),
date range, and unique taxa. `metadata` is a tibble with dataset title,
contact, institution, license, project tag, and the processing
statistics recorded during the run.

## Exporting results

Use base R or the storage helpers to persist the output.

``` r
# Write locally
readr::write_csv(dc_data$event, "event.csv")
readr::write_csv(dc_data$occurrence, "occurrence.csv")
readr::write_csv(dc_data$emof, "emof.csv")

# Or upload back to SharePoint with versioning
upload_sharepoint_df(
  data = dc_data$emof,
  prefix = "darwin-core/emof",
  options = read_config()$storage$sharepoint,
  format = "csv"
)
```

## Limitations and next steps

- The current implementation processes a single legacy file
  (`McZoo_84-13.parquet`). Additional years will need to be added
  explicitly.
- The function assumes WoRMS identifiers are already present; it does
  not call external taxonomic services.
- No automated quality control is applied beyond field renaming and
  simple presence/absence derivation. Add your own checks before
  publication.
