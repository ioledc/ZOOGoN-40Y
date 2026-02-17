# Darwin Core Output for LTER-MareChiara Legacy Data

``` r
library(ZooGoN)
library(dplyr)
```

## Overview

This vignette shows how the package builds Darwin Core tables from the
LTER-MareChiara zooplankton dataset. The conversion is handled by
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md),
which downloads the merged tidy dataset from SharePoint (produced by
[`raw_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_tidy.md)),
adds the required Darwin Core fields, and uploads the resulting tables
back to SharePoint as a versioned RDS file. The function uses data that
already contains WoRMS LSIDs and does not perform additional taxonomic
queries.

### Prerequisites

- `inst/config.yml` configured with SharePoint credentials and bucket
  names.
- The tidy dataset must exist in the `automation_bucket` (produced by
  running
  [`raw_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_tidy.md)
  earlier in the pipeline). This merged dataset covers 1984-2024 and
  contains preprocessed records with WoRMS LSIDs.
- Optional: set `verbose = FALSE` to silence log messages.

## Expected Input Structure

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
expects the tidy dataset to contain at least:

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
# Convert tidy data to Darwin Core format
# Tables are uploaded to SharePoint automatically
raw_to_dc()
```

The function downloads the tidy dataset from SharePoint, builds the
three Darwin Core tables (Event, Occurrence, eMoF), and uploads them as
a versioned RDS file to the `automation_bucket`.

To build a Darwin Core Archive with EML metadata on top of the tables:

``` r
# Build DwC-A zip and upload to SharePoint
dc_to_archive()
```

## Darwin Core tables

### Event

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
builds one row per unique event with fixed station metadata.

``` r
# After downloading the DC tables from SharePoint:
# event_table %>%
#   select(eventID, eventDate, decimalLatitude, decimalLongitude, samplingProtocol) %>%
#   head()
```

Columns include:

- `eventID`, `eventDate`
- `decimalLatitude` = 40.81, `decimalLongitude` = 14.25
- `locality`, `country`, `stateProvince`, `waterBody`
- `maximumDepthInMeters` = 50, `minimumDepthInMeters` = 0
- `samplingProtocol`, `sampleSizeValue` (= 1), `sampleSizeUnit` (=
  “sample”)

### Occurrence

Occurrences are derived from the tidy data with an automatically
generated `occurrenceID` and presence flag.

- `occurrenceStatus` is set to `"present"` when `individualCount > 0`,
  otherwise `"absent"`.
- `scientificName` and `scientificNameID` come directly from the input
  (no new validation is run).

### eMoF (Extended Measurement or Fact)

Measurements are produced by pivoting the remaining columns per
occurrence.

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

[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
stores processing metadata alongside the tables:

- `processing_info`: event, occurrence and measurement counts, date
  range, unique taxa count.
- `metadata`: dataset title, contact, institution, license, project tag,
  and the processing statistics.

## Publishing (optional)

If you need to publish the archive to GBIF:

- Production: supply your real organization/installation keys and a
  public DwC-A URL to
  [`register_gbif_dataset()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset.md).
- Test: use the GBIF-Test demo helper
  [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  with the demo credentials and a public DwC-A URL to check the flow
  safely.

## Limitations and next steps

- The function assumes WoRMS identifiers are already present; it does
  not call external taxonomic services.
- No automated quality control is applied beyond field renaming and
  simple presence/absence derivation. Add your own checks before
  publication.
