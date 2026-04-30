# Darwin Core Output for LTER-MareChiara Legacy Data

``` r

library(ZooGoN)
library(dplyr)
```

## Overview

Darwin Core is the international data standard required by OBIS, EMODnet
Biology, and GBIF to publish biodiversity occurrence records. It
structures data into three linked tables — sampling events, species
occurrences, and associated measurements — allowing any repository
worldwide to ingest and cross-reference the records. Converting the
LTER-MareChiara dataset to this format is the step that makes it ready
for submission to these repositories.

This vignette shows how the package builds those Darwin Core tables from
the LTER-MareChiara zooplankton dataset. The conversion is handled by
[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md),
which downloads the merged tidy dataset from SharePoint (produced by
[`format_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_tidy.md)),
adds the required Darwin Core fields, and returns the three tables as an
R list. The function does not upload anything — that is handled by
[`format_to_DC_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_DC_archive.md),
which calls
[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
internally and then assembles and uploads the Darwin Core Archive zip.
The function uses data that already contains WoRMS LSIDs and does not
perform additional taxonomic queries.

### Prerequisites

- `inst/config.yml` configured with SharePoint credentials and bucket
  names.
- The merged dataset must exist in SharePoint (produced by running
  [`format_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_tidy.md)
  earlier in the pipeline). This dataset covers 1984-2024 and contains
  preprocessed records with WoRMS LSIDs.
- Optional: set `verbose = FALSE` to silence log messages.

## Expected Input Structure

[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
expects the tidy dataset to contain at least:

- `eventID`: sampling event identifier (for example `"mc_1"`).
- `eventDate`: sampling date (`Date` column).
- `scientificname`: WoRMS validated taxon name.
- `lsid`: WoRMS Life Science Identifier.
- `Abundance`: abundance measurement (individuals per cubic meter).
- `lifeStage`: life stage codes (`f`, `m`, `j`, `fm`, `fmj`).

Any extra columns are carried through and pivoted into the measurement
table.

## Running the conversion

``` r

# Convert tidy data to Darwin Core format
# Tables are uploaded to SharePoint automatically
format_to_dc()
```

The function downloads the merged dataset from SharePoint and builds the
three Darwin Core tables (Event, Occurrence, eMoF) in memory. It does
**not** upload anything to SharePoint.

To build a Darwin Core Archive and upload it to SharePoint, call
[`format_to_DC_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_DC_archive.md).
This function calls
[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
internally, so in the automated pipeline it is the only function you
need to run:

``` r

# Build DwC-A zip (runs format_to_dc() internally) and upload to SharePoint
format_to_DC_archive()
```

## Darwin Core tables

### Event

[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
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

- `occurrenceStatus` is set to `"present"` when `Abundance > 0`,
  otherwise `"absent"`.
- `scientificName` and `scientificNameID` come directly from the input
  (no new validation is run).

### eMoF (Extended Measurement or Fact)

eMoF is the Darwin Core extension used to store quantitative
measurements alongside each occurrence record. For the LTER-MareChiara
dataset this includes abundance (individuals per m³), sex, and life
stage. Each measurement is linked to a controlled-vocabulary identifier
so that the values can be interpreted unambiguously by any international
biodiversity database.

Mapping logic implemented in
[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md):

- Measurements with values `f`, `m`, `fm`, `fmj` are labelled as
  `measurementType = "sex"` with
  `measurementTypeID = http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/`.
- Measurements with value `j` are labelled
  `measurementType = "lifeStage"` with
  `measurementTypeID = http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/`.
- `Abundance` keeps `measurementType = "Abundance"` with
  `measurementTypeID = https://vocab.nerc.ac.uk/collection/S06/current/S0600002/`
  and
  `measurementUnitID = http://vocab.nerc.ac.uk/collection/P06/current/UPMM/`.
- `measurementValueID` is set for the coded values above (S10/S11 URIs);
  other measurements keep `NA`.

## Processing summary

[`format_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_dc.md)
prints a processing summary on completion: the number of sampling
events, occurrences, and measurements produced, the date range covered,
and the number of unique taxa included.

## Publishing (optional)

If you need to publish the archive to GBIF:

- Production: call `register_gbif_dataset(title, description)` —
  credentials, org/install keys, and endpoint URL are read from
  environment variables via `inst/config.yml` (`GBIF_ENDPOINT_URL`,
  `GBIF_ORG_KEY`, `GBIF_INSTALL_KEY`, `GBIF_USERNAME`, `GBIF_PASSWORD`).
- Test: call
  [`register_gbif_dataset_test()`](https://ioledc.github.io/ZOOGoN-40Y/reference/register_gbif_dataset_test.md)
  — same pattern targeting the GBIF-Test sandbox; configure the
  `GBIF_TEST_*` variables (see `.env.example`).

## Limitations and next steps

- The function assumes WoRMS identifiers are already present; it does
  not call external taxonomic services.
- No automated quality control is applied beyond field renaming and
  simple presence/absence derivation. Add your own checks before
  publication.
