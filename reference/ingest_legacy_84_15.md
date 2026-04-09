# Ingest Legacy Zooplankton Data (1984-2015)

Downloads, validates, and harmonizes the 1984-2015 legacy zooplankton
dataset from SharePoint. Taxa are matched against the WoRMS database and
the output is written in both Parquet and CSV formats to the automation
bucket.

## Usage

``` r
ingest_legacy_84_15()
```

## Value

Invisible NULL. Outputs are uploaded to SharePoint as versioned CSV and
Parquet files (prefix: `legacy-84-15`) to the automation bucket.

## Details

The function performs the following steps:

1.  Downloads sample ID metadata (`ids_84_15.csv`) and biological data
    (`zoo_84_15.csv`) from the `legacy_data` SharePoint bucket.

2.  Integrates manually curated unmatched taxa from
    `unmatched_worms_84_15.xlsx` to correct known synonyms before WoRMS
    validation.

3.  Queries WoRMS via
    [`worrms::wm_records_taxamatch()`](https://docs.ropensci.org/worrms/reference/wm_records_taxamatch.html)
    for every unique taxon; unmatched taxa are flagged with
    `match_type = "no_match"`.

4.  Selects a single AphiaID per taxon (lowest, i.e. oldest
    classification).

5.  Joins biological observations with WoRMS-validated taxonomy and
    sample metadata, pivoting date columns to long format.

6.  Standardises life-stage codes.

7.  Aggregates duplicate records by summing `Abundance`.

8.  Uploads tidy data to the automation bucket in Parquet and CSV
    formats.

## Note

Unmatched taxa (no WoRMS hit) are reported to the console and excluded
from the final export. Curators should review them and add corrections
to `unmatched_worms_84_15.xlsx` before re-running.

## Examples

``` r
if (FALSE) { # \dontrun{
ingest_legacy_84_15()
} # }
```
