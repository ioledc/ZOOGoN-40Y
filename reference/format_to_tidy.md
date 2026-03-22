# Merge all zooplankton datasets into an analysis-ready tidy table

Combines legacy data (1984-2020) with ongoing Kobo survey landings
(2021-present) into a single long-format dataset ready for analysis. The
output contains one row per taxon-life stage-event combination with
WoRMS-validated taxonomy and abundance in ind/m³.

## Usage

``` r
format_to_tidy()
```

## Value

Invisible NULL. The tidy dataset is uploaded as versioned CSV and
Parquet files to SharePoint.

## Examples

``` r
if (FALSE) { # \dontrun{
format_to_tidy()
} # }
```
