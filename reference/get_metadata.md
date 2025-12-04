# Build basic EML metadata

Creates a simple EML list for the MareChiara dataset using the supplied
event table to derive the date range.

## Usage

``` r
get_metadata(event_df = NULL)
```

## Arguments

- event_df:

  Event data frame with `eventDate`.

## Value

A list suitable for
[`EML::write_eml()`](https://docs.ropensci.org/EML/reference/write_eml.html).
