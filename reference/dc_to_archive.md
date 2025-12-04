# Build a Darwin Core Archive and upload to SharePoint

Takes the tables produced by
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md),
builds a Darwin Core Archive zip with an EML file, and uploads the
archive to SharePoint using the configured credentials.

## Usage

``` r
dc_to_archive(dc_list)
```

## Arguments

- dc_list:

  List from
  [`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
  containing `event`, `occurrence`, and `emof`.

## Value

Invisible list with paths to the archive and EML.
