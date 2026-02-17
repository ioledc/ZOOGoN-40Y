# Build a Darwin Core Archive and upload to SharePoint

Downloads the Darwin Core tables produced by
[`raw_to_dc()`](https://ioledc.github.io/ZOOGoN-40Y/reference/raw_to_dc.md)
from SharePoint, builds a Darwin Core Archive zip with an EML file, and
uploads the archive back to SharePoint.

## Usage

``` r
dc_to_archive()
```

## Value

Invisible list with paths to the archive and EML.
