# Add GBIF-style license block to an EML file

This helper modifies an existing EML file by inserting a
GBIF/IPT-compatible `<intellectualRights>` element. Any existing
`<intellectualRights>` node is removed and replaced with a paragraph
containing a `<ulink>` pointing to the chosen license URL. The new block
is inserted before `<coverage>` (or `<contact>` if `<coverage>` is
missing) to satisfy the GBIF EML profile element order.

## Usage

``` r
add_gbif_license_block(
  eml_path,
  url = "http://creativecommons.org/licenses/by-nc/4.0/legalcode",
  title = "Creative Commons Attribution Non Commercial (CC-BY-NC) 4.0 License"
)
```

## Arguments

- eml_path:

  Character string giving the path to an EML file on disk. The file is
  modified in place.

- url:

  Character string with the license URL. Defaults to the Creative
  Commons Attribution Non Commercial 4.0 International licence URL.

- title:

  Character string with the human-readable licence title used inside
  `<citetitle>`.

## Value

Invisibly returns `eml_path`.

## Examples

``` r
if (FALSE) { # \dontrun{
eml_path <- "mc_eml.xml"
add_gbif_license_block(eml_path)
} # }
```
