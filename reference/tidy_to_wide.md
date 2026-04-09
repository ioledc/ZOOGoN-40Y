# Pivot tidy zooplankton data to a wide species-by-sample matrix

Reads the analysis-ready tidy dataset from SharePoint and pivots it so
that each sampling event becomes a column and each row represents a
unique taxon-life stage combination. Full taxonomic classification
(Phylum through Species) is fetched from WoRMS and prepended to the
matrix. The result is uploaded as an Excel workbook to the reports
bucket on SharePoint.

## Usage

``` r
tidy_to_wide()
```

## Value

Invisible NULL. The wide-format dataset is uploaded as an XLSX file to
the SharePoint reports bucket.

## Details

**Input data:**

Reads the latest versioned tidy Parquet file from the SharePoint
automation bucket (prefix defined by
`conf$ingestion$tidy_data$file_prefix`). Expects the columns `eventID`,
`eventDate`, `lsid`, `scientificName`, `Abundance`, and `lifeStage`.

**Wide pivot:**

Sample columns are named `<eventID>__<eventDate>`, sorted
chronologically. Cell values are abundance in ind/m³. Rows represent
unique taxon-life stage combinations; missing cells indicate absence.

**Taxonomic enrichment:**

AphiaIDs are extracted from the WoRMS LSID field and used to query the
WoRMS API
([`worrms::wm_classification`](https://docs.ropensci.org/worrms/reference/wm_classification.html))
for full classification. The following ranks are prepended as leading
columns:

- Phylum, Subphylum, Class, Subclass

- Order, Suborder, Family, Subfamily

- Genus, Species

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_to_wide()
} # }
```
