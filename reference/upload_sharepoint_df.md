# Upload a data frame to SharePoint

Upload a data frame to SharePoint

## Usage

``` r
upload_sharepoint_df(
  data,
  prefix,
  options,
  bucket = NULL,
  format = c("csv", "tsv", "parquet", "xlsx", "rds"),
  filename = FALSE
)
```

## Arguments

- data:

  A data frame to upload

- prefix:

  File prefix path (e.g., "raw", "preprocessed"), or exact filename if
  filename = TRUE

- options:

  SharePoint configuration list from config\$storage\$sharepoint

- bucket:

  Bucket name (optional)

- format:

  File format: "csv", "tsv", "xlsx", "parquet", or "rds". Default is
  "csv"

- filename:

  Logical. If TRUE, treat prefix as exact filename and skip versioning.
  Default is FALSE

## Value

Invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
conf <- read_config()
# Upload with automatic versioning
upload_sharepoint_df(my_data, "raw", conf$storage$sharepoint)
# Upload to specific filename (no versioning)
upload_sharepoint_df(my_data, "reference_data.csv", conf$storage$sharepoint, filename = TRUE)
} # }
```
