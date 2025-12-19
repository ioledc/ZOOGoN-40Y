# Download a file from SharePoint

Download a file from SharePoint

## Usage

``` r
download_sharepoint_file(
  prefix,
  options,
  bucket = NULL,
  format = c("csv", "tsv", "parquet", "xlsx"),
  filename = FALSE
)
```

## Arguments

- prefix:

  File prefix path (e.g., "raw", "preprocessed"), or exact filename if
  filename = TRUE

- options:

  SharePoint configuration list from config\$storage\$sharepoint

- bucket:

  Bucket name (optional)

- format:

  File format: "csv", "tsv", "xlsx", or "parquet". Default is "csv"

- filename:

  Logical. If TRUE, treat prefix as exact filename. Default is FALSE

## Value

Data frame with downloaded data

## Examples

``` r
if (FALSE) { # \dontrun{
conf <- read_config()
# Download latest version by prefix
data <- download_sharepoint_file("raw", conf$storage$sharepoint)
# Download specific file by name
data <- download_sharepoint_file("raw__2024-01-15.csv", conf$storage$sharepoint, filename = TRUE)
} # }
```
