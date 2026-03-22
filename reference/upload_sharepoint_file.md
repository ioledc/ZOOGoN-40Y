# Upload a local file to SharePoint

Uploads any local file (not just data frames) to a SharePoint document
library. Unlike
[`upload_sharepoint_df()`](https://ioledc.github.io/ZOOGoN-40Y/reference/upload_sharepoint_df.md),
this function accepts an arbitrary file path and format, making it
suitable for HTML reports, ZIP archives, and other file types.

## Usage

``` r
upload_sharepoint_file(
  file_path,
  remote_filename,
  options,
  bucket = NULL,
  format,
  filename = FALSE
)
```

## Arguments

- file_path:

  Local path to the file to upload

- remote_filename:

  Destination filename in SharePoint (versioned automatically unless
  `filename = TRUE`)

- options:

  SharePoint configuration list from
  config\$storage\$sharepoint\$credentials

- bucket:

  Bucket name (optional)

- format:

  File format/extension (e.g. "html", "zip", "csv")

- filename:

  Logical. If TRUE, use `remote_filename` as-is without versioning.
  Default is FALSE

## Value

Invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
conf <- read_config()
upload_sharepoint_file(
  file_path       = "report.html",
  remote_filename = "Report",
  options         = conf$storage$sharepoint$credentials,
  bucket          = conf$storage$sharepoint$buckets$reports_bucket,
  format          = "html"
)
} # }
```
