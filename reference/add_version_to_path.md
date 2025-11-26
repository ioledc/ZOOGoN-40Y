# Add version to file path

Applies the add_version() function to a file path, handling directory
structure. This wraps add_version() to work with full paths including
directories.

## Usage

``` r
add_version_to_path(remote_path, format)
```

## Arguments

- remote_path:

  Full remote path (e.g., "data/my-file.csv")

- format:

  File format (csv, tsv, or parquet)

## Value

Versioned path (e.g., "data/my-file\_\_20240122211850_abc1234.csv")
