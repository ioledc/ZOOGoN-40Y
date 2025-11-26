# Find latest versioned file in SharePoint

Searches for files matching the prefix pattern and returns the latest
version.

## Usage

``` r
find_latest_version(prefix, bucket, format, options)
```

## Arguments

- prefix:

  File prefix to search for

- bucket:

  Bucket/folder path

- format:

  File format extension

- options:

  SharePoint options with connection details

## Value

Full path to the latest versioned file
