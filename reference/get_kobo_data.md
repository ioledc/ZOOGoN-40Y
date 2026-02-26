# Retrieve Data from Kobotoolbox API

This function retrieves survey data from Kobotoolbox API for a specific
asset. It supports pagination and handles both JSON and XML formats.

## Usage

``` r
get_kobo_data(
  assetid,
  url = "eu.kobotoolbox.org",
  uname = NULL,
  pwd = NULL,
  encoding = "UTF-8",
  format = "json",
  limit = 1000,
  since_id = NULL,
  retry_times = 3,
  progress = TRUE
)
```

## Arguments

- assetid:

  The asset ID of the Kobotoolbox form.

- url:

  The URL of Kobotoolbox (default is "eu.kobotoolbox.org").

- uname:

  Username for Kobotoolbox account.

- pwd:

  Password for Kobotoolbox account.

- encoding:

  Encoding to be used for data retrieval (default is "UTF-8").

- format:

  Format of the data to retrieve, either "json" or "xml" (default is
  "json").

- limit:

  Number of records per page (default 1000). Maximum allowed is 1000.

- since_id:

  Optional. If provided, only fetch submissions with `_id` greater than
  or equal to this value. Useful for incremental data retrieval.

- retry_times:

  Number of retry attempts for failed requests (default is 3).

- progress:

  Logical. Whether to show a progress message (default is TRUE).

## Value

A list containing all retrieved survey results.

## Details

As of March 2026, the Kobotoolbox API enforces a maximum page size of
1,000 records per request (previously 30,000). The default page size if
not specified is 100. This function uses pagination via the
[`next`](https://rdrr.io/r/base/Control.html) field in the API response
to iterate through all available records.

For incremental data retrieval (e.g., syncing only new submissions), use
the `since_id` parameter with the last known `_id` value.

Note: This change does NOT affect synchronous export endpoints
(`/api/v2/assets/{uid}/export-settings/{uid_export}/data.xlsx|csv`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Full retrieval
kobo_data <- get_kobo_data(
  assetid = "your_asset_id",
  uname = "your_username",
  pwd = "your_password"
)

# Incremental retrieval (only new records since last sync)
new_data <- get_kobo_data(
  assetid = "your_asset_id",
  uname = "your_username",
  pwd = "your_password",
  since_id = 52149
)
} # }
```
