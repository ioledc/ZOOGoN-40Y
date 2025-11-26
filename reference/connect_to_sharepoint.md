# Connect to SharePoint site

This function orchestrates the connection process:

1.  Gets an access token

2.  Retrieves the SharePoint site information

3.  Gets the document library (drive) ID

## Usage

``` r
connect_to_sharepoint(sp_conf)
```

## Arguments

- sp_conf:

  SharePoint configuration list

## Value

List with token and drive_id
