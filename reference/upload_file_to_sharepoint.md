# Upload file to SharePoint drive

Uploads a local file to a SharePoint document library using Microsoft
Graph API. Files larger than 4 MB are automatically uploaded via a
resumable upload session; smaller files use a simple PUT request.

## Usage

``` r
upload_file_to_sharepoint(
  file_path,
  remote_path,
  drive_id,
  token,
  format,
  overwrite = TRUE
)
```

## Arguments

- file_path:

  Local file path to upload

- remote_path:

  Destination path in SharePoint

- drive_id:

  SharePoint drive ID

- token:

  Microsoft Graph API access token

- format:

  File format for content-type header

- overwrite:

  Should existing files be replaced?
