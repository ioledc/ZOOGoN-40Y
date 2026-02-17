# Upload large file to SharePoint via resumable upload session

Uses the Microsoft Graph API createUploadSession endpoint to upload
files larger than 4 MB in chunks. Chunk size must be a multiple of 320
KB; this implementation uses 10 MB chunks.

## Usage

``` r
upload_large_file_to_sharepoint(
  file_path,
  remote_path,
  drive_id,
  token,
  chunk_size = 10 * 1024 * 1024
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

- chunk_size:

  Size of each upload chunk in bytes (default: 10 MB). Must be a
  multiple of 327680 (320 KB).

## Value

Invisible NULL
