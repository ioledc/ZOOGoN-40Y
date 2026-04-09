# ==============================================================================
# SHAREPOINT STORAGE FUNCTIONS
# ==============================================================================
# This module provides functions to upload and download data to/from Microsoft
# SharePoint using the Microsoft Graph API. It follows a layered architecture:
#
# 1. Authentication Layer: Get access tokens for Microsoft Graph API
# 2. SharePoint Connection Layer: Connect to sites and get drive information
# 3. File Operations Layer: Upload and download files
# 4. High-Level Interface: User-friendly functions for common operations
#
# Configuration is read from inst/config.yml under storage$sharepoint section.
# ==============================================================================

# ------------------------------------------------------------------------------
# HIGH-LEVEL INTERFACE FUNCTIONS
# ------------------------------------------------------------------------------
# These are the main functions users should interact with. They provide simple
# interfaces for common tasks like uploading data frames or downloading files.

#' Upload a data frame to SharePoint
#'
#' @param data A data frame to upload
#' @param prefix File prefix path (e.g., "raw", "preprocessed"), or exact filename if filename = TRUE
#' @param options SharePoint configuration list from config$storage$sharepoint
#' @param bucket Bucket name (optional)
#' @param format File format: "csv", "tsv", "xlsx", "parquet", or "rds". Default is "csv"
#' @param filename Logical. If TRUE, treat prefix as exact filename and skip versioning. Default is FALSE
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' conf <- read_config()
#' # Upload with automatic versioning
#' upload_sharepoint_df(my_data, "raw", conf$storage$sharepoint)
#' # Upload to specific filename (no versioning)
#' upload_sharepoint_df(my_data, "reference_data.csv", conf$storage$sharepoint, filename = TRUE)
#' }
#'
#' @keywords storage
#' @export
upload_sharepoint_df <- function(
  data,
  prefix,
  options,
  bucket = NULL,
  format = c("csv", "tsv", "parquet", "xlsx", "rds"),
  filename = FALSE
) {
  # Set bucket if provided
  if (!is.null(bucket)) {
    options$bucket <- bucket
  }

  if (filename) {
    # Use exact filename without versioning
    remote_filename <- prefix
    format <- tools::file_ext(prefix)
    if (!format %in% c("csv", "tsv", "parquet", "xlsx", "rds")) {
      stop(
        sprintf(
          "Unsupported file format: %s. Must be csv, tsv, xlsx, parquet, or rds",
          format
        ),
        call. = FALSE
      )
    }
  } else {
    # Validate format parameter and add versioning
    format <- match.arg(format)
    remote_filename <- add_version(prefix, format)
  }

  # Build full remote path: bucket/filename
  remote_path <- file.path(options$bucket, remote_filename)

  # Write data frame to temporary file
  temp_file <- write_df_to_temp(data, format)
  on.exit(unlink(temp_file), add = TRUE)

  # Get SharePoint connection details
  sharepoint_conn <- connect_to_sharepoint(options)

  # Upload file
  upload_file_to_sharepoint(
    file_path = temp_file,
    remote_path = remote_path,
    drive_id = sharepoint_conn$drive_id,
    token = sharepoint_conn$token,
    format = format
  )

  logger::log_debug("Uploaded to: {remote_path}", namespace = "ZooGoN")
  invisible(NULL)
}

#' Upload a local file to SharePoint
#'
#' Uploads any local file (not just data frames) to a SharePoint document library.
#' Unlike [upload_sharepoint_df()], this function accepts an arbitrary file path and
#' format, making it suitable for HTML reports, ZIP archives, and other file types.
#'
#' @param file_path Local path to the file to upload
#' @param remote_filename Destination filename in SharePoint (versioned automatically
#'   unless `filename = TRUE`)
#' @param options SharePoint configuration list from config$storage$sharepoint$credentials
#' @param bucket Bucket name (optional)
#' @param format File format/extension (e.g. "html", "zip", "csv")
#' @param filename Logical. If TRUE, use `remote_filename` as-is without versioning.
#'   Default is FALSE
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' conf <- read_config()
#' upload_sharepoint_file(
#'   file_path       = "report.html",
#'   remote_filename = "Report",
#'   options         = conf$storage$sharepoint$credentials,
#'   bucket          = conf$storage$sharepoint$buckets$reports_bucket,
#'   format          = "html"
#' )
#' }
#'
#' @keywords storage
#' @export
upload_sharepoint_file <- function(
  file_path,
  remote_filename,
  options,
  bucket = NULL,
  format,
  filename = FALSE
) {
  if (!is.null(bucket)) {
    options$bucket <- bucket
  }

  dest_name <- if (filename) remote_filename else add_version(remote_filename, format)
  remote_path <- file.path(options$bucket, dest_name)

  sharepoint_conn <- connect_to_sharepoint(options)
  upload_file_to_sharepoint(
    file_path   = file_path,
    remote_path = remote_path,
    drive_id    = sharepoint_conn$drive_id,
    token       = sharepoint_conn$token,
    format      = format
  )

  logger::log_debug("Uploaded to: {remote_path}", namespace = "ZooGoN")
  invisible(NULL)
}

#' Download a file from SharePoint
#'
#' @param prefix File prefix path (e.g., "raw", "preprocessed"), or exact filename if filename = TRUE
#' @param options SharePoint configuration list from config$storage$sharepoint
#' @param bucket Bucket name (optional)
#' @param format File format: "csv", "tsv", "xlsx", "parquet", or "rds". Default is "csv"
#' @param filename Logical. If TRUE, treat prefix as exact filename. Default is FALSE
#'
#' @return Data frame with downloaded data
#'
#' @examples
#' \dontrun{
#' conf <- read_config()
#' # Download latest version by prefix
#' data <- download_sharepoint_file("raw", conf$storage$sharepoint)
#' # Download specific file by name
#' data <- download_sharepoint_file("raw__2024-01-15.csv", conf$storage$sharepoint, filename = TRUE)
#' }
#'
#' @keywords storage
#' @export
download_sharepoint_file <- function(
  prefix,
  options,
  bucket = NULL,
  format = c("csv", "tsv", "parquet", "xlsx", "rds"),
  filename = FALSE
) {
  # Set bucket if provided
  if (!is.null(bucket)) {
    options$bucket <- bucket
  }

  if (filename) {
    # Treat prefix as exact filename
    remote_path <- file.path(options$bucket, prefix)
    format <- tools::file_ext(prefix)
    if (!format %in% c("csv", "tsv", "parquet", "xlsx", "rds")) {
      stop(
        sprintf(
          "Unsupported file format: %s. Must be csv, tsv, xlsx, parquet, or rds",
          format
        ),
        call. = FALSE
      )
    }
    logger::log_debug("Downloading file: {prefix}", namespace = "ZooGoN")
  } else {
    # Find the latest versioned file with this prefix
    format <- match.arg(format)
    remote_path <- find_latest_version(prefix, options$bucket, format, options)
    logger::log_debug("Found file: {basename(remote_path)}", namespace = "ZooGoN")
  }

  # Get SharePoint connection details
  sharepoint_conn <- connect_to_sharepoint(options)

  # Create temp file for download
  temp_file <- tempfile(fileext = paste0(".", format))
  on.exit(unlink(temp_file), add = TRUE)

  # Download file
  download_file_from_sharepoint(
    remote_path = remote_path,
    dest = temp_file,
    drive_id = sharepoint_conn$drive_id,
    token = sharepoint_conn$token
  )

  logger::log_debug("Downloaded successfully", namespace = "ZooGoN")

  # Read and return data
  switch(
    format,
    csv = readr::read_csv(temp_file, show_col_types = FALSE),
    tsv = readr::read_tsv(temp_file, show_col_types = FALSE),
    parquet = arrow::read_parquet(temp_file),
    xlsx = openxlsx::read.xlsx(temp_file),
    rds = readr::read_rds(temp_file)
  )
}

# ------------------------------------------------------------------------------
# AUTHENTICATION LAYER
# ------------------------------------------------------------------------------
# Functions that handle Microsoft Graph API authentication

#' Get Microsoft Graph API access token
#'
#' Authenticates with Microsoft using client credentials (app registration)
#' and returns an access token for API calls.
#'
#' @param sp_conf SharePoint configuration list with tenant_id, client_id, and client_secret
#' @return Access token string
#' @keywords internal
get_ms_graph_token <- function(sp_conf) {
  token_url <- sprintf(
    "https://login.microsoftonline.com/%s/oauth2/v2.0/token",
    sp_conf$tenant_id
  )

  token_resp <- httr2::request(token_url) |>
    httr2::req_body_form(
      grant_type = "client_credentials",
      client_id = sp_conf$client_id,
      client_secret = sp_conf$client_secret,
      scope = "https://graph.microsoft.com/.default"
    ) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  httr2::resp_body_json(token_resp)$access_token
}

# ------------------------------------------------------------------------------
# SHAREPOINT CONNECTION LAYER
# ------------------------------------------------------------------------------
# Functions that establish connection to SharePoint sites and drives

#' Connect to SharePoint site
#'
#' This function orchestrates the connection process:
#' 1. Gets an access token
#' 2. Retrieves the SharePoint site information
#' 3. Gets the document library (drive) ID
#'
#' @param sp_conf SharePoint configuration list
#' @return List with token and drive_id
#' @keywords internal
connect_to_sharepoint <- function(sp_conf) {
  token <- get_ms_graph_token(sp_conf)
  site <- get_sharepoint_site(sp_conf$site_url, token)
  drive_id <- get_site_drive_id(site$id, token)

  list(
    token = token,
    drive_id = drive_id
  )
}

#' Get SharePoint site information
#'
#' Retrieves site metadata from Microsoft Graph API using the site URL.
#'
#' @param site_url Full SharePoint site URL
#' @param token Microsoft Graph API access token
#' @return Site information as a list
#' @keywords internal
get_sharepoint_site <- function(site_url, token) {
  # Parse site URL into host and path
  host <- sub("^https?://([^/]+)/?.*$", "\\1", site_url)
  path <- sub("^https?://[^/]+/?", "", site_url)
  path <- sub("^/+", "", path)

  # Build API endpoint
  site_endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/sites/%s:/%s",
    host,
    path
  )

  # Make API request
  site_resp <- httr2::request(site_endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", token)) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  httr2::resp_body_json(site_resp)
}

#' Get SharePoint site drive ID
#'
#' Retrieves the default document library (drive) ID for a SharePoint site.
#'
#' @param site_id SharePoint site ID
#' @param token Microsoft Graph API access token
#' @return Drive ID string
#' @keywords internal
get_site_drive_id <- function(site_id, token) {
  drive_endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/sites/%s/drive",
    site_id
  )

  drive_resp <- httr2::request(drive_endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", token)) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  httr2::resp_body_json(drive_resp)$id
}

# ------------------------------------------------------------------------------
# FILE OPERATIONS LAYER
# ------------------------------------------------------------------------------
# Core functions for uploading and downloading files

#' Upload file to SharePoint drive
#'
#' Uploads a local file to a SharePoint document library using Microsoft Graph API.
#' Files larger than 4 MB are automatically uploaded via a resumable upload session;
#' smaller files use a simple PUT request.
#'
#' @param file_path Local file path to upload
#' @param remote_path Destination path in SharePoint
#' @param drive_id SharePoint drive ID
#' @param token Microsoft Graph API access token
#' @param format File format for content-type header
#' @param overwrite Should existing files be replaced?
#' @keywords internal
upload_file_to_sharepoint <- function(
  file_path,
  remote_path,
  drive_id,
  token,
  format,
  overwrite = TRUE
) {
  # Microsoft Graph simple upload limit is 4 MB
  max_simple_size <- 4 * 1024 * 1024
  file_size <- file.info(file_path)$size

  if (file_size > max_simple_size) {
    return(
      upload_large_file_to_sharepoint(
        file_path = file_path,
        remote_path = remote_path,
        drive_id = drive_id,
        token = token
      )
    )
  }

  # Build upload endpoint
  encoded_path <- utils::URLencode(remote_path, reserved = TRUE)
  upload_endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root:/%s:/content",
    drive_id,
    encoded_path
  )

  # Prepare request
  req <- httr2::request(upload_endpoint) |>
    httr2::req_method("PUT") |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      "Content-Type" = get_content_type(format)
    ) |>
    httr2::req_body_file(file_path)

  # Add If-None-Match header if overwrite is FALSE
  if (!overwrite) {
    req <- req |> httr2::req_headers("If-None-Match" = "*")
  }

  # Execute upload
  httr2::req_perform(req) |>
    httr2::resp_check_status()

  invisible(NULL)
}

#' Upload large file to SharePoint via resumable upload session
#'
#' Uses the Microsoft Graph API createUploadSession endpoint to upload files
#' larger than 4 MB in chunks. Chunk size must be a multiple of 320 KB;
#' this implementation uses 10 MB chunks.
#'
#' @param file_path Local file path to upload
#' @param remote_path Destination path in SharePoint
#' @param drive_id SharePoint drive ID
#' @param token Microsoft Graph API access token
#' @param chunk_size Size of each upload chunk in bytes (default: 10 MB).
#'   Must be a multiple of 327680 (320 KB).
#' @return Invisible NULL
#' @keywords internal
upload_large_file_to_sharepoint <- function(
  file_path,
  remote_path,
  drive_id,
  token,
  chunk_size = 10 * 1024 * 1024
) {
  file_size <- file.info(file_path)$size
  logger::log_info("Large file upload: {basename(file_path)} ({round(file_size / 1024 / 1024, 1)} MB)", namespace = "ZooGoN")

  # Create upload session
  encoded_path <- utils::URLencode(remote_path, reserved = TRUE)
  session_endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root:/%s:/createUploadSession",
    drive_id,
    encoded_path
  )

  session_resp <- httr2::request(session_endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      item = list(
        "@microsoft.graph.conflictBehavior" = "replace"
      )
    )) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  upload_url <- httr2::resp_body_json(session_resp)$uploadUrl

  # Upload file in chunks
  con <- file(file_path, "rb")
  on.exit(close(con), add = TRUE)

  offset <- 0
  while (offset < file_size) {
    chunk_data <- readBin(con, "raw", n = chunk_size)
    current_chunk_size <- length(chunk_data)
    range_end <- offset + current_chunk_size - 1

    content_range <- sprintf(
      "bytes %d-%d/%d",
      offset, range_end, file_size
    )

    httr2::request(upload_url) |>
      httr2::req_method("PUT") |>
      httr2::req_headers(
        "Content-Length" = current_chunk_size,
        "Content-Range" = content_range
      ) |>
      httr2::req_body_raw(chunk_data) |>
      httr2::req_perform() |>
      httr2::resp_check_status()

    offset <- offset + current_chunk_size
    logger::log_debug("Uploaded {round(offset / file_size * 100)}% ({round(offset / 1024 / 1024, 1)} / {round(file_size / 1024 / 1024, 1)} MB)", namespace = "ZooGoN")
  }

  invisible(NULL)
}

#' Download file from SharePoint drive
#'
#' Downloads a file from SharePoint document library to local filesystem.
#'
#' @param remote_path Path to file in SharePoint
#' @param dest Local destination path
#' @param drive_id SharePoint drive ID
#' @param token Microsoft Graph API access token
#' @keywords internal
download_file_from_sharepoint <- function(remote_path, dest, drive_id, token) {
  # Build download endpoint
  encoded_path <- utils::URLencode(remote_path, reserved = TRUE)
  download_endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root:/%s:/content",
    drive_id,
    encoded_path
  )

  # Download file
  resp <- httr2::request(download_endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", token)) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  # Write to destination
  writeBin(httr2::resp_body_raw(resp), dest)

  invisible(NULL)
}

# ------------------------------------------------------------------------------
# UTILITY FUNCTIONS
# ------------------------------------------------------------------------------
# Helper functions for common tasks

#' Find latest versioned file in SharePoint
#'
#' Searches for files matching the prefix pattern and returns the latest version.
#'
#' @param prefix File prefix to search for
#' @param bucket Bucket/folder path
#' @param format File format extension
#' @param options SharePoint options with connection details
#' @return Full path to the latest versioned file
#' @keywords internal
find_latest_version <- function(prefix, bucket, format, options) {
  # Get connection to SharePoint
  sharepoint_conn <- connect_to_sharepoint(options)

  # Build API endpoint to list files in the bucket folder
  folder_path <- utils::URLencode(bucket, reserved = TRUE)
  list_endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/drives/%s/root:/%s:/children",
    sharepoint_conn$drive_id,
    folder_path
  )

  # List files in the folder (handle pagination via @odata.nextLink)
  files <- list()
  next_url <- list_endpoint
  while (!is.null(next_url)) {
    list_resp <- httr2::request(next_url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", sharepoint_conn$token)
      ) |>
      httr2::req_perform() |>
      httr2::resp_check_status()

    body <- httr2::resp_body_json(list_resp)
    files <- c(files, body$value)
    next_url <- body$`@odata.nextLink`
  }

  # Filter files matching the pattern: prefix__*.*format
  pattern <- sprintf("^%s__.*\\.%s$", prefix, format)
  matching_files <- sapply(files, function(f) f$name)
  matching_files <- matching_files[grepl(pattern, matching_files)]

  if (length(matching_files) == 0) {
    stop(
      sprintf(
        "No files found matching pattern '%s' in folder '%s'",
        pattern,
        bucket
      ),
      call. = FALSE
    )
  }

  # Sort by name (version timestamps are sortable) and get the latest
  latest_file <- sort(matching_files, decreasing = TRUE)[1]

  file.path(bucket, latest_file)
}

#' Clean path by removing leading slashes
#'
#' @param path File path
#' @return Cleaned path
#' @keywords internal
clean_path <- function(path) {
  sub("^/+", "", path)
}

#' Write data frame to temporary file
#'
#' Writes a data frame to a temporary file in the specified format.
#'
#' @param df Data frame to write
#' @param format File format (csv, tsv, or parquet)
#' @return Path to temporary file
#' @keywords internal
write_df_to_temp <- function(df, format) {
  # Create temp file with appropriate extension
  temp_file <- tempfile(fileext = paste0(".", format))

  # Write data frame
  switch(
    format,
    csv = readr::write_csv(df, temp_file),
    tsv = readr::write_tsv(df, temp_file),
    parquet = arrow::write_parquet(df, temp_file),
    xlsx = openxlsx::write.xlsx(df, temp_file),
    rds = readr::write_rds(df, temp_file)
  )
  temp_file
}
#' Get MIME content type for file format
#'
#' Returns the appropriate MIME type for the specified file format.
#'
#' @param format File format (csv, tsv, or parquet)
#' @return MIME type string
#' @keywords internal
get_content_type <- function(format) {
  switch(
    format,
    csv     = "text/csv",
    tsv     = "text/tab-separated-values",
    parquet = "application/vnd.apache.parquet",
    xlsx    = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    rds     = "application/octet-stream",
    zip     = "application/zip",
    html    = "text/html",
    stop(sprintf("Unsupported format: %s", format), call. = FALSE)
  )
}

#' Add version to file path
#'
#' Applies the add_version() function to a file path, handling directory structure.
#' This wraps add_version() to work with full paths including directories.
#'
#' @param remote_path Full remote path (e.g., "data/my-file.csv")
#' @param format File format (csv, tsv, or parquet)
#' @return Versioned path (e.g., "data/my-file__20240122211850_abc1234.csv")
#' @keywords internal
add_version_to_path <- function(remote_path, format) {
  # Split path into directory and filename
  dir_part <- dirname(remote_path)
  file_part <- basename(remote_path)

  # Remove extension from filename if present
  file_base <- tools::file_path_sans_ext(file_part)

  # Apply versioning to base filename
  versioned_file <- add_version(file_base, format)

  # Reconstruct full path
  if (dir_part == "." || dir_part == "") {
    versioned_file
  } else {
    file.path(dir_part, versioned_file)
  }
}