#' Download MC Surveys from Kobotoolbox
#'
#' This function retrieves MC survey data from Kobotoolbox, processes it,
#' and uploads the raw data to Google Cloud Storage as Parquet files. It uses the
#' `get_kobo_data` function to retrieve survey submissions via the Kobotoolbox API.
#'
#' @return Invisible NULL. Function downloads data, processes it, and uploads to Google Cloud Storage.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads configuration settings from config.yml
#' 2. Downloads survey data from Kobotoolbox using `get_kobo_data`
#' 3. Checks for uniqueness of submissions
#' 4. Flattens nested JSON data to tabular format
#' 5. Uploads raw data as versioned Parquet file to Google Cloud Storage
#'
#' @note The function uses configuration values from config.yml:
#' - Hardcoded URL: "eu.kobotoolbox.org"
#' - Hardcoded encoding: "UTF-8"
#' - Configuration values for: asset_id, username, password (shared with Lurio)
#' - GCS bucket and credentials from configuration
#'
#' @keywords workflow ingestion
#' @export
#'
#' @examples
#' \dontrun{
#' ingest_surveys()
#' }
ingest_surveys <- function() {
  conf <- read_config()

  logger::log_info("Downloading MC Survey Kobo data...", namespace = "ZooGoN")
  data_raw <-
    get_kobo_data(
      url = "eu.kobotoolbox.org",
      assetid = conf$ingestion$kobo$asset_id,
      uname = conf$ingestion$kobo$username,
      pwd = conf$ingestion$kobo$password,
      encoding = "UTF-8",
      format = "json"
    )

  # Check that submissions are unique in case there is overlap in the pagination
  if (
    dplyr::n_distinct(purrr::map_dbl(data_raw, ~ .$`_id`)) != length(data_raw)
  ) {
    stop("Number of submission ids not the same as number of records")
  }

  logger::log_info(
    "Converting MC Survey Kobo data to tabular format...",
    namespace = "ZooGoN"
  )
  raw_survey <-
    purrr::map(data_raw, flatten_row) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(submission_id = "_id")

  logger::log_debug(
    "Flattened survey: {nrow(raw_survey)} rows, {ncol(raw_survey)} columns",
    namespace = "ZooGoN"
  )

  logger::log_info(
    "Uploading raw survey data (CSV + Parquet)...",
    namespace = "ZooGoN"
  )
  c("csv", "parquet") |>
    purrr::walk(
      ~ upload_sharepoint_df(
        data = raw_survey,
        prefix = conf$ingestion$surveys$raw$file_prefix,
        options = conf$storage$sharepoint$credentials,
        bucket = conf$storage$sharepoint$buckets$automation_bucket,
        format = .
      )
    )

  logger::log_success("ingest_surveys complete", namespace = "ZooGoN")
}

#' Retrieve Data from Kobotoolbox API
#'
#' This function retrieves survey data from Kobotoolbox API for a specific asset.
#' It supports pagination and handles both JSON and XML formats.
#'
#' @param assetid The asset ID of the Kobotoolbox form.
#' @param url The URL of Kobotoolbox (default is "eu.kobotoolbox.org").
#' @param uname Username for Kobotoolbox account.
#' @param pwd Password for Kobotoolbox account.
#' @param encoding Encoding to be used for data retrieval (default is "UTF-8").
#' @param format Format of the data to retrieve, either "json" or "xml" (default is "json").
#' @param limit Number of records per page (default 1000). Maximum allowed is 1000.
#' @param since_id Optional. If provided, only fetch submissions with `_id`
#'   greater than or equal to this value. Useful for incremental data retrieval.
#' @param retry_times Number of retry attempts for failed requests (default is 3).
#' @param progress Logical. Whether to show a progress message (default is TRUE).
#'
#' @return A list containing all retrieved survey results.
#' @keywords ingestion
#' @details
#' As of March 2026, the Kobotoolbox API enforces a maximum page size of 1,000
#' records per request (previously 30,000). The default page size if not specified
#' is 100. This function uses pagination via the `next` field in the API response
#' to iterate through all available records.
#'
#' For incremental data retrieval (e.g., syncing only new submissions), use the
#' `since_id` parameter with the last known `_id` value.
#'
#' Note: This change does NOT affect synchronous export endpoints
#' (`/api/v2/assets/{uid}/export-settings/{uid_export}/data.xlsx|csv`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Full retrieval
#' kobo_data <- get_kobo_data(
#'   assetid = "your_asset_id",
#'   uname = "your_username",
#'   pwd = "your_password"
#' )
#'
#' # Incremental retrieval (only new records since last sync)
#' new_data <- get_kobo_data(
#'   assetid = "your_asset_id",
#'   uname = "your_username",
#'   pwd = "your_password",
#'   since_id = 52149
#' )
#' }
get_kobo_data <- function(
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
) {
  # --- Input validation ---
  if (is.null(uname) || !is.character(uname) || uname == "") {
    stop("`uname` (username) must be a non-empty string.")
  }
  if (is.null(pwd) || !is.character(pwd) || pwd == "") {
    stop("`pwd` (password) must be a non-empty string.")
  }
  if (is.null(assetid) || !is.character(assetid) || assetid == "") {
    stop("`assetid` must be a non-empty string.")
  }
  if (is.null(url) || !is.character(url) || url == "") {
    stop("`url` must be a non-empty string.")
  }
  if (!format %in% c("json", "xml")) {
    stop("`format` must be either 'json' or 'xml'.")
  }
  if (!is.numeric(limit) || limit < 1 || limit > 1000) {
    stop("`limit` must be a number between 1 and 1000.")
  }
  limit <- as.integer(limit)

  base_url <- paste0(
    "https://",
    url,
    "/api/v2/assets/",
    assetid,
    "/data.",
    format
  )

  if (progress) {
    message("Starting data retrieval from ", base_url)
  }

  # --- Page fetcher ---
  get_page <- function(page_url) {
    response <- tryCatch(
      expr = {
        httr2::request(page_url) |>
          httr2::req_auth_basic(uname, pwd) |>
          httr2::req_retry(max_tries = retry_times) |>
          httr2::req_error(is_error = \(resp) FALSE) |>
          httr2::req_perform()
      },
      error = function(e) {
        warning("Request failed: ", conditionMessage(e))
        return(NULL)
      }
    )

    if (is.null(response)) {
      return(NULL)
    }

    status <- httr2::resp_status(response)
    if (status >= 400) {
      warning(
        "HTTP error ",
        status,
        " when fetching: ",
        page_url,
        "\nBody: ",
        tryCatch(
          httr2::resp_body_string(response),
          error = function(e) "(unable to read body)"
        )
      )
      return(NULL)
    }

    content_type <- httr2::resp_content_type(response)

    if (grepl("json", content_type)) {
      return(httr2::resp_body_json(response, encoding = encoding))
    } else if (grepl("xml", content_type)) {
      return(httr2::resp_body_string(response, encoding = encoding))
    } else {
      warning("Unexpected content type: ", content_type)
      return(NULL)
    }
  }

  # --- Build initial URL with query params ---
  initial_url <- paste0(base_url, "?limit=", limit, "&start=0")

  if (!is.null(since_id)) {
    query_json <- paste0('{"_id":{"$gte":', since_id, '}}')
    initial_url <- paste0(
      initial_url,
      "&query=",
      utils::URLencode(query_json, reserved = TRUE)
    )
  }

  # --- Pagination loop using `next` field ---
  all_results <- list()
  current_url <- initial_url
  page_num <- 1L

  repeat {
    if (progress) {
      message("Fetching page ", page_num, "...")
    }

    page_data <- get_page(current_url)

    if (is.null(page_data)) {
      warning("Failed to retrieve page ", page_num, ". Stopping.")
      break
    }

    new_results <- page_data$results
    if (is.null(new_results) || length(new_results) == 0) {
      if (progress) {
        message("No results on page ", page_num, ". Done.")
      }
      break
    }

    all_results <- c(all_results, new_results)

    if (progress) {
      message(
        "Page ",
        page_num,
        ": retrieved ",
        length(new_results),
        " records (total: ",
        length(all_results),
        " / ",
        if (!is.null(page_data$count)) page_data$count else "unknown",
        ")"
      )
    }

    # Use the `next` URL provided by the API for pagination
    next_url <- page_data$`next`
    if (is.null(next_url) || identical(next_url, "")) {
      if (progress) {
        message("No more pages. Retrieval complete.")
      }
      break
    }

    current_url <- next_url
    page_num <- page_num + 1L
  }

  if (progress) {
    message("Data retrieval complete. Total records: ", length(all_results))
  }

  # --- Check for duplicate submission IDs ---
  if (length(all_results) > 0) {
    submission_ids <- vapply(
      all_results,
      function(x) if (!is.null(x$`_id`)) x$`_id` else NA_integer_,
      integer(1)
    )
    n_unique <- length(unique(submission_ids[!is.na(submission_ids)]))
    if (n_unique != length(all_results)) {
      warning(
        "Found ",
        length(all_results) - n_unique,
        " duplicate submission IDs out of ",
        length(all_results),
        " records."
      )
    }
  }

  all_results
}


#' Flatten Survey Data Rows
#'
#' Transforms each row of nested survey data into a flat tabular format using a mapping and flattening process.
#'
#' @param x A list representing a row of data, potentially containing nested lists or vectors.
#' @return A tibble with each row representing flattened survey data.
#' @keywords internal
flatten_row <- function(x) {
  x %>%
    # Each row is composed of several fields
    purrr::imap(flatten_field) %>%
    rlang::squash() %>%
    # Remove NULL values before creating tibble
    purrr::compact() %>%
    tibble::as_tibble(.name_repair = "unique")
}

#' Flatten Survey Data Fields
#'
#' Processes each field within a row of survey data, handling both simple vectors and nested lists. For lists with named elements, renames and unlists them for flat structure preparation.
#'
#' @param x A vector or list representing a field in the data.
#' @param p The prefix or name associated with the field, used for naming during the flattening process.
#' @return Modified field, either unchanged, unnested, or appropriately renamed.
#' @keywords internal
flatten_field <- function(x, p) {
  # If the field is a simple vector do nothing but if the field is a list we
  # need more logic
  if (inherits(x, "list")) {
    if (length(x) > 0) {
      if (purrr::vec_depth(x) == 2) {
        # If the field-list has named elements is we just need to rename the list
        x <- list(x) %>%
          rlang::set_names(p) %>%
          unlist() %>%
          as.list()
      } else {
        # If the field-list is an "array" we need to iterate over its children
        x <- purrr::imap(x, rename_child, p = p)
      }
    } else {
      # Handle empty lists by returning NULL (will be removed by compact)
      return(NULL)
    }
  } else {
    if (is.null(x)) x <- NA
  }
  x
}

#' Rename Nested Survey Data Elements
#'
#' Appends a parent name or index to child elements within a nested list, assisting in creating a coherent and traceable data structure during the flattening process.
#'
#' @param x A list element, possibly nested, to be renamed.
#' @param i The index or key of the element within the parent list.
#' @param p The parent name to prepend to the element's existing name for context.
#' @return A renamed list element, structured to maintain contextual relevance in a flattened dataset.
#' @keywords internal
rename_child <- function(x, i, p) {
  if (length(x) == 0) {
    if (is.null(x)) {
      x <- NA
    }
    x <- list(x)
    x <- rlang::set_names(x, paste(p, i - 1, sep = "."))
  } else {
    if (inherits(i, "character")) {
      x <- rlang::set_names(x, paste(p, i, sep = "."))
    } else if (inherits(i, "integer")) {
      x <- rlang::set_names(x, paste(p, i - 1, names(x), sep = "."))
    }
  }
  x
}
