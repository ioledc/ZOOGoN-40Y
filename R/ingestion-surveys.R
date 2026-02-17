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

  logger::log_info("Converting MC Survey Kobo data to tabular format...", namespace = "ZooGoN")
  raw_survey <-
    purrr::map(data_raw, flatten_row) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(submission_id = "_id")

  logger::log_debug("Flattened survey: {nrow(raw_survey)} rows, {ncol(raw_survey)} columns", namespace = "ZooGoN")

  logger::log_info("Uploading raw survey data (CSV + Parquet)...", namespace = "ZooGoN")
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
#'
#' @return A list containing all retrieved survey results.
#' @keywords ingestion
#' @details
#' The function uses pagination to retrieve large datasets, with a limit of 30,000 records per request.
#' It continues to fetch data until all records are retrieved or an error occurs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kobo_data <- get_kobo_data(
#'   assetid = "your_asset_id",
#'   uname = "your_username",
#'   pwd = "your_password"
#' )
#' }
get_kobo_data <- function(
  assetid,
  url = "eu.kobotoolbox.org",
  uname = NULL,
  pwd = NULL,
  encoding = "UTF-8",
  format = "json"
) {
  if (!is.character(url)) {
    stop("URL entered is not a string")
  }
  if (!is.character(uname)) {
    stop("uname (username) entered is not a string")
  }
  if (!is.character(pwd)) {
    stop("pwd (password) entered is not a string")
  }
  if (!is.character(assetid)) {
    stop("assetid entered is not a string")
  }
  if (is.null(url) || url == "") {
    stop("URL empty")
  }
  if (is.null(uname) || uname == "") {
    stop("uname (username) empty")
  }
  if (is.null(pwd) || pwd == "") {
    stop("pwd (password) empty")
  }
  if (is.null(assetid) || assetid == "") {
    stop("assetid empty")
  }
  if (!format %in% c("json", "xml")) {
    stop("format must be either 'json' or 'xml'")
  }

  base_url <- paste0(
    "https://",
    url,
    "/api/v2/assets/",
    assetid,
    "/data.",
    format
  )

  logger::log_info("Starting data retrieval from {base_url}", namespace = "ZooGoN")

  get_page <- function(url, limit = 30000, start = 0) {
    full_url <- paste0(url, "?limit=", limit, "&start=", start)

    logger::log_debug("Retrieving page starting at record {start}", namespace = "ZooGoN")

    respon.kpi <- tryCatch(
      expr = {
        httr2::request(full_url) |>
          httr2::req_auth_basic(uname, pwd) |>
          httr2::req_perform()
      },
      error = function(x) {
        logger::log_error("Error on page starting at record {start}. Please try again or check the input parameters.", namespace = "ZooGoN")
        return(NULL)
      }
    )

    if (!is.null(respon.kpi)) {
      content_type <- httr2::resp_content_type(respon.kpi)

      if (grepl("json", content_type)) {
        logger::log_debug("Successfully retrieved JSON data starting at record {start}", namespace = "ZooGoN")
        return(httr2::resp_body_json(respon.kpi, encoding = encoding))
      } else if (grepl("xml", content_type)) {
        logger::log_debug("Successfully retrieved XML data starting at record {start}", namespace = "ZooGoN")
        return(httr2::resp_body_string(respon.kpi, encoding = encoding))
      } else if (grepl("html", content_type)) {
        warning(
          "Unexpected HTML response for start ",
          start,
          ". Unable to parse."
        )
        return(NULL)
      } else {
        warning(
          "Unexpected content type: ",
          content_type,
          " for start ",
          start,
          ". Unable to parse."
        )
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }

  all_results <- list()
  start <- 0
  limit <- 30000
  get_next <- TRUE

  while (get_next) {
    page_results <- get_page(base_url, limit, start)

    if (is.null(page_results)) {
      logger::log_error("Error occurred. Stopping data retrieval.", namespace = "ZooGoN")
      break
    }

    new_results <- page_results$results
    all_results <- c(all_results, new_results)

    logger::log_debug("Total records retrieved so far: {length(all_results)}", namespace = "ZooGoN")

    if (length(new_results) < limit) {
      logger::log_info("Retrieved all available records.", namespace = "ZooGoN")
      get_next <- FALSE
    } else {
      start <- start + limit
    }
  }

  logger::log_info("Data retrieval complete. Total records: {length(all_results)}", namespace = "ZooGoN")

  # Check for unique submission IDs
  submission_ids <- sapply(all_results, function(x) x$`_id`)
  if (length(unique(submission_ids)) != length(all_results)) {
    warning(
      "Number of unique submission IDs does not match the number of records. There may be duplicates."
    )
  }

  return(all_results)
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
