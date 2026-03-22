#' Build a Darwin Core Archive and upload to SharePoint
#'
#' Downloads the Darwin Core tables produced by `format_to_dc()` from SharePoint,
#' builds a Darwin Core Archive zip with an EML file, and uploads the archive
#' back to SharePoint.
#'
#' @return Invisible list with paths to the archive and EML.
#' @export
format_to_DC_archive <- function() {
  conf <- read_config()

  dc_list <- format_to_dc(verbose = FALSE)

  event_df <- dc_list$event

  logger::log_info(
    "Initializing DwC core and extensions...",
    namespace = "ZooGoN"
  )
  core_event <- LivingNorwayR::initializeGBIFEvent(
    event_df,
    idColumnInfo = "eventID",
    nameAutoMap = TRUE
  )

  occ_ext <- LivingNorwayR::initializeGBIFOccurrence(
    dc_list$occurrence,
    idColumnInfo = "eventID",
    nameAutoMap = TRUE
  )

  emof_ext <- LivingNorwayR::initializeGBIFMeasurementOrFact(
    dc_list$emof,
    idColumnInfo = "eventID",
    nameAutoMap = TRUE
  )

  logger::log_info("Generating EML metadata...", namespace = "ZooGoN")
  eml_obj <- get_metadata(event_df)
  eml_path <- file.path(tempdir(), "mc_eml.xml")

  EML::write_eml(eml_obj, eml_path)
  add_gbif_license_block(eml_path)
  EML::eml_validate(eml_path)
  #TODO: Validate EML properly
  logger::log_debug("EML validated successfully", namespace = "ZooGoN")

  metadata <- LivingNorwayR::initializeDwCMetadata(
    fileLocation = eml_path,
    fileEncoding = "UTF-8",
    fileType = "eml"
  )

  dwc <- LivingNorwayR::initializeDwCArchive(
    coreDwC = core_event,
    extDwC = list(occ_ext, emof_ext),
    metadata = metadata
  )

  zip_file <- add_version("ZooGoN_dwca", extension = "zip")

  logger::log_info(
    "Exporting DwC-A zip: {basename(zip_file)}",
    namespace = "ZooGoN"
  )
  dwc$exportAsDwCArchive(
    fileName = zip_file,
    emlLocation = basename(eml_path)
  )

  logger::log_info("Uploading archive to SharePoint...", namespace = "ZooGoN")
  sp_conn <- connect_to_sharepoint(conf$storage$sharepoint$credentials)
  remote_path <- file.path(
    conf$storage$sharepoint$buckets$darwin_core_bucket,
    basename(zip_file)
  )

  upload_file_to_sharepoint(
    file_path = zip_file,
    remote_path = remote_path,
    drive_id = sp_conn$drive_id,
    token = sp_conn$token,
    format = "zip"
  )

  logger::log_success(
    "dc_to_archive complete -- uploaded to: {remote_path}",
    namespace = "ZooGoN"
  )
  invisible(list(archive_path = zip_file, eml_path = eml_path))
}

#' Build basic EML metadata
#'
#' Creates a simple EML list for the MareChiara dataset using the supplied
#' event table to derive the date range.
#'
#' @param event_df Event data frame with `eventDate`.
#'
#' @return A list suitable for `EML::write_eml()`.
#' @keywords internal
get_metadata <- function(event_df = NULL) {
  me <- list(
    individualName = list(
      givenName = "Iole",
      surName = "Di Capua"
    ),
    organizationName = "Stazione Zoologica Anton Dohrn",
    electronicMailAddress = "iole.dicapua@szn.it"
  )

  begin_date <- if (!is.null(event_df)) {
    min(event_df$eventDate, na.rm = TRUE)
  } else {
    NA
  }
  end_date <- if (!is.null(event_df)) {
    max(event_df$eventDate, na.rm = TRUE)
  } else {
    NA
  }

  eml_obj <- list(
    dataset = list(
      title = paste0(
        "Zooplankton data at LTER MareChiara site in the Gulf of Naples from 1984-",
        max(lubridate::year(event_df$eventDate))
      ),
      abstract = list(
        para = "Zooplankton vertical tows 0-50 m at LTER-MareChiara, 1984-2024."
      ),
      creator = me,
      contact = me,
      coverage = list(
        geographicCoverage = list(
          geographicDescription = "Gulf of Naples, LTER-MareChiara station",
          boundingCoordinates = list(
            westBoundingCoordinate = 14.25,
            eastBoundingCoordinate = 14.25,
            northBoundingCoordinate = 40.81,
            southBoundingCoordinate = 40.81
          )
        ),
        temporalCoverage = list(
          rangeOfDates = list(
            beginDate = list(calendarDate = begin_date),
            endDate = list(calendarDate = end_date)
          )
        )
      ),
      intellectualRights = list(para = "CC-BY-NC")
    )
  )
  eml_obj
}

#' Add GBIF-style license block to an EML file
#'
#' This helper modifies an existing EML file by inserting a
#' GBIF/IPT-compatible \code{<intellectualRights>} element.
#' Any existing \code{<intellectualRights>} node is removed and
#' replaced with a paragraph containing a \code{<ulink>} pointing
#' to the chosen license URL. The new block is inserted before
#' \code{<coverage>} (or \code{<contact>} if \code{<coverage>} is
#' missing) to satisfy the GBIF EML profile element order.
#'
#' @param eml_path Character string giving the path to an EML file
#'   on disk. The file is modified in place.
#' @param url Character string with the license URL. Defaults to
#'   the Creative Commons Attribution Non Commercial 4.0
#'   International licence URL.
#' @param title Character string with the human-readable licence
#'   title used inside \code{<citetitle>}.
#'
#' @return Invisibly returns \code{eml_path}.
#'
#' @examples
#' \dontrun{
#' eml_path <- "mc_eml.xml"
#' add_gbif_license_block(eml_path)
#' }
#'
#' @export
add_gbif_license_block <- function(
  eml_path,
  url = "http://creativecommons.org/licenses/by-nc/4.0/legalcode",
  title = "Creative Commons Attribution Non Commercial (CC-BY-NC) 4.0 License"
) {
  doc <- xml2::read_xml(eml_path)
  dataset <- xml2::xml_find_first(doc, ".//dataset")

  # 1. Remove any existing <intellectualRights>
  xml2::xml_remove(xml2::xml_find_all(dataset, "intellectualRights"))

  # 2. New GBIF-style block
  ir_xml <- sprintf(
    '
    <intellectualRights>
      <para>This work is licensed under a
        <ulink url="%s">
          <citetitle>%s</citetitle>
        </ulink>.
      </para>
    </intellectualRights>
  ',
    url,
    title
  )

  ir_node <- xml2::read_xml(ir_xml)

  # 3. Insert in correct position in the GBIF dataset sequence:
  #    ideally after abstract and BEFORE coverage/contact.
  cov_node <- xml2::xml_find_first(dataset, "coverage")
  if (!inherits(cov_node, "xml_missing")) {
    # insert before <coverage>
    xml2::xml_add_sibling(cov_node, ir_node, .where = "before")
  } else {
    contact_node <- xml2::xml_find_first(dataset, "contact")
    if (!inherits(contact_node, "xml_missing")) {
      # fallback: before <contact>
      xml2::xml_add_sibling(contact_node, ir_node, .where = "before")
    } else {
      # last resort: append at end (should not happen in your case)
      xml2::xml_add_child(dataset, ir_node)
    }
  }

  xml2::write_xml(doc, eml_path)
  invisible(eml_path)
}

#' Register a hosted archive on GBIF
#'
#' Create a dataset entry in GBIF and point it to your public DwC-A zip.
#' You must supply valid GBIF organization and installation UUIDs and a
#' URL that anyone can download.
#'
#' @param endpoint_url Public URL to the DwC-A zip file.
#' @param organization_key GBIF publishing organization UUID.
#' @param installation_key GBIF installation UUID (e.g., from IPT).
#' @param title Dataset title.
#' @param description Brief dataset description.
#' @param username GBIF account username.
#' @param password GBIF account password.
#' @param license GBIF license code. Default: "CC_BY_NC_4_0".
#' @param language ISO language code. Default: "eng".
#' @param type Dataset type. Default: "OCCURRENCE".
#'
#' @return Parsed JSON response from GBIF.
#' @export
register_gbif_dataset <- function(
  endpoint_url,
  organization_key,
  installation_key,
  title,
  description,
  username,
  password,
  license = "CC_BY_NC_4_0",
  language = "eng",
  type = "OCCURRENCE"
) {
  payload <- list(
    title = title,
    description = description,
    language = language,
    type = type,
    publishingOrganizationKey = organization_key,
    installationKey = installation_key,
    license = license,
    endpoints = list(list(
      type = "DWC_ARCHIVE",
      url = endpoint_url
    ))
  )

  resp <- httr2::request(
    sprintf("https://api.gbif.org/v1/organization/%s/dataset", organization_key)
  ) |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_body_json(payload) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  httr2::resp_body_json(resp)
}

#' GBIF-Test demo flow (fixed keys and credentials)
#'
#' Runs the GBIF-Test demo recipe: creates a dataset with the documented demo
#' org/install keys and adds your DwC-A URL as the endpoint. No lookups needed.
#' Use only on GBIF-Test with the demo credentials.
#'
#' @param endpoint_url Public URL to the DwC-A zip file.
#' @param title Dataset title.
#' @param description Brief dataset description.
#' @param type Dataset type. Default: "OCCURRENCE".
#' @param license License URL. Default: "http://creativecommons.org/publicdomain/zero/1.0/legalcode".
#' @param language ISO language code. Default: "eng".
#'
#' @return List with `dataset_key`, `registration`, and `endpoint`.
#' @export
register_gbif_dataset_test <- function(
  endpoint_url,
  title = "Example dataset registration",
  description = "Minimal metadata; overwritten after GBIF fetches the archive.",
  type = "OCCURRENCE",
  license = "http://creativecommons.org/publicdomain/zero/1.0/legalcode",
  language = "eng"
) {
  base_url <- "https://api.gbif-test.org/v1"
  user <- "ws_client_demo"
  pass <- "Demo123"
  org_key <- "0a16da09-7719-40de-8d4f-56a15ed52fb6"
  install_key <- "92d76df5-3de1-4c89-be03-7a17abad962a"

  reg_payload <- list(
    publishingOrganizationKey = org_key,
    installationKey = install_key,
    type = type,
    title = title,
    description = description,
    language = language,
    license = license
  )

  reg_resp <- httr2::request(paste0(base_url, "/dataset")) |>
    httr2::req_auth_basic(user, pass) |>
    httr2::req_body_json(reg_payload) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  reg_body <- httr2::resp_body_json(reg_resp)
  dataset_key <- if (is.list(reg_body) && !is.null(reg_body$key)) {
    reg_body$key
  } else if (is.character(reg_body) && length(reg_body) == 1) {
    reg_body
  } else {
    stop(
      "Unexpected registration response: ",
      httr2::resp_body_string(reg_resp),
      call. = FALSE
    )
  }

  endpoint_payload <- list(
    type = "DWC_ARCHIVE",
    url = endpoint_url
  )

  endpoint_resp <- httr2::request(
    sprintf("%s/dataset/%s/endpoint", base_url, dataset_key)
  ) |>
    httr2::req_auth_basic(user, pass) |>
    httr2::req_body_json(endpoint_payload) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  list(
    dataset_key = dataset_key,
    registration = reg_body,
    endpoint = httr2::resp_body_json(endpoint_resp)
  )
  # https://registry.gbif-test.org/dataset/{dataset_key}
}

#' Render ZooGoN MC Survey Report
#'
#' This function renders the Quarto report using the preprocessed survey data.
#' It downloads the latest preprocessed data from SharePoint and renders the
#' report to HTML format, saving it in the working directory.
#'
#' @param output_dir Directory where the rendered report will be saved.
#'   Defaults to the current working directory.
#'
#' @return Invisible NULL. Renders the report to the output directory.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads configuration settings from config.yml
#' 2. Locates Report.qmd inside the installed package (inst/report/)
#' 3. Renders the Quarto report to HTML in the specified output directory
#'
#' @keywords workflow report
#' @export
#'
#' @examples
#' \dontrun{
#' render_report()
#' }
render_report <- function(output_dir = "/home") {
  conf <- read_config()

  report_path <- system.file("report/REPORT.qmd", package = "ZooGoN")

  if (report_path == "") {
    stop(
      "Report.qmd not found in inst/report/. Make sure the package is installed."
    )
  }

  logger::log_info("Rendering Quarto report...", namespace = "ZooGoN")
  quarto::quarto_render(
    input = report_path,
    output_format = "html",
    execute_params = list(
      sharepoint_site_url = conf$storage$sharepoint$credentials$site_url,
      data_prefix = conf$ingestion$surveys$preprocessed$file_prefix,
      automation_bucket = conf$storage$sharepoint$buckets$automation_bucket
    )
  )

  # Quarto saves files next to the .qmd -- copy both to output_dir
  out_file <- "REPORT.html"
  rendered_path <- file.path(dirname(report_path), out_file)
  dest_path <- file.path(
    output_dir,
    paste0("ZooGoN-report-", Sys.Date(), ".html")
  )
  if (file.exists(rendered_path)) {
    file.copy(rendered_path, dest_path, overwrite = TRUE)
    logger::log_success(
      "Report saved as: {dest_path}",
      namespace = "ZooGoN"
    )
  } else {
    logger::log_warn(
      "Expected file not found: {rendered_path}",
      namespace = "ZooGoN"
    )
  }
}


#' Run the full ZooGoN pipeline
#'
#' Runs the full ZooGoN data pipeline in sequence:
#' ingestion -> preprocessing -> report rendering.
#'
#' @return Invisible NULL.
#' @keywords workflow
#' @export
#'
#' @examples
#' \dontrun{
#' run_pipeline()
#' }
run_pipeline <- function() {
  logger::log_info("Starting ZooGoN pipeline...", namespace = "ZooGoN")

  ingest_surveys()
  preprocess_surveys()
  render_report()

  logger::log_success("ZooGoN pipeline complete", namespace = "ZooGoN")
  invisible(NULL)
}
