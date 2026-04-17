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

  fix_meta_xml(zip_file)

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
        "Zooplankton data from LTER MareChiara site in the Gulf of Naples, Mediterranean Sea, (",
        paste0(
          min(lubridate::month(event_df$eventDate, label = TRUE)),
          " ",
          min(lubridate::year(event_df$eventDate))
        ),
        "-",
        paste0(
          max(lubridate::month(event_df$eventDate, label = TRUE)),
          " ",
          max(lubridate::year(event_df$eventDate))
        ),
        ")"
      ),
      abstract = list(
        para = paste0(
          "This dataset contains zooplankton community data collected at the ",
          "Long-Term Ecological Research (LTER) station MareChiara, located in ",
          "the Gulf of Naples, Mediterranean Sea (40.81\u00b0N, 14.25\u00b0E), ",
          "operated by the Stazione Zoologica Anton Dohrn (SZN). ",
          "Samples were collected through vertical net tows from 50 m depth to ",
          "the surface using an Indian Ocean net (pre-2016) and a WP2 plankton ",
          "net (from 2016 onwards). ",
          "Zooplankton abundance is expressed as individuals per cubic metre. ",
          "The dataset covers the period ",
          min(lubridate::year(event_df$eventDate)),
          "\u2013",
          max(lubridate::year(event_df$eventDate)),
          " and includes taxonomic identifications with WoRMS LSIDs, ",
          "life-stage and sex annotations, and sampling instrument metadata ",
          "following BODC NERC Vocabulary Server standards. ",
          "Data are structured according to the Darwin Core Event core with ",
          "Occurrence and Extended Measurement or Fact (eMoF) extensions, ",
          "following OBIS data standards."
        )
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

#' Patch a Darwin Core Archive's meta.xml for OBIS compatibility
#'
#' Post-processes the meta.xml inside a DwC-A zip to fix two issues that
#' LivingNorwayR does not handle automatically:
#' \enumerate{
#'   \item Corrects the eMoF \code{rowType} to the OBIS URI
#'     (\code{http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact})
#'     if the legacy TDWG URI is found.
#'   \item Adds \code{<field>} mappings for columns present in the CSV files
#'     but absent from meta.xml -- specifically \code{eventType} (Event core)
#'     and \code{eventDate} (eMoF extension), which LivingNorwayR's term
#'     database does not recognise.
#' }
#' The zip is modified in place; a backup is removed on success.
#'
#' @param zip_file Path to the DwC-A zip file.
#' @return Invisibly returns \code{zip_file}.
#' @keywords internal
fix_meta_xml <- function(zip_file) {
  # Column names to DwC/OBIS term IRIs not auto-mapped by LivingNorwayR.
  # eventType -- newer DwC term absent from LivingNorwayR's event member list.
  # occurrenceID / eventDate / *ID -- not mapped by initializeGBIFMeasurementOrFact
  #   (basic MeasurementOrFact skips OBIS extension terms and eventDate).
  extra_terms <- list(
    eventType          = "http://rs.tdwg.org/dwc/terms/eventType",
    occurrenceID       = "http://rs.tdwg.org/dwc/terms/occurrenceID",
    eventDate          = "http://rs.tdwg.org/dwc/terms/eventDate",
    measurementTypeID  = "http://rs.iobis.org/obis/terms/measurementTypeID",
    measurementValueID = "http://rs.iobis.org/obis/terms/measurementValueID",
    measurementUnitID  = "http://rs.iobis.org/obis/terms/measurementUnitID"
  )

  tmp_dir <- tempfile("dwca_fix_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(zip_file, exdir = tmp_dir)
  meta_path <- file.path(tmp_dir, "meta.xml")

  if (!file.exists(meta_path)) {
    logger::log_warn(
      "meta.xml not found in archive -- skipping meta.xml fix",
      namespace = "ZooGoN"
    )
    return(invisible(zip_file))
  }

  doc <- xml2::read_xml(meta_path)

  # Collect <core> and <extension> nodes — use local-name() because the
  # document has a default namespace that bare XPath names don't match
  section_nodes <- c(
    xml2::xml_find_all(doc, "//*[local-name()='core']"),
    xml2::xml_find_all(doc, "//*[local-name()='extension']")
  )

  for (node in section_nodes) {
    # 1. Fix eMoF rowType
    row_type <- xml2::xml_attr(node, "rowType")
    if (identical(row_type, "http://rs.tdwg.org/dwc/terms/MeasurementOrFact")) {
      xml2::xml_set_attr(
        node,
        "rowType",
        "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"
      )
      logger::log_info(
        "meta.xml: corrected eMoF rowType to ExtendedMeasurementOrFact",
        namespace = "ZooGoN"
      )
    }

    # 2. Add field mappings for unmapped columns
    loc_node <- xml2::xml_find_first(
      node, ".//*[local-name()='files']/*[local-name()='location']"
    )
    if (inherits(loc_node, "xml_missing")) {
      next
    }

    csv_path <- file.path(tmp_dir, xml2::xml_text(loc_node))
    if (!file.exists(csv_path)) {
      next
    }

    sep     <- xml2::xml_attr(node, "fieldsTerminatedBy")
    sep     <- if (is.na(sep) || sep == "\\t") "\t" else sep
    headers <- names(utils::read.csv(csv_path, nrow = 0, sep = sep, check.names = FALSE))
    field_nodes  <- xml2::xml_find_all(node, "*[local-name()='field']")
    mapped_idx   <- as.integer(xml2::xml_attr(field_nodes, "index"))
    mapped_terms <- xml2::xml_attr(field_nodes, "term")

    for (col_name in names(extra_terms)) {
      term_iri <- extra_terms[[col_name]]
      col_idx <- which(headers == col_name) - 1L # 0-based

      if (length(col_idx) != 1) {
        next
      }
      if (col_idx %in% mapped_idx || term_iri %in% mapped_terms) {
        next
      }

      new_field <- xml2::read_xml(
        sprintf('<field index="%d" term="%s"/>', col_idx, term_iri)
      )
      xml2::xml_add_child(node, new_field)
      logger::log_info(
        "meta.xml: added missing field '{col_name}' (index {col_idx})",
        namespace = "ZooGoN"
      )
    }
  }

  xml2::write_xml(doc, meta_path)

  # Rezip in place — resolve to absolute path before setwd so utils::zip
  # writes outside tmp_dir (otherwise the file gets deleted by on.exit cleanup)
  zip_abs <- normalizePath(zip_file, mustWork = FALSE)
  bak      <- paste0(zip_abs, ".bak")
  file.rename(zip_abs, bak)
  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  all_files <- list.files(tmp_dir, recursive = TRUE, full.names = FALSE)
  utils::zip(zipfile = zip_abs, files = all_files)
  setwd(old_wd)
  file.remove(bak)

  invisible(zip_file)
}

#' Register a hosted archive on GBIF
#'
#' Create a dataset entry on GBIF and point it to your public DwC-A zip.
#' Credentials and keys are read from \code{inst/config.yml} (\code{gbif$production}).
#' Reads \code{GBIF_ENDPOINT_URL}, \code{GBIF_USERNAME}, \code{GBIF_PASSWORD},
#' \code{GBIF_ORG_KEY}, and \code{GBIF_INSTALL_KEY} from the environment.
#'
#' @param title Dataset title.
#' @param description Brief dataset description.
#' @param license GBIF license code. Default: "CC_BY_NC_4_0".
#' @param language ISO language code. Default: "eng".
#' @param type Dataset type. Default: "OCCURRENCE".
#'
#' @return List with `dataset_key`, `registration`, and `endpoint`.
#' @export
register_gbif_dataset <- function(
  title,
  description,
  license = "CC_BY_NC_4_0",
  language = "eng",
  type = "OCCURRENCE"
) {
  conf <- read_config()$gbif$production

  reg_payload <- list(
    publishingOrganizationKey = conf$organization_key,
    installationKey = conf$installation_key,
    type = type,
    title = title,
    description = description,
    language = language,
    license = license
  )

  reg_resp <- httr2::request(paste0(conf$base_url, "/dataset")) |>
    httr2::req_auth_basic(conf$username, conf$password) |>
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

  endpoint_resp <- httr2::request(
    sprintf("%s/dataset/%s/endpoint", conf$base_url, dataset_key)
  ) |>
    httr2::req_auth_basic(conf$username, conf$password) |>
    httr2::req_body_json(list(type = "DWC_ARCHIVE", url = conf$endpoint_url)) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  list(
    dataset_key = dataset_key,
    registration = reg_body,
    endpoint = httr2::resp_body_json(endpoint_resp)
  )
}

#' GBIF-Test registration flow
#'
#' Registers a dataset on the GBIF-Test sandbox using credentials and keys from
#' \code{inst/config.yml} (\code{gbif$test}). Reads \code{GBIF_TEST_ENDPOINT_URL},
#' \code{GBIF_TEST_USER}, \code{GBIF_TEST_PASSWORD}, \code{GBIF_TEST_ORG_KEY},
#' and \code{GBIF_TEST_INSTALL_KEY} from the environment.
#'
#' @param title Dataset title.
#' @param description Brief dataset description.
#' @param type Dataset type. Default: "OCCURRENCE".
#' @param license License URL. Default: "http://creativecommons.org/publicdomain/zero/1.0/legalcode".
#' @param language ISO language code. Default: "eng".
#'
#' @return List with `dataset_key`, `registration`, and `endpoint`.
#' @export
register_gbif_dataset_test <- function(
  title = "Example dataset registration",
  description = "Minimal metadata; overwritten after GBIF fetches the archive.",
  type = "OCCURRENCE",
  license = "http://creativecommons.org/publicdomain/zero/1.0/legalcode",
  language = "eng"
) {
  conf <- read_config()$gbif$test

  reg_payload <- list(
    publishingOrganizationKey = conf$organization_key,
    installationKey = conf$installation_key,
    type = type,
    title = title,
    description = description,
    language = language,
    license = license
  )

  reg_resp <- httr2::request(paste0(conf$base_url, "/dataset")) |>
    httr2::req_auth_basic(conf$username, conf$password) |>
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

  endpoint_resp <- httr2::request(
    sprintf("%s/dataset/%s/endpoint", conf$base_url, dataset_key)
  ) |>
    httr2::req_auth_basic(conf$username, conf$password) |>
    httr2::req_body_json(list(type = "DWC_ARCHIVE", url = conf$endpoint_url)) |>
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
#' This function renders the Quarto monitoring report from the installed
#' package, saves the HTML output with a versioned filename, and uploads it to
#' the SharePoint reports bucket. The report reads the merged tidy dataset
#' directly from SharePoint via \code{read_config()}.
#'
#' @param output_dir Local directory where the rendered HTML is saved before
#'   upload. Defaults to \code{"/home"}.
#'
#' @return Invisible NULL.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads configuration from \code{inst/config.yml} via \code{read_config()}.
#' 2. Locates \code{REPORT_interact.qmd} inside the installed package.
#' 3. Renders the Quarto report to HTML.
#' 4. Copies the output to \code{output_dir} with a versioned filename
#'    produced by \code{add_version()} (timestamp + git SHA).
#' 5. Uploads the HTML to the SharePoint \code{reports} bucket.
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

  report_path <- system.file("report/REPORT_interact.qmd", package = "ZooGoN")

  if (report_path == "") {
    stop(
      "Report.qmd not found in inst/report/. Make sure the package is installed."
    )
  }

  logger::log_info("Rendering Quarto report...", namespace = "ZooGoN")
  quarto::quarto_render(
    input = report_path,
    output_format = "html"
  )

  # Quarto saves the HTML next to the .qmd; move to output_dir with a versioned name
  rendered_path <- file.path(dirname(report_path), "REPORT_interact.html")
  prefix <- conf$ingestion$reports$file_prefix
  versioned_name <- add_version(prefix, "html")
  dest_path <- file.path(output_dir, versioned_name)

  if (!file.exists(rendered_path)) {
    logger::log_warn(
      "Expected rendered file not found: {rendered_path}",
      namespace = "ZooGoN"
    )
    return(invisible(NULL))
  }

  file.copy(rendered_path, dest_path, overwrite = TRUE)
  logger::log_success("Report saved as: {dest_path}", namespace = "ZooGoN")

  upload_sharepoint_file(
    file_path = dest_path,
    remote_filename = prefix,
    options = conf$storage$sharepoint$credentials,
    bucket = conf$storage$sharepoint$buckets$reports_bucket,
    format = "html"
  )
  logger::log_success(
    "Report uploaded to SharePoint reports bucket.",
    namespace = "ZooGoN"
  )

  invisible(NULL)
}


#' Run the ZooGoN survey-to-report pipeline
#'
#' Runs the survey ingestion and report rendering steps in sequence:
#' ingestion -> preprocessing -> report rendering. This covers the
#' reporting branch of the pipeline. To also build the Darwin Core
#' Archive, call \code{format_to_tidy()} followed by
#' \code{format_to_DC_archive()}.
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
