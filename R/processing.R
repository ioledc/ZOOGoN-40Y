#' Convert legacy LTER-MareChiara zooplankton data to Darwin Core format
#'
#' This function converts preprocessed legacy zooplankton datasets from the
#' LTER-MareChiara station into Darwin Core-compliant format for EMODnet Biology
#' publication and Digital Twin Ocean integration. The function processes
#' standardized parquet files containing taxonomically validated data with
#' WoRMS LSIDs (Life Science Identifiers).
#'
#' @param verbose Logical. Whether to print processing messages. Default is TRUE.
#'
#' @return A list containing Darwin Core formatted tables and metadata:
#' \describe{
#'   \item{event}{Event extension table with sampling event information (eventID,
#'     eventDate, geographic coordinates, sampling protocol)}
#'   \item{occurrence}{Occurrence extension table with species occurrence data
#'     (eventID, occurrenceID, scientificName, scientificNameID, occurrenceStatus)}
#'   \item{emof}{Extended Measurement or Fact (eMoF) table with measurements
#'     (occurrenceID, measurementType, measurementValue, measurementTypeID,
#'     measurementValueID, measurementUnitID) including individual counts, sex,
#'     and life stage information}
#'   \item{raw_data}{Original preprocessed data before Darwin Core conversion}
#'   \item{processing_info}{List with processing metadata (processing_date,
#'     total_events, total_occurrences, total_measurements, date_range, unique_taxa)}
#'   \item{metadata}{Tibble with dataset-level metadata (title, contact,
#'     institution, license, project information)}
#' }
#'
#' @details
#' **Input Data Format:**
#'
#' The function expects preprocessed legacy data in parquet format with the
#' following structure:
#' \itemize{
#'   \item \code{eventID}: Unique sampling event identifier (e.g., "mc_1", "mc_2")
#'   \item \code{eventDate}: Sampling date in Date format (YYYY-MM-DD)
#'   \item \code{scientificName}: Full scientific name with WoRMS validation
#'   \item \code{lsid}: WoRMS Life Science Identifier URN (e.g.,
#'     "urn:lsid:marinespecies.org:taxname:104251")
#'   \item \code{individualCount}: Abundance measurement (ind/m³)
#'   \item \code{lifeStage}: Life stage code ("f" = female, "m" = male, "j" = juvenile,
#'     "fm" = both sexes, "fmj" = all stages)
#' }
#'
#' **Current Implementation:**
#'
#' Currently processes the file \code{McZoo_84-13.parquet} (1984-2013 data).
#' Future versions will support additional legacy files (\code{McZoo_16.parquet},
#' \code{McZoo_17.parquet}, etc.) following the same standardized format.
#'
#' **Darwin Core Conversion:**
#'
#' The function creates three Darwin Core extension tables following OBIS
#' (Ocean Biodiversity Information System) standards:
#'
#' \itemize{
#'   \item **Event Extension**: Core table with sampling event metadata including
#'     temporal (eventDate) and spatial information (decimalLatitude,
#'     decimalLongitude, locality, waterBody, depth ranges, samplingProtocol)
#'   \item **Occurrence Extension**: Links to Event via eventID, contains species
#'     occurrences with scientificName, scientificNameID (WoRMS LSID), and
#'     occurrenceStatus (present/absent based on individualCount > 0)
#'   \item **eMoF Extension**: Extended Measurement or Fact table linked via
#'     occurrenceID, containing quantitative measurements with standardized
#'     vocabulary terms from BODC NERC Vocabulary Server (NVS):
#'     \itemize{
#'       \item Individual counts with NERC P06 units (ind/m³)
#'       \item Sex information (P01/ENTSEX01) with S10 controlled vocabulary
#'       \item Life stage information (P01/LSTAGE01) with S11 controlled vocabulary
#'     }
#' }
#'
#' **Geographic Information:**
#'
#' LTER-MareChiara station coordinates are automatically assigned to all events:
#' \itemize{
#'   \item Latitude: 40.81°N
#'   \item Longitude: 14.25°E
#'   \item Locality: LTER-MareChiara station
#'   \item Water Body: Mediterranean Sea (Gulf of Naples, Tyrrhenian Sea)
#'   \item Depth range: 0-50 meters (vertical tow)
#'   \item Country: Italy (Campania region)
#' }
#'
#' **Data Standards:**
#' \itemize{
#'   \item Taxonomic identifiers: WoRMS LSIDs
#'   \item Measurement vocabulary: BODC NERC Vocabulary Server (NVS)
#'   \item Format: Darwin Core Archive structure
#'   \item Compliance: OBIS schema and EMODnet Biology requirements
#' }
#'
#' @examples
#' \dontrun{
#' # Process legacy data to Darwin Core format
#' dc_data <- raw_to_dc()
#'
#' # Access individual Darwin Core extension tables
#' events <- dc_data$event
#' occurrences <- dc_data$occurrence
#' measurements <- dc_data$emof
#'
#' # View processing summary
#' dc_data$processing_info
#'
#' # Access metadata
#' dc_data$metadata
#'
#' # Silent processing (no console messages)
#' dc_data <- raw_to_dc(verbose = FALSE)
#' }
#'
#' @seealso
#' \itemize{
#'   \item Darwin Core standard: \url{https://dwc.tdwg.org/}
#'   \item OBIS Manual for Darwin Core: \url{https://manual.obis.org/darwin_core.html}
#'   \item EMODnet Biology: \url{https://www.emodnet-biology.eu/}
#'   \item LTER-MareChiara: \url{https://deims.org/0b87459a-da3c-45af-a3e1-cb1508519411}
#'   \item WoRMS (World Register of Marine Species): \url{https://www.marinespecies.org/}
#'   \item BODC NERC Vocabulary Server: \url{https://vocab.nerc.ac.uk/}
#' }
#'
#' @export
raw_to_dc <- function(
  verbose = TRUE
) {
  conf <- read_config()
  if (verbose) {
    message("Starting LTER-MareChiara data processing...")
  }

  legacy_84_15 <-
    download_sharepoint_file(
      prefix = "McZoo_84-15.parquet",
      options = conf$storage$sharepoint$credentials,
      bucket = conf$storage$sharepoint$buckets$hot_bucket,
      filename = TRUE
    )

  legacy_16_20 <-
    download_sharepoint_file(
      prefix = "McZoo_16-20.parquet",
      options = conf$storage$sharepoint$credentials,
      bucket = conf$storage$sharepoint$buckets$hot_bucket,
      filename = TRUE
    )

  legacy_21_24 <-
    download_sharepoint_file(
      prefix = "McZoo_21-24.parquet",
      options = conf$storage$sharepoint$credentials,
      bucket = conf$storage$sharepoint$buckets$hot_bucket,
      filename = TRUE
    )

  legacy_84_24 <-
    dplyr::bind_rows(legacy_84_15, legacy_16_20, legacy_21_24)

  # Create Darwin Core Event extension
  if (verbose) {
    message("Creating Event extension table...")
  }

  event_ext <- legacy_84_24 |>
    dplyr::select(dplyr::all_of(c("eventID", "eventDate"))) |>
    dplyr::arrange(.data$eventDate) |>
    dplyr::distinct() |>
    dplyr::mutate(
      eventDate = as.character(.data$eventDate),
      eventType = "Site Visit",
      decimalLatitude = 40.81,
      decimalLongitude = 14.25,
      geodeticDatum = "WGS84",
      continent = "Europe",
      countryCode = "IT",
      institutionCode = "SZN",
      datasetName = "ZooGoN", # to do: verify dataset name
      locality = "Gulf of Naples",
      stateProvince = "Campania",
      waterBody = "Tyrrhenian Sea",
      maximumDepthInMeters = 50,
      minimumDepthInMeters = 0,
      samplingProtocol = "Vertical tow 0-50m depth"
    )

  # Create Darwin Core Occurrence extension
  if (verbose) {
    message("Creating Occurrence extension table...")
  }

  full_table <- legacy_84_24 |>
    dplyr::mutate(
      occurrenceStatus = dplyr::if_else(
        .data$individualCount > 0,
        "present",
        "absent"
      ),
      occurrenceID = paste0(.data$eventID, "-occ", dplyr::row_number())
    ) |>
    dplyr::relocate("occurrenceID", .after = "eventID") |>
    dplyr::distinct()

  occurrence_table <- full_table |>
    dplyr::mutate(basisOfRecord = "MachineObservation") |>
    dplyr::select(
      "eventID",
      "occurrenceID",
      "basisOfRecord",
      "scientificName",
      scientificNameID = "lsid",
      "occurrenceStatus"
    )

  # Create Darwin Core eMoF extension
  if (verbose) {
    message("Creating eMoF extension table...")
  }

  emof_occ <- build_emof_occurrence(data = full_table)
  emof_events <- build_emof_events(data = full_table)
  emof_table <- dplyr::bind_rows(emof_occ, emof_events)

  # Prepare output
  processing_info <- list(
    processing_date = Sys.time(),
    total_events = nrow(event_ext),
    total_occurrences = nrow(occurrence_table),
    total_measurements = nrow(emof_table),
    date_range = range(event_ext$eventDate, na.rm = TRUE),
    unique_taxa = length(unique(occurrence_table$scientificName))
  )

  metadata_df <- tibble::tibble(
    dataset_title = "40 years of Zooplankton data at LTER MareChiara site (Gulf of Naples, Mediterranean Sea) 1984-2024", # to do: depends on the data we are going to publish
    contact = "Dr. Iole Di Capua (iole.dicapua@szn.it)",
    institution = "Stazione Zoologica Anton Dohrn",
    license = "CC-BY-NC",
    project = "DTO-BioFlow FSTP Grant",
    processing_date = as.character(processing_info$processing_date),
    total_events = processing_info$total_events,
    total_occurrences = processing_info$total_occurrences,
    date_range_start = as.character(processing_info$date_range[1]),
    date_range_end = as.character(processing_info$date_range[2])
  )

  darwin_core_data <- list(
    event = event_ext,
    occurrence = occurrence_table,
    emof = emof_table,
    raw_data = legacy_84_24,
    processing_info = processing_info,
    metadata = metadata_df
  )

  if (verbose) {
    message("Processing completed successfully!")
    message("Total events: ", processing_info$total_events)
    message("Total occurrences: ", processing_info$total_occurrences)
    message("Total measurements: ", processing_info$total_measurements)
    message("Unique taxa: ", processing_info$unique_taxa)
    message(
      "Date range: ",
      processing_info$date_range[1],
      " to ",
      processing_info$date_range[2]
    )
  }

  return(darwin_core_data)
}


#' Build occurrence-level eMoF table
#'
#' Converts occurrence attributes (abundance, sex, life stage) to
#' Darwin Core eMoF format. Recodes legacy life stage/sex codes to
#' controlled vocabularies (NERC P01/S10/S11) and assigns units for
#' abundance (ind/m³).
#'
#' @param data A data frame containing at least `eventID`, `eventDate`,
#'   `occurrenceID`, `individualCount`, and `lifeStage`.
#'
#' @return A tibble in eMoF format with one row per occurrence-level
#'   measurement and the columns:
#'   `eventID`, `occurrenceID`, `eventDate`,
#'   `measurementType`, `measurementTypeID`,
#'   `measurementValue`, `measurementValueID`,
#'   `measurementUnit`, `measurementUnitID`.
#'
#' @export
build_emof_occurrence <- function(data = NULL) {
  sex_map <- tibble::tribble(
    ~code , ~value        , ~value_id                                              ,
    "f"   , "female"      , "http://vocab.nerc.ac.uk/collection/S10/current/S102/" ,
    "m"   , "male"        , "http://vocab.nerc.ac.uk/collection/S10/current/S103/" ,
    "fm"  , "male+female" , "http://vocab.nerc.ac.uk/collection/S10/current/S108/"
  )

  stage_map <- tibble::tribble(
    ~code , ~value           , ~value_id                                               ,
    "j"   , "juvenile"       , "http://vocab.nerc.ac.uk/collection/S11/current/S1127/" ,
    "fmj" , "juvenile+adult" , "http://vocab.nerc.ac.uk/collection/S11/current/S1145/" ,
    "lar" , "larva"          , "http://vocab.nerc.ac.uk/collection/S11/current/S1128/" ,
    "egg" , "egg"            , "http://vocab.nerc.ac.uk/collection/S11/current/S1122/" ,
    "nau" , "nauplius"       , "http://vocab.nerc.ac.uk/collection/S11/current/S1130/"
  )

  type_ids <- c(
    sex = "http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/",
    lifeStage = "http://vocab.nerc.ac.uk/collection/P01/current/LSTAGE01/",
    individualCount = "http://vocab.nerc.ac.uk/collection/P01/current/ZU00M00D/"
  )

  data |>
    dplyr::select(-"scientificName", -"occurrenceStatus", -"lsid") |>
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across(
      -c("eventID", "eventDate", "occurrenceID"),
      as.character
    )) |>
    tidyr::pivot_longer(
      cols = -c("eventID", "eventDate", "occurrenceID"),
      names_to = "measurementType",
      values_to = "raw"
    ) |>
    dplyr::mutate(raw = as.character(.data$raw)) |>
    dplyr::left_join(sex_map, by = c("raw" = "code")) |>
    dplyr::left_join(
      stage_map,
      by = c("raw" = "code"),
      suffix = c("_sex", "_stage")
    ) |>
    dplyr::mutate(
      measurementValue = dplyr::coalesce(
        .data$value_sex,
        .data$value_stage,
        .data$raw
      ),
      measurementValueID = dplyr::coalesce(
        .data$value_id_sex,
        .data$value_id_stage
      ),
      measurementType = dplyr::case_when(
        !is.na(.data$value_sex) ~ "sex",
        !is.na(.data$value_stage) ~ "lifeStage",
        TRUE ~ .data$measurementType
      ),
      measurementValue = dplyr::case_when(
        .data$measurementType %in%
          c("sex", "lifeStage") &
          is.na(.data$measurementValue) ~ "not specified",
        TRUE ~ .data$measurementValue
      ),
      measurementValueID = dplyr::case_when(
        !is.na(.data$value_id_sex) ~ .data$value_id_sex,
        !is.na(.data$value_id_stage) ~ .data$value_id_stage,
        .data$measurementValue == "not specified" &
          .data$measurementType == "sex" ~
          "http://vocab.nerc.ac.uk/collection/S10/current/S104/",
        .data$measurementValue == "not specified" &
          .data$measurementType == "lifeStage" ~
          "https://vocab.nerc.ac.uk/collection/S11/current/S1131/",
        TRUE ~ NA_character_
      ),
      measurementTypeID = unname(type_ids[.data$measurementType]),
      measurementUnit = dplyr::if_else(
        .data$measurementType == "individualCount",
        "Number per cubic metre",
        NA_character_
      ),
      measurementUnitID = dplyr::if_else(
        .data$measurementType == "individualCount",
        "http://vocab.nerc.ac.uk/collection/P06/current/UPMM/",
        NA_character_
      )
    ) |>
    dplyr::select(
      "eventID",
      "occurrenceID",
      "eventDate",
      "measurementType",
      "measurementTypeID",
      "measurementValue",
      "measurementValueID",
      "measurementUnit",
      "measurementUnitID"
    ) |>
    dplyr::mutate(eventDate = as.character(.data$eventDate))
}

#' Build event-level eMoF sampling metadata
#'
#' Creates one set of sampling metadata per event, including
#' sampling protocol, instrument type, net mouth area and diameter.
#' Instrument and geometry depend on sampling period (pre/post 2016-02-18).
#'
#' @param data A data frame containing at least `eventID` and `eventDate`.
#'
#' @return A tibble in eMoF format with event-level measurements
#'   (`occurrenceID = NA`) and controlled NERC vocabulary identifiers
#'   for protocol (P01/SAMPPROT), instrument (P01/NMSPINST + L22),
#'   mouth area (P01/MTHAREA1, m²) and mouth diameter (P01/DSAMPA01, m).
#'
#' @export
build_emof_events <- function(data = NULL) {
  events <- data |>
    dplyr::distinct(.data$eventID, .data$eventDate) |>
    dplyr::mutate(
      period = ifelse(.data$eventDate < as.Date("2016-02-18"), "old", "new"),
      eventDate = as.character(.data$eventDate)
    )

  dplyr::bind_rows(
    # protocol
    events |>
      dplyr::transmute(
        .data$eventID,
        .data$eventDate,
        occurrenceID = NA_character_,
        measurementType = "samplingProtocol",
        measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/SAMPPROT/",
        measurementValue = "Vertical zooplankton net tow from 50 m depth to surface",
        measurementValueID = NA_character_,
        measurementUnit = NA_character_,
        measurementUnitID = NA_character_
      ),

    # instrument
    events |>
      dplyr::transmute(
        .data$eventID,
        .data$eventDate,
        occurrenceID = NA_character_,
        measurementType = "samplingInstrument",
        measurementTypeID = "https://vocab.nerc.ac.uk/collection/P01/current/NMSPINST/",
        measurementValue = dplyr::if_else(
          .data$period == "old",
          "Indian Ocean net",
          "WP2 plankton net"
        ),
        measurementValueID = dplyr::if_else(
          .data$period == "old",
          "https://vocab.nerc.ac.uk/collection/L22/current/TOOL0979/",
          "https://vocab.nerc.ac.uk/collection/L22/current/NETT0075/"
        ),
        measurementUnit = NA_character_,
        measurementUnitID = NA_character_
      ),

    # mouth area
    events |>
      dplyr::transmute(
        .data$eventID,
        .data$eventDate,
        occurrenceID = NA_character_,
        measurementType = "mouthArea",
        measurementTypeID = "https://vocab.nerc.ac.uk/collection/P01/current/MTHAREA1/",
        measurementValue = dplyr::if_else(.data$period == "old", "1", "0.25"),
        measurementValueID = NA_character_,
        measurementUnit = "m2",
        measurementUnitID = "https://vocab.nerc.ac.uk/collection/P06/current/UMSQ/"
      ),

    # mouth diameter
    events |>
      dplyr::transmute(
        .data$eventID,
        .data$eventDate,
        occurrenceID = NA_character_,
        measurementType = "mouthDiameter",
        measurementTypeID = "https://vocab.nerc.ac.uk/collection/P01/current/DSAMPA01/",
        measurementValue = dplyr::if_else(
          .data$period == "old",
          "1.13",
          "0.57"
        ),
        measurementValueID = NA_character_,
        measurementUnit = "m",
        measurementUnitID = "https://vocab.nerc.ac.uk/collection/P06/current/ULAA/"
      )
  )
}
