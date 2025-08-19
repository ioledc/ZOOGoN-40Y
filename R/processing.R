#' Process LTER-MareChiara zooplankton data to Darwin Core format
#'
#' This function implements the complete data processing workflow for the
#' LTER-MareChiara zooplankton dataset, transforming raw Excel data into
#' Darwin Core-compliant format with WoRMS taxonomic validation. The function
#' handles 40 years of Gulf of Naples zooplankton monitoring data (1984-2024)
#' and prepares it for EMODnet Biology publication and Digital Twin Ocean integration.
#'
#' The processing workflow includes:
#' - Loading and restructuring raw abundance data
#' - Applying ZooGoN taxonomic standardization
#' - WoRMS taxonomic validation and matching
#' - Darwin Core format conversion (Event, Occurrence, eMoF tables)
#' - Geographic coordinate assignment for LTER-MareChiara station
#' - Quality control and data validation
#'
#' @param zoo_data_path Character string. Path to the main zooplankton data Excel file.
#'   Default is "data/lter_zoo_84_13.xlsx".
#' @param ids_data_path Character string. Path to the sample IDs Excel file.
#'   Default is "data/ids.xlsx".
#' @param worms_validation Logical. Whether to perform WoRMS taxonomic validation.
#'   Default is TRUE. Set to FALSE to skip WoRMS matching for faster processing.
#' @param output_format Character string. Output format for the processed data.
#'   Options are "list" (default) for a list of Darwin Core tables, or "csv"
#'   to write CSV files to a specified directory.
#' @param output_dir Character string. Directory to write CSV files when
#'   output_format = "csv". Default is "processed_data/darwin_core".
#' @param verbose Logical. Whether to print processing messages. Default is TRUE.
#'
#' @return A list containing Darwin Core formatted tables:
#' \describe{
#'   \item{event}{Event extension table with sampling event information}
#'   \item{occurrence}{Occurrence extension table with species occurrence data}
#'   \item{emof}{Extended Measurement or Fact (eMoF) table with quantitative data}
#'   \item{raw_data}{Processed raw data before Darwin Core conversion}
#'   \item{processing_info}{Metadata about the processing workflow}
#' }
#'
#' @details
#' **Data Sources:**
#' The function processes two main Excel files:
#' - Main zooplankton data: Species abundance matrix with sampling dates as columns
#' - Sample IDs: Mapping between dates and standardized sample identifiers
#'
#' **Taxonomic Processing:**
#' - Applies `extract_genus_species()` for name standardization
#' - Splits genus-species names into separate genus and species columns
#' - Attempts WoRMS validation using `worrms::wm_records_taxamatch()`
#' - Handles taxonomic matching errors gracefully with NA values
#'
#' **Darwin Core Compliance:**
#' - **Event Extension**: Sampling events with temporal and spatial information
#' - **Occurrence Extension**: Species occurrences with presence/absence status
#' - **eMoF Extension**: Quantitative measurements (abundance as ind/m³)
#'
#' **Geographic Information:**
#' LTER-MareChiara station coordinates are automatically assigned:
#' - Latitude: 40.81°N
#' - Longitude: 14.25°E
#' - Location: Gulf of Naples, Tyrrhenian Sea, Mediterranean
#'
#' @examples
#' \dontrun{
#' # Basic processing with WoRMS validation
#' processed_data <- process_lter_data()
#'
#' # Access individual Darwin Core tables
#' events <- processed_data$event
#' occurrences <- processed_data$occurrence
#' measurements <- processed_data$emof
#'
#' # Process without WoRMS validation for faster execution
#' quick_data <- process_lter_data(worms_validation = FALSE)
#'
#' # Export directly to CSV files
#' process_lter_data(
#'   output_format = "csv",
#'   output_dir = "my_output_folder"
#' )
#'
#' # Process with custom file paths
#' custom_data <- process_lter_data(
#'   zoo_data_path = "custom/path/zooplankton.xlsx",
#'   ids_data_path = "custom/path/sample_ids.xlsx"
#' )
#' }
#'
#' @seealso
#' - [extract_genus_species()] for taxonomic name standardization
#' - [worrms::wm_records_taxamatch()] for WoRMS taxonomic validation
#' - Darwin Core standard: \url{https://dwc.tdwg.org/}
#' - EMODnet Biology: \url{https://www.emodnet-biology.eu/}
#' - LTER-MareChiara: \url{https://deims.org/0b87459a-da3c-45af-a3e1-cb1508519411}
#'
#' @export
process_lter_data <- function(
  zoo_data_path = "data/lter_zoo_84_13.xlsx",
  ids_data_path = "data/ids.xlsx",
  worms_validation = TRUE,
  output_format = "list",
  output_dir = "processed_data/darwin_core",
  verbose = TRUE
) {
  if (verbose) {
    message("Starting LTER-MareChiara data processing...")
  }

  # Validate input parameters
  if (!output_format %in% c("list", "csv")) {
    stop("output_format must be either 'list' or 'csv'")
  }

  if (!file.exists(zoo_data_path)) {
    stop("Zooplankton data file not found: ", zoo_data_path)
  }

  if (!file.exists(ids_data_path)) {
    stop("Sample IDs file not found: ", ids_data_path)
  }

  # Load and prepare raw data
  if (verbose) {
    message("Loading raw data files...")
  }

  dat <- readxl::read_xlsx(
    zoo_data_path,
    .name_repair = "minimal"
  ) |>
    dplyr::mutate(dat_id = seq_len(dplyr::n()))

  ids <- readxl::read_xlsx(
    ids_data_path,
    .name_repair = "minimal"
  ) |>
    dplyr::mutate(
      date = lubridate::as_date(as.numeric(date), origin = "1899-12-30"),
      sample_id = janitor::make_clean_names(sample_id)
    )

  # Extract date columns and taxonomic data
  if (verbose) {
    message("Restructuring taxonomic and temporal data...")
  }

  dates <- dat |>
    dplyr::select(-c(1:10), "dat_id")

  raw_taxa <- dat |>
    janitor::clean_names() |>
    dplyr::select("phylum_o_subphylum":"stage", "dat_id") |>
    dplyr::mutate(
      genus_species = extract_genus_species(.data$taxa)$genus_species,
    ) |>
    tidyr::separate(
      genus_species,
      into = c("genus", "species"),
      sep = " ",
      remove = FALSE,
      fill = "right"
    ) |>
    dplyr::mutate(
      taxa_worms = dplyr::case_when(
        !is.na(.data$genus) & !is.na(.data$species) ~
          paste(.data$genus, .data$species),
        !is.na(.data$genus) ~ .data$genus,
        TRUE ~ .data$taxa
      )
    )

  # WoRMS taxonomic validation (optional)
  if (worms_validation) {
    if (verbose) {
      message("Performing WoRMS taxonomic validation...")
    }

    if (!requireNamespace("worrms", quietly = TRUE)) {
      warning("worrms package not available. Skipping WoRMS validation.")
      worms_validation <- FALSE
    }
  }

  if (worms_validation) {
    worrms_matched <- raw_taxa |>
      dplyr::select("dat_id", "taxa_worms") |>
      dplyr::rowwise() |>
      dplyr::mutate(
        worrms_match = tryCatch(
          {
            worrms::wm_records_taxamatch(name = taxa_worms, verbose = FALSE)
          },
          error = function(e) {
            if (verbose) {
              message("WoRMS match failed for: ", taxa_worms)
            }
            NA
          }
        )
      ) |>
      dplyr::select(-"taxa_worms")
  } else {
    # Create empty WoRMS match column
    worrms_matched <- raw_taxa |>
      dplyr::select("dat_id") |>
      dplyr::mutate(worrms_match = NA)
  }

  # Combine taxonomic and temporal data
  if (verbose) {
    message("Creating Darwin Core formatted dataset...")
  }

  tidy_data <- raw_taxa |>
    dplyr::select(-"taxa_worms") |>
    dplyr::left_join(worrms_matched, by = "dat_id") |>
    dplyr::select("dat_id", "taxa", "worrms_match", "stage") |>
    dplyr::left_join(dates, by = "dat_id") |>
    tidyr::pivot_longer(
      -c("dat_id", "taxa", "worrms_match", "stage"),
      names_to = "date",
      values_to = "ind_m3"
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      date = lubridate::as_date(as.numeric(date), origin = "1899-12-30")
    ) |>
    dplyr::select(-"dat_id") |>
    dplyr::left_join(ids, by = "date") |>
    dplyr::relocate(sample_id, .before = "taxa") |>
    dplyr::relocate(stage, .after = "ind_m3") |>
    dplyr::relocate(date, .after = "sample_id") |>
    dplyr::arrange(sample_id) |>
    dplyr::rename(
      eventID = sample_id,
      eventDate = date,
      scientificName = taxa,
      individualCount = ind_m3,
      lifeStage = stage
    ) |>
    dplyr::distinct()

  # Create Darwin Core Event extension
  if (verbose) {
    message("Creating Event extension table...")
  }

  event_ext <- tidy_data |>
    dplyr::select(eventID, eventDate) |>
    dplyr::distinct() |>
    dplyr::mutate(
      decimalLatitude = 40.81,
      decimalLongitude = 14.25,
      locality = "LTER-MareChiara station",
      country = "Italy",
      stateProvince = "Campania",
      waterBody = "Mediterranean Sea",
      maximumDepthInMeters = 50,
      minimumDepthInMeters = 0,
      samplingProtocol = "Vertical tow 0-50m depth",
      sampleSizeValue = 1,
      sampleSizeUnit = "sample"
    )

  # Create Darwin Core Occurrence extension
  if (verbose) {
    message("Creating Occurrence extension table...")
  }

  full_table <- tidy_data |>
    dplyr::mutate(
      occurrenceStatus = dplyr::if_else(
        individualCount > 0,
        "present",
        "absent"
      ),
      occurrenceID = paste0(eventID, "-occ", dplyr::row_number())
    ) |>
    dplyr::relocate(occurrenceID, .after = "eventDate") |>
    dplyr::distinct()

  occurrence_table <- full_table |>
    dplyr::select(
      eventID,
      occurrenceID,
      scientificName,
      worrms_match,
      lifeStage,
      occurrenceStatus
    )

  # Create Darwin Core eMoF extension
  if (verbose) {
    message("Creating eMoF extension table...")
  }

  emof_table <- full_table |>
    dplyr::select(-c(scientificName, occurrenceStatus, worrms_match)) |>
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(
      cols = -c(eventID, eventDate, occurrenceID),
      names_to = "measurementType",
      values_to = "measurementValue"
    ) |>
    dplyr::filter(!is.na(measurementValue), measurementValue != "") |>
    dplyr::mutate(
      measurementUnit = dplyr::case_when(
        measurementType == "individualCount" ~ "individuals per cubic meter",
        measurementType == "lifeStage" ~ "categorical",
        TRUE ~ "text"
      ),
      measurementRemarks = dplyr::case_when(
        measurementType == "individualCount" ~
          "Abundance count from microscopic analysis",
        measurementType == "lifeStage" ~ "Development stage classification",
        TRUE ~ NA_character_
      )
    )

  # Prepare output
  processing_info <- list(
    processing_date = Sys.time(),
    zoo_data_file = zoo_data_path,
    ids_data_file = ids_data_path,
    worms_validation_performed = worms_validation,
    total_events = nrow(event_ext),
    total_occurrences = nrow(occurrence_table),
    total_measurements = nrow(emof_table),
    date_range = range(event_ext$eventDate, na.rm = TRUE),
    unique_taxa = length(unique(occurrence_table$scientificName))
  )

  darwin_core_data <- list(
    event = event_ext,
    occurrence = occurrence_table,
    emof = emof_table,
    raw_data = tidy_data,
    processing_info = processing_info
  )

  # Export to CSV if requested
  if (output_format == "csv") {
    if (verbose) {
      message("Exporting Darwin Core tables to CSV files...")
    }

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    readr::write_csv(event_ext, file.path(output_dir, "event.csv"))
    readr::write_csv(occurrence_table, file.path(output_dir, "occurrence.csv"))
    readr::write_csv(emof_table, file.path(output_dir, "emof.csv"))
    readr::write_csv(tidy_data, file.path(output_dir, "raw_processed.csv"))

    # Write processing metadata
    metadata_df <- tibble::tibble(
      dataset_title = "40 years of Zooplankton data at LTER MareChiara site (Gulf of Naples, Mediterranean Sea) 1984-2024",
      contact = "Dr. Iole Di Capua (iole.dicapua@szn.it)",
      institution = "Stazione Zoologica Anton Dohrn",
      license = "CC-BY-NC",
      project = "DTO-BioFlow FSTP Grant",
      processing_date = as.character(processing_info$processing_date),
      worms_validation = worms_validation,
      total_events = processing_info$total_events,
      total_occurrences = processing_info$total_occurrences,
      date_range_start = as.character(processing_info$date_range[1]),
      date_range_end = as.character(processing_info$date_range[2])
    )

    readr::write_csv(
      metadata_df,
      file.path(output_dir, "processing_metadata.csv")
    )

    if (verbose) message("Darwin Core tables exported to: ", output_dir)
  }

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

#' Extract standardized genus-species names from taxonomic strings
#'
#' This function parses a vector of taxonomic names from the LTER-MareChiara
#' zooplankton dataset and attempts to extract or standardize genus-species level
#' information for Darwin Core compliance. It handles various cases commonly found
#' in Mediterranean zooplankton taxonomy including species complexes, family-level
#' entries, higher taxonomic groups, and standard binomial names with author citations.
#'
#' The standardization follows these rules:
#' - Species complexes (e.g., "Genus1+Genus2") become "Genus1 spp"
#' - Family-level entries (e.g., "Familidae n.i.") become "Familgenus sp"
#' - Higher groups with dashes (e.g., "Genus - group") become "Genus indet"
#' - Standard binomials with authors are cleaned to "Genus species" format
#' - Subgenus information in parentheses is removed
#' - Unmatched entries retain their original form
#'
#' @param taxa_vector A character vector of taxonomic names from zooplankton samples.
#'   Typically from the TAXA column of LTER-MareChiara dataset.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{original_name}{The original taxonomic string as provided.}
#'   \item{genus_species}{The extracted or standardized genus-species name
#'     suitable for Darwin Core scientificName field.}
#' }
#'
#' @examples
#' # Examples from Gulf of Naples zooplankton samples
#' taxa_examples <- c(
#'   "Sardinella+Sardinops",                    # Species complex
#'   "Clupeidae n.i.",                          # Family level identification
#'   "Engraulis - group",                       # Higher taxonomic group
#'   "Lutjanus (Paradies) argentimaculatus (Forsskål, 1775)",  # With subgenus & author
#'   "Chiridius poppei Giesbrecht, 1893",       # Standard binomial with author
#'   "Larvae n.i."                              # Developmental stage entry
#' )
#'
#' result <- extract_genus_species(taxa_examples)
#' print(result)
#'
#' # Expected output:
#' # original_name                                          genus_species
#' # "Sardinella+Sardinops"                                 "Sardinella spp"
#' # "Clupeidae n.i."                                       "Clupegenus sp"
#' # "Engraulis - group"                                     "Engraulis indet"
#' # "Lutjanus (Paradies) argentimaculatus (Forsskål, 1775)" "Lutjanus argentimaculatus"
#' # "Chiridius poppei Giesbrecht, 1893"                    "Chiridius poppei"
#' # "Larvae n.i."                                          "Larvae n.i."
#'
#' @seealso
#' - Darwin Core standard: \url{https://dwc.tdwg.org/}
#' - World Register of Marine Species: \url{https://www.marinespecies.org/}
#' - LTER-MareChiara station: \url{https://deims.org/0b87459a-da3c-45af-a3e1-cb1508519411}
#'
#' @export
extract_genus_species <- function(taxa_vector) {
  tibble::tibble(
    original_name = taxa_vector,
    genus_species = dplyr::case_when(
      is.na(taxa_vector) ~ NA_character_,

      # Handle species complexes with + (convert to Genus spp.)
      stringr::str_detect(taxa_vector, "\\+") ~
        {
          genus <- stringr::str_extract(taxa_vector, "^[A-Z][a-z]+")
          paste(genus, "spp")
        },

      # Handle family level with n.i. (create genus name from family)
      stringr::str_detect(taxa_vector, "[A-Z][a-z]*idae\\s+n\\.i\\.") ~
        {
          family <- stringr::str_extract(taxa_vector, "[A-Z][a-z]*idae")
          genus <- stringr::str_replace(family, "idae$", "genus")
          paste(genus, "sp")
        },

      # Handle higher groups with dashes (take main group)
      stringr::str_detect(taxa_vector, "^[A-Z][a-z]+\\s*-") ~
        {
          main_group <- stringr::str_extract(taxa_vector, "^[A-Z][a-z]+")
          paste(main_group, "indet")
        },

      # Extract Genus species from standard binomial patterns
      # Handles: "Genus species (Author, Year)" or "Genus (Subgenus) species Author"
      stringr::str_detect(
        taxa_vector,
        "^[A-Z][a-z]+\\s+(?:\\([A-Z][a-z]+\\)\\s+)?[a-z]+"
      ) ~
        stringr::str_extract(
          taxa_vector,
          "^[A-Z][a-z]+(?:\\s+\\([A-Z][a-z]+\\))?\\s+[a-z]+"
        ) %>%
          stringr::str_remove("\\s+\\([A-Z][a-z]+\\)") |> # Remove subgenus if present
          stringr::str_trim(),

      # For everything else, keep original
      TRUE ~ taxa_vector
    )
  )
}
