extract_genus_species <- function(taxa_vector) {
  tibble::tibble(
    original_name = taxa_vector,
    genus_species = case_when(
      is.na(taxa_vector) ~ NA_character_,

      # Handle species complexes with + (convert to Genus spp.)
      str_detect(taxa_vector, "\\+") ~
        {
          genus <- str_extract(taxa_vector, "^[A-Z][a-z]+")
          paste(genus, "spp")
        },

      # Handle family level with n.i. (create genus name from family)
      str_detect(taxa_vector, "[A-Z][a-z]*idae\\s+n\\.i\\.") ~
        {
          family <- str_extract(taxa_vector, "[A-Z][a-z]*idae")
          genus <- str_replace(family, "idae$", "genus")
          paste(genus, "sp")
        },

      # Handle higher groups with dashes (take main group)
      str_detect(taxa_vector, "^[A-Z][a-z]+\\s*-") ~
        {
          main_group <- str_extract(taxa_vector, "^[A-Z][a-z]+")
          paste(main_group, "indet")
        },

      # Extract Genus species from standard binomial patterns
      # Handles: "Genus species (Author, Year)" or "Genus (Subgenus) species Author"
      str_detect(
        taxa_vector,
        "^[A-Z][a-z]+\\s+(?:\\([A-Z][a-z]+\\)\\s+)?[a-z]+"
      ) ~
        str_extract(
          taxa_vector,
          "^[A-Z][a-z]+(?:\\s+\\([A-Z][a-z]+\\))?\\s+[a-z]+"
        ) %>%
        str_remove("\\s+\\([A-Z][a-z]+\\)") %>% # Remove subgenus if present
        str_trim(),

      # For everything else, keep original
      TRUE ~ taxa_vector
    )
  )
}

dat <- readxl::read_xlsx(
  "data/lter_zoo_84_13.xlsx",
  .name_repair = "minimal"
) |>
  dplyr::mutate(dat_id = seq_len(dplyr::n()))

ids <-
  readxl::read_xlsx(
    "data/ids.xlsx",
    .name_repair = "minimal"
  ) |>
  dplyr::mutate(
    date = lubridate::as_date(as.numeric(date), origin = "1899-12-30"),
    sample_id = janitor::make_clean_names(sample_id)
  )

dates <-
  dat |>
  dplyr::select(-c(1:10), "dat_id")


raw_taxa <-
  dat |>
  janitor::clean_names() |>
  dplyr::select("phylum_o_subphylum":"stage", "dat_id") |>
  dplyr::mutate(
    genus_species = extract_genus_species(.data$taxa)$genus_species,
  ) |>
  tidyr::separate(
    genus_species,
    into = c("genus", "species"),
    sep = " ",
    remove = FALSE
  ) |>
  dplyr::mutate(taxa_worrms = paste(.data$genus_subgenus, .data$species))

worrms_matched <-
  raw_taxa |>
  dplyr::select("dat_id", "taxa_worrms") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    worrms_match = tryCatch(
      {
        worrms::wm_records_taxamatch(name = taxa_worrms, verbose = FALSE)
      },
      error = function(e) {
        # Return NA or empty tibble when there's an error
        NA
      }
    )
  ) |>
  dplyr::select(-"taxa_worrms")

tidy_data <-
  raw_taxa |>
  dplyr::select(-"taxa_worrms") |>
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
    date = lubridate::as_date(as.numeric(date), origin = "1899-12-30"),
  ) |>
  dplyr::select(-"dat_id") |>
  dplyr::left_join(ids, by = c("date")) |>
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
  #dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  #tidyr::pivot_longer(
  #  cols = -c(eventID, eventDate, scientificName),
  #  names_to = "meameasurementType",
  #  values_to = "measurementValue") |>
  dplyr::distinct()


event_ext <-
  tidy_data |>
  dplyr::select(eventID, eventDate) |>
  dplyr::distinct() |>
  # add geo info
  dplyr::mutate(
    decimalLatitude = 40.81,
    decimalLongitude = -14.25
  )

full_table <-
  tidy_data |>
  dplyr::mutate(
    occurrenceStatus = dplyr::if_else(individualCount > 0, "present", "absent")
  ) |>
  #dplyr::select(eventID, scientificName, lifeStage, occurrenceStatus) |>
  dplyr::mutate(occurrenceID = paste0(eventID, "-", "occ", 1:dplyr::n())) |>
  dplyr::relocate(occurrenceID, .after = "eventDate") |>
  dplyr::distinct()

occurrence_table <-
  full_table |>
  dplyr::select(
    eventID,
    occurrenceID,
    scientificName,
    lifeStage,
    occurrenceStatus
  )

emof_table <-
  full_table |>
  dplyr::select(-c(scientificName, occurrenceStatus)) |>
  dplyr::distinct() |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  tidyr::pivot_longer(
    cols = -c(eventID, eventDate, occurrenceID),
    names_to = "meameasurementType",
    values_to = "measurementValue"
  )
