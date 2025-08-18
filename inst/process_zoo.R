extract_genus_species <- function(taxa_vector) {
  require(stringr)
  require(dplyr)

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
usethis::use_devtools()

dat <- readxl::read_xlsx(
  "lter_zoo_84_13.xlsx",
  .name_repair = "minimal"
)

ids <-
  readxl::read_xlsx(
    "ids.xlsx",
    .name_repair = "minimal"
  ) |>
  dplyr::mutate(
    date = lubridate::as_date(as.numeric(date), origin = "1899-12-30"),
    sample_id = janitor::make_clean_names(sample_id)
  )


tidy_data <-
  dat |>
  dplyr::select(-c(1:7, "NOTES")) |>
  tidyr::pivot_longer(
    -c("TAXA", "stage"),
    names_to = "date",
    values_to = "ind_m3"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = lubridate::as_date(as.numeric(date), origin = "1899-12-30"),
    #taxa_std = extract_genus_species(taxa)$genus_species
  ) |>
  dplyr::full_join(ids, by = c("date")) |>
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
occurrence_table |>
  dplyr::filter(scientificName == "Larvae n.i." & occurrenceStatus == "present")

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


emof_table |>
  dplyr::filter(occurrenceID == "mc_318-occ130001")


occurrence_table |>
  dplyr::filter(occurrenceID == "mc_1-occ29")


full_table |>
  dplyr::filter(
    scientificName == "Chiridius poppei Giesbrecht, 1893",
    eventID == "mc_1"
  )


we <-
  full_table |>
  dplyr::filter(individualCount > 0 & is.na(lifeStage))


unique(we$scientificName)
