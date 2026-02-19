# McZoo 21_24
# Load packages and configurations
conf <- read_config()

# TO DO: develop function to download from sharepoint processed data
# Data ingestion
# Upload raw data from kobo toolbox
ingest_surveys()
# Download and preview raw data
results <- preprocess_surveys()

# Data Cleaning
# Select relevant columns and create event identifiers
clean_21_24 <-
  results |>
  dplyr::select(
    "site_name",
    "sampling_id",
    "sampling_date",
    "taxon",
    "is_copepod",
    dplyr::starts_with("n_")
  ) |>
  dplyr::mutate(eventID = paste0(site_name, sampling_id)) |>
  dplyr::relocate(eventID, .before = site_name) |>
  dplyr::select(-c("site_name", "sampling_id")) |>
  dplyr::arrange(sampling_date) |>
  dplyr::rename(
    eventDate = "sampling_date",
    aphiaID = "taxon",
    isCopepod = "is_copepod"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), as.numeric)) |>
  # TO DO: Ideally there should be no NAs unless the net was empty(!), to clarify. Meanwhile we drop all NAs
  dplyr::filter(!is.na(aphia_id))

# Get unique AphiaID
unique_aphia_ids <- as.numeric(unique(clean_21_24$aphia_id))

worms_records <-
  unique_aphia_ids |>
  purrr::map_dfr(worrms::wm_record) |>
  dplyr::select(aphia_id = "AphiaID", "scientificname", "lsid") |>
  dplyr::mutate(aphia_id = as.character(aphia_id))

# Data harmonization as other matrices
tidy_data <-
  clean_21_24 |>
  dplyr::left_join(worms_records, by = "aphia_id") |>
  dplyr::relocate("scientificname", "lsid", .after = "aphia_id") |>
  # Pivot abundance columns to long format
  tidyr::pivot_longer(
    cols = c(
      "n_male",
      "n_female",
      "n_copepodite",
      "n_undetermined",
      "n_larvae",
      "n_eggs",
      "n_nauplius"
    ),
    names_to = "life_stage_temp",
    values_to = "individual_count"
  ) |>
  # Convert abundance to numeric and map life stages
  dplyr::mutate(
    life_stage = dplyr::case_when(
      life_stage_temp == "n_male" ~ "m",
      life_stage_temp == "n_female" ~ "f",
      life_stage_temp == "n_copepodite" ~ "j",
      life_stage_temp == "n_undetermined" ~ "fmj",
      life_stage_temp == "n_larvae" ~ "lar",
      life_stage_temp == "n_eggs" ~ "egg",
      # TO DO: do we have nauplii only in 21-24 data?
      life_stage_temp == "n_nauplius" ~ "nau",
      TRUE ~ NA_character_
    )
  ) |>
  # Clean and organize columns
  dplyr::select(
    -c("life_stage_temp", "aphia_id", "n_individuals", "n_sample")
  ) |>
  dplyr::filter(!is.na(individual_count)) |>
  dplyr::rename(
    scientificName = "scientificname",
    eventID = "event_id",
    eventDate = "event_date",
    lifeStage = "life_stage",
    individualCount = "individual_count",
    isCopepod = "is_copepod"
  ) |>
  dplyr::distinct() |>
  # remove NA counts & IDs
  dplyr::filter(
    !is.na(.data$individualCount),
    !is.na(.data$eventDate),
    !is.na(.data$eventID)
  ) |>
  # remove duplicates
  dplyr::group_by(eventID, eventDate, scientificName, lsid, isCopepod, lifeStage) |>
  dplyr::summarise(
    individualCount = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::relocate("individualCount", .before = "lifeStage")


# export csv and parquet tidy files to hot storage bucket
# vreate a vector with the two formats to be generated
formats <- c("parquet", "csv")

# Iterate over both formats using purrr::walk, used for saving files, printing, etc.
# create the file name with the correct extension, first iteration .parquet and second one .csv
purrr::walk(formats, function(fmt) {
  filename <- paste0("McZoo_21-24.", fmt)

  # Write locally
  if (fmt == "parquet") {
    arrow::write_parquet(tidy_data, filename)
  } else {
    readr::write_csv2(tidy_data, filename)
  }

  # Upload to SharePoint
  upload_sharepoint_df(
    data = tidy_data,
    prefix = filename,
    bucket = conf$storage$sharepoint$buckets$hot_bucket,
    options = conf$storage$sharepoint$credentials,
    format = fmt,
    filename = TRUE
  )
})
