## code to prepare zoo data (1984-2015)

# load configuration parameters
devtools::load_all()
conf <- read_config()

# get legacy data from legacy_data bucket
ids <-
  download_sharepoint_file(
    prefix = "ids_84_15.csv",
    options = conf$storage$sharepoint,
    bucket = "legacy_data",
    filename = TRUE
  ) |>
  dplyr::mutate(
    date = lubridate::as_date(as.numeric(.data$date), origin = "1899-12-30"),
    sample_id = janitor::make_clean_names(.data$sample_id),
    sample_id = stringr::str_replace_all(.data$sample_id, "_", "")
  )


bio <-
  download_sharepoint_file(
    prefix = "zoo_84_15.csv",
    options = conf$storage$sharepoint,
    bucket = "legacy_data",
    filename = TRUE
  ) |>
  dplyr::mutate(
    dat_id = seq_len(dplyr::n()),
    TAXA = stringr::str_trim(.data$TAXA)
  )


dates <-
  bio |>
  dplyr::select(-c(1:10), "dat_id")

# match taxa down to worms

reported_taxa <-
  as.character(unique(bio$TAXA))

worms_matched <- purrr::map2_dfr(
  .x = seq_along(reported_taxa),
  .y = reported_taxa,
  .f = function(i, taxon) {
    res <- tryCatch(
      worrms::wm_records_taxamatch(taxon),
      error = function(e) NULL
    )
    if (
      is.null(res) ||
        length(res) == 0 ||
        is.null(res[[1]]) ||
        nrow(res[[1]]) == 0
    ) {
      return(dplyr::tibble(
        original = taxon,
        AphiaID = NA_integer_,
        scientificname = NA_character_,
        status = NA_character_,
        match_type = "no_match"
      ))
    }

    res[[1]] %>% dplyr::mutate(original = taxon, .before = 1)
  }
)


# Ensure we get one AphiaID per taxon
worms_matched_clean <-
  worms_matched |>
  dplyr::select(
    "original",
    "AphiaID",
    "lsid",
    "scientificname",
    "status",
    "match_type"
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(.data$original) |>
  dplyr::arrange(.data$AphiaID, .by_group = TRUE) |>
  #take only the first row by group (i.e. pick the oldest classification)
  dplyr::slice_head(n = 1) |>
  dplyr::select(-"AphiaID") |>
  dplyr::ungroup()


# Merge taxa datafrmae with worms and add dates and ids
taxa_df <-
  bio |>
  dplyr::select(
    "dat_id",
    reported_taxa = "TAXA",
    "stage",
  ) |>
  dplyr::full_join(worms_matched_clean, by = c("reported_taxa" = "original")) |>
  dplyr::full_join(dates, by = "dat_id") |>
  tidyr::pivot_longer(
    -c(
      "dat_id",
      "reported_taxa",
      "lsid",
      "scientificname",
      "status",
      "match_type",
      "stage"
    ),
    names_to = "date",
    values_to = "ind_m3"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = lubridate::as_date(as.numeric(.data$date), origin = "1899-12-30")
  ) |>
  dplyr::select(-"dat_id") |>
  dplyr::left_join(ids, by = "date") |>
  dplyr::relocate("sample_id", .before = "reported_taxa") |>
  dplyr::relocate("stage", .after = "ind_m3") |>
  dplyr::relocate("date", .after = "sample_id") |>
  dplyr::arrange(.data$sample_id) |>
  dplyr::arrange(.data$date, .data$sample_id) |>
  dplyr::rename(
    eventID = "sample_id",
    eventDate = "date",
    individualCount = "ind_m3",
    lifeStage = "stage"
  ) |>
  dplyr::distinct()


# after executing the taxa_id code, the data is not sorted chronologically
# check the eventID order vs chronological order
# probably because it's sorted by eventID (alphanumeric) instead of eventDate
View(taxa_df)
taxa_df[1194:1204, ]
summary(taxa_df$eventDate)

taxa_df |>
  dplyr::distinct(eventID, eventDate) |>
  dplyr::arrange(eventID) |>
  print(n = 30)

taxa_df |>
  dplyr::distinct(eventID, eventDate) |>
  dplyr::arrange(eventDate) |>
  print(n = 30)

# Check for unmatched taxa (to be checked by curators)
unmatched <-
  taxa_df |>
  dplyr::select(
    "reported_taxa",
    "scientificname",
    "lsid",
    "status",
    "match_type"
  ) |>
  dplyr::distinct() |>
  dplyr::filter(is.na(.data$lsid)) |>
  dplyr::select("reported_taxa") |>
  dplyr::distinct() |>
  dplyr::mutate(
    "accepted scientific name" = NA_character_,
    "lifestage" = NA_character_
  )

unmatched

# to upload to sharepoint
xlsx::write.xlsx(unmatched, "unmatched_worms_84_15.xlsx", sheetName = "unmatch")

# prepare ready for export
tidy_data <-
  taxa_df |>
  dplyr::select(
    "eventID",
    "eventDate",
    "scientificname",
    "lsid",
    "individualCount",
    "lifeStage"
  ) |>
  dplyr::rename(
    scientificName = scientificname
  ) |>
  dplyr::distinct() |>
  dplyr::filter(!is.na(.data$lsid)) |>
  # standardize lifeStage format
  dplyr::mutate(
    lifeStage = dplyr::case_when(
      .data$lifeStage == "f/m" ~ "fm",
      .data$lifeStage == "f+m+j" ~ "fmj",
      TRUE ~ .data$lifeStage
    )
  )

# ==============================================================================
# INTEGRATION CORRECTED UNMATCHED TAXA (Andrea)
# ==============================================================================

# load xlsx file from working directory and convert to csv
unmatched_corrected <-
  readxl::read_excel("unmatched_worms_84_15 (1).xlsx", sheet = "unmatch")

# Save as CSV with semicolon separator to avoid issues with commas in scientific names
readr::write_csv2(unmatched_corrected, "unmatched_worms_84_15.csv")

cat("File convertito da xlsx a csv\n")

unmatched_corrected

# Prepare corrected data with proper lsid format
unmatched_corrected_clean <-
  unmatched_corrected |>
  janitor::clean_names() |>
  # After clean_names(): reported_taxa, accepted_scientific_name, aphia_id, lifestage
  dplyr::rename(
    scientificname = accepted_scientific_name,
    lsid = aphia_id
  )

unmatched_corrected_clean
recovered_records |> View()
# Recover originally excluded records from taxa_df
recovered_records <-
  taxa_df |>
  # Get records that were excluded (no lsid)
  dplyr::filter(is.na(lsid)) |>
  # Remove old incomplete data
  dplyr::select(-scientificname, -lsid, -status, -match_type) |>
  # Join with corrected data
  dplyr::left_join(
    unmatched_corrected_clean |>
      dplyr::select(
        reported_taxa,
        scientificname,
        lsid,
        lifestage_corrected = lifestage
      ),
    by = "reported_taxa"
  ) |>
  # Use corrected lifestage if available, otherwise keep original --> condition: lifestage_corrected is not NA: if true use the value from lifestage_corrected; if false keep the original value of lifeStage
  dplyr::mutate(
    lifeStage = dplyr::if_else(
      !is.na(lifestage_corrected),
      lifestage_corrected,
      lifeStage
    )
  ) |>
  # Select same columns as tidy_data
  dplyr::select(
    eventID,
    eventDate,
    scientificname,
    lsid,
    individualCount,
    lifeStage
  ) |>
  dplyr::rename(
    scientificName = scientificname
  ) |>
  dplyr::distinct()

# integrate recovered records into final dataset
tidy_data_complete <-
  dplyr::bind_rows(tidy_data, recovered_records) |>
  dplyr::distinct() |>
  # Ensure chronological order
  dplyr::arrange(eventDate, eventID)

tidy_data_complete |> View()

# Verify integration
cat("\nOriginal tidy_data rows:", nrow(tidy_data))
cat("\nRecovered records:", nrow(recovered_records))
cat("\nFinal tidy_data_complete rows:", nrow(tidy_data_complete))
cat("\n")

# Check which taxa were recovered
recovered_taxa <-
  recovered_records |>
  dplyr::distinct(scientificName) |>
  dplyr::pull(scientificName)

cat("Recovered taxa:\n")
print(recovered_taxa)

# WoRMS match test
# Extract all unique scientific names from the final dataset, use as.character() to convert any factors to strings
reported_taxa_final <-
  as.character(unique(tidy_data_complete$scientificName))

# Print how many unique taxa need to check
cat("Totale taxa da verificare:", length(reported_taxa_final), "\n\n")

# Match taxa on WoRMS
# purrr::map2_dfr() iterates over two vectors simultaneously and combines the results into a dataframe.
worms_matched_final <- purrr::map2_dfr(
  .x = seq_along(reported_taxa_final), # .x = numerical index (1, 2, 3, ...)
  .y = reported_taxa_final, # .y = taxon name ("Calanus helgolandicus", ...)
  .f = function(i, taxon) {
    # function that is performed for each taxon

    # Progress indicator
    # print every 10 taxa processed
    # the %% operator calculates the remainder of the division (modulo)
    # when i is a multiple of 10 (remainder = 0), print the progress
    if (i %% 10 == 0) {
      cat("Processing taxon", i, "of", length(reported_taxa_final), "\n")
    }
    # search for the taxon on WoRMS tryCatch() handles any errors without blocking execution, ff there is an error, it returns NULL
    res <- tryCatch(
      worrms::wm_records_taxamatch(taxon),
      error = function(e) NULL
    )
    # check if the WoRMS search failed or found no results by verifying 4 conditions
    if (
      is.null(res) ||
        length(res) == 0 ||
        is.null(res[[1]]) ||
        nrow(res[[1]]) == 0
    ) {
      # if no match is found, return a dataframe with NA values
      return(dplyr::tibble(
        original = taxon,
        AphiaID = NA_integer_,
        scientificname = NA_character_,
        status = NA_character_,
        match_type = "no_match"
      ))
    }
    # if the match is found, it returns the result res[[1]] contains the dataframe with the match results
    # dplyr::mutate() adds the ‘original’ column at the beginning (.before = 1)
    res[[1]] %>% dplyr::mutate(original = taxon, .before = 1)
  }
)
worms_matched_final |> View()
worms_matched_final


# Check for any unmatched taxa
# Filter taxa that did not find a valid match on WoRMS under two conditions: AphiaID is NA (no ID found); match_type = “no_match” (explicitly marked as not matched)
verification_unmatched <-
  worms_matched_final |>
  dplyr::filter(is.na(AphiaID) | match_type == "no_match") |>
  dplyr::distinct(original)
verification_unmatched

# final report
cat("\n=== VERIFICATION RESULTS ===\n")
cat("Total taxa checked:", length(reported_taxa_final), "\n")
cat("Unmatched taxa found:", nrow(verification_unmatched), "\n")

# if there are unmatched taxa, display a warning and print them
# otherwise, confirm that everything is OK!
if (nrow(verification_unmatched) > 0) {
  cat("\n WARNING: Some taxa still don't match on WoRMS:\n")
  print(verification_unmatched)
} else {
  cat("\n SUCCESS: All taxa match on WoRMS!\n")
}

#==============================================================================================================================================================================================================

# export csv and parquet tidy files to hot storage bucket
formats <- c("parquet", "csv")

purrr::walk(formats, function(fmt) {
  filename <- paste0("McZoo_84-15.", fmt)

  # Write locally
  if (fmt == "parquet") {
    arrow::write_parquet(tidy_data, filename)
  } else {
    readr::write_csv2(tidy_data, filename)
  }

  # Upload to SharePoint
  upload_sharepoint_df(
    data = tidy_data_complete,
    prefix = filename,
    bucket = "hot_storage",
    options = conf$storage$sharepoint,
    format = fmt,
    filename = TRUE
  )
})
