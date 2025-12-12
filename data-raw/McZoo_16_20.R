## code to prepare zoo data (2016-2020)

# Load packages and configurations (including credentials from .env)
devtools::load_all()
conf <- read_config()

# get legacy data from legacy_data bucket from sharepoint, formatting data and name
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

# Inside a pipe: use .data$column or simply column.
# Outside the pipe: you can use dataframe$column.
# The pipe creates a “data flow” where the dataframe is transformed step by step, and .data always refers to the current state of that flow

# get legacy data from legacy_data bucket from sharepoint, formatting data
bio <- download_sharepoint_file(
  prefix = "zoo_16_20.csv",
  options = conf$storage$sharepoint,
  bucket = "legacy_data",
  filename = TRUE
) |>
  dplyr::mutate(
    date = lubridate::mdy(.data$date),
    dat_id = seq_len(dplyr::n()),
    taxa = stringr::str_trim(.data$taxa),
    ind_m3 = stringr::str_replace_all(ind_m3, ",", ".")
  )

## Data Integration
# previous run of the code highlighted some taxa that did not match on worms
# downloading this data into a dataframe called “unmatched”
# where the accepted scientific name and the respective AphiaID were added.

# integrate unmatched taxa
unmatched_fixed <-
  download_sharepoint_file(
    prefix = "unmatched_worms_16_20.xlsx",
    options = conf$storage$sharepoint,
    bucket = "worms_unmatched",
    filename = TRUE
  ) |>
  janitor::clean_names()

# Add worms unmatched to our dataset
update_bio <-
  bio |>
  dplyr::left_join(unmatched_fixed, by = c("taxa" = "reported_taxa")) |>
  dplyr::mutate(
    taxa = dplyr::case_when(
      taxa = !is.na(accepted_scientific_name) ~ accepted_scientific_name,
      TRUE ~ taxa
    ),
    stage = dplyr::case_when(!is.na(lifestage) ~ lifestage, TRUE ~ stage)
  ) |>
  dplyr::select(-c(accepted_scientific_name, aphia_id, lifestage))

## Data Validation
# This code performs taxonomic validation:
# It takes the names of taxa from your data (which may contain errors, synonyms, old names)
# searches for them in the international WoRMS database
# obtains standardized and verified names
# keeps track of which taxa were not found.

# extracts all unique values from the taxa column (removes duplicates) and converts them to characters
reported_taxa <-
  as.character(unique(update_bio$taxa))

# purrr::map2_dfr:
# Iterates over two vectors simultaneously,
# applies a function to each pair of elements
# combines the results into a dataframe (_dfr = “data frame by rows”)
worms_matched <- purrr::map2_dfr(
  .x = seq_along(reported_taxa),
  .y = reported_taxa,
  .f = function(i, taxon) {
    # internal function search on WoRMS, performs this function for each taxon
    res <- tryCatch(
      # Handles errors - if the search fails (e.g., internet connection), it returns NULL instead of blocking all code
      worrms::wm_records_taxamatch(taxon), # Search for the taxon in the WoRMS database
      error = function(e) NULL # it takes the error (e) as input and returns it as NULL instead of interrupting the code
    )
    if (
      # If any of these conditions are true → no match found:
      is.null(res) || # The search did not return any results
        length(res) == 0 || # The object is empty
        is.null(res[[1]]) || # The first element is NULL
        nrow(res[[1]]) == 0 # There are no rows of data
    ) {
      return(dplyr::tibble(
        # Management of “no match”, Create a dataframe with:
        original = taxon, # The original name searched for
        AphiaID = NA_integer_, # WoRMS database ID (NA = not available)
        scientificname = NA_character_, # Standardized scientific name (NA)
        status = NA_character_, # Status tassonomico (NA)
        match_type = "no_match" # “no_match” indicates that no match was found
      ))
    } # If the match has been found:
    res[[1]] %>% dplyr::mutate(original = taxon, .before = 1) # res[[1]]: Extracts the first search result, mutate(original = taxon, .before = 1): adds the original column with the original name searched for as the first column
  }
)

## Data WoRMS TEST
# Check for any unmatched taxa
# Filter taxa that did not find a valid match on WoRMS under two conditions: AphiaID is NA (no ID found); match_type = “no_match” (explicitly marked as not matched)
verification_unmatched <-
  worms_matched |>
  dplyr::filter(is.na(valid_AphiaID) | match_type == "no_match") |>
  dplyr::distinct(original)


cat("Unmatched taxa found:", nrow(verification_unmatched), "\n")

# if there are unmatched taxa, display a warning and print them
# otherwise, confirm that everything is OK!
if (nrow(verification_unmatched) > 0) {
  cat("\n WARNING: Some taxa still don't match on WoRMS:\n")
  print(verification_unmatched)
} else {
  cat("\n SUCCESS: All taxa match on WoRMS!\n")
}

# Ensure we get one AphiaID per taxon
worms_matched_clean <-
  worms_matched |>
  dplyr::select(
    # select only some columns from the data frame "worms_matched"
    "original",
    "AphiaID",
    "lsid",
    "scientificname",
    "status",
    "match_type"
  ) |>
  dplyr::distinct() |> # remove duplicates from all selected columns
  dplyr::group_by(.data$original) |> # group the rows by the value of the original variable (taxa), for each original name, analyze all its possible matches together.
  dplyr::arrange(.data$AphiaID, .by_group = TRUE) |> # within each group, sort the rows by AphiaID (in ascending order), where a smaller AphiaID corresponds to an older classification.
  dplyr::slice_head(n = 1) |> # take only the first row by group (i.e. pick the oldest classification), Lowest AphiaID (oldest classification)
  dplyr::select(-"AphiaID") |> # removes the AphiaID column after making the selection, because it is no longer needed
  dplyr::ungroup() # removes the group structure, returning a normal dataframe

# Merge the zooplankton dataframe with validated WoRMS taxonomy,
# merge with ids data frame and sample metadata from worms, reorder and rename columns,
# and remove duplicates to produce a clean, analysis-ready taxa dataframe.
taxa_df <-
  update_bio |>
  dplyr::select(
    "dat_id",
    "sample_id",
    reported_taxa = "taxa",
    "stage",
    "date",
    "ind_m3",
  ) |>
  dplyr::full_join(worms_matched_clean, by = c("reported_taxa" = "original")) |>
  janitor::clean_names() |>
  dplyr::select(-"dat_id") |>
  dplyr::left_join(ids, by = c("date", "sample_id")) |>
  dplyr::relocate("stage", .after = "ind_m3") |>
  dplyr::relocate("date", .after = "sample_id") |>
  dplyr::arrange(.data$sample_id) |>
  dplyr::rename(
    eventID = "sample_id",
    eventDate = "date",
    IndividualCount = "ind_m3",
    lifeStage = "stage"
  ) |>
  dplyr::distinct()

# Check for unmatched taxa (to be checked by curators)
# Identify taxa that did not match any entries in the WoRMS database
# to allow manual review by editors
unmatched <-
  taxa_df |>
  dplyr::select(
    # Select only the columns relevant for taxonomic control
    "reported_taxa",
    "scientificname",
    "lsid",
    "status",
    "match_type"
  ) |>
  dplyr::distinct() |> # remove diplicates rows, important because the same taxon may appear in multiple samples we only want a single list of taxa not found
  dplyr::filter(is.na(.data$lsid)) |> # filter only taxa NOT found in WoRMS
  dplyr::select("reported_taxa") |> # keep only the column with the original names
  dplyr::distinct() |>
  dplyr::mutate(
    "accepted scientific name" = NA_character_,
    "lifestage" = NA_character_
  )

# prepare ready for export
tidy_data <-
  taxa_df |>
  dplyr::select(
    # select only the columns needed for export
    "eventID",
    "eventDate",
    "scientificname",
    "lsid",
    "IndividualCount",
    "lifeStage"
  ) |>
  dplyr::distinct() |> # keeps only one copy of each unique combination
  dplyr::filter(!is.na(.data$lsid)) |> # filter, Remove unvalidated taxa, keep only those with a valid lsid (found in WoRMS), automatically exclude all taxa “unmatched”
  dplyr::mutate(
    # standardize the format of vital stages
    lifeStage = dplyr::case_when(
      .data$lifeStage == "f+m" ~ "fm",
      .data$lifeStage == "f+m+j" ~ "fmj",
      .data$lifeStage == "larvae" ~ "lar",
      .data$lifeStage == "eggs" ~ "egg",
      TRUE ~ .data$lifeStage # for all other cases, leave the original value unchanged.
    )
  ) |>
  dplyr::mutate(
    IndividualCount = dplyr::if_else(
      IndividualCount %in% c("#N/D", "#N/A"),
      NA_character_,
      IndividualCount # convert "#N/D" e "#N/A" in NA
    ),
    lifeStage = dplyr::if_else(
      lifeStage == "#N/D",
      NA_character_,
      lifeStage
    ),
    IndividualCount = as.numeric(IndividualCount), # convert from character to numeric
  )

# export csv and parquet tidy files to hot storage bucket
# vreate a vector with the two formats to be generated
formats <- c("parquet", "csv")

# Iterate over both formats using purrr::walk, used for saving files, printing, etc.
# create the file name with the correct extension, first iteration .parquet and second one .csv
purrr::walk(formats, function(fmt) {
  filename <- paste0("McZoo_16-20.", fmt)

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
    bucket = "hot_storage",
    options = conf$storage$sharepoint,
    format = fmt,
    filename = TRUE
  )
})
