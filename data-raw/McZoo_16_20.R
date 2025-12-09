## code to prepare zoo data (2016-2020)
# Load packages and configurations (including credentials from .env)
devtools::load_all()
conf <- read_config()

# get legacy data from legacy_data bucket from sharepoint, formatting data and name
ids <-
  download_sharepoint_file(
    prefix = "ids_16_20.csv",
    options = conf$storage$sharepoint,
    bucket = "legacy_data",
    filename = TRUE
  ) |>
  dplyr::rename(sample_id = id) |>
  dplyr::mutate(
    filtered_volume_m3 = dplyr::if_else(
      filtered_volume_m3 == "#N/D",
      NA_character_,
      filtered_volume_m3
    ),
    date = lubridate::mdy(.data$date)
  )


# Inside a pipe: use .data$column or simply column.
# Outside the pipe: you can use dataframe$column.
# # The pipe creates a “data flow” where the dataframe is transformed step by step, and .data always refers to the current state of that flow

# verifying variables to understand the nature of the variable helped me get the code to work because the dates were of a “character” nature.
# date is "character" class, I must convert it directly from string to date
# class(ids$date)

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


## Data Cleaning: this code downloads historical zooplankton data from SharePoint and fixes a CSV parsing issue where commas in scientific names caused data to shift between columns
# I apply transformations because the “dowload_sharepoint_file” function confuses the “,” separator in the CSV file with data such as scientific names
# convert everything to characters
# zoo <- zoo |>
#   dplyr::mutate(
#     dat_id = seq_len(dplyr::n()),
#     stage = as.character(stage),
#     ind_m3 = as.character(ind_m3), # Convert stage and ind_m3 into characters for manipulation
#     has_comma = grepl(",", ind_m3, fixed = TRUE), # Create a temporary column has_comma that is TRUE if ind_m3 contains a comma
#     taxa = dplyr::case_when(
#       grepl("[0-9]{4}", stage) ~ paste0(taxa, ", ", stage),
#       TRUE ~ taxa
#     ), # Reconstructs the name of the taxa: if stage contains 4 consecutive digits (probably data that ended up there by mistake), it adds it to taxa, otherwise it leaves taxa as it is
#     stage = dplyr::case_when(
#       has_comma ~ sub(",.*", "", ind_m3),
#       TRUE ~ stage
#     ), # Corrects stage: if there is a comma in ind_m3, it takes everything before the comma and puts it in stage, otherwise it leaves stage as it is
#     ind_m3 = dplyr::case_when(
#       has_comma ~ sub(".*,", "", ind_m3),
#       TRUE ~ ind_m3
#     ), # Correct ind_m3: if there is a comma, take everything after the comma otherwise, leave ind_m3 as it is
#     stage = dplyr::na_if(stage, "#N/D"),
#     ind_m3 = dplyr::na_if(ind_m3, "#N/D"), # Converts the text #N/D (Not Available, equivalent to #N/A in Excel) to NA (missing value in R)
#     ind_m3 = as.numeric(ind_m3),
#     date = lubridate::mdy(date) # Convert dates
#   ) |>
#   dplyr::select(-has_comma) # Removes the temporary column: has_comma

# class(zoo)
# head(zoo)
# View(zoo)

# This code performs taxonomic validation:
# It takes the names of taxa from your data (which may contain errors, synonyms, old names)
# searches for them in the international WoRMS database
# obtains standardized and verified names
# keeps track of which taxa were not found.

# match taxa down to worms
reported_taxa <-
  as.character(unique(bio$taxa)) # extracts all unique values from the taxa column (removes duplicates) and converts them to characters (text strings).

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
  bio |>
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
  dplyr::select(-c("filtered_volume_m3")) |>
  #dplyr::relocate("filtered_volume_m3", .before = "ind_m3") |>
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

filename <- "taxa_unmatched_16_20.csv"
# Upload to SharePoint
upload_sharepoint_df(
  data = unmatched,
  prefix = filename,
  bucket = "worms_unmatched",
  options = conf$storage$sharepoint,
  format = "csv",
  filename = TRUE
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
  dplyr::filter(!is.na(.data$lsid)) |> # filter, Remove unvalidated taxa, keep only those with a valid lsid (found in WoRMS), automatically exclude all 53 taxa in “unmatched”
  dplyr::mutate(
    # standardize the format of vital stages
    lifeStage = dplyr::case_when(
      .data$lifeStage == "f+m" ~ "fm",
      .data$lifeStage == "f+m+j" ~ "fmj",
      TRUE ~ .data$lifeStage # for all other cases, leave the original value unchanged.
    )
  ) |>
  dplyr::mutate(
    IndividualCount = dplyr::if_else(
      IndividualCount == "#N/D",
      NA_character_,
      IndividualCount
    ),
    lifeStage = dplyr::if_else(
      lifeStage == "#N/D",
      NA_character_,
      lifeStage
    ),
    IndividualCount = as.numeric(IndividualCount),
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
