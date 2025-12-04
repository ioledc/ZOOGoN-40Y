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
    date = lubridate::mdy(.data$date) # .data$date means: “take the dates column from the current dataframe
  )

# Inside a pipe: use .data$column or simply column.
# Outside the pipe: you can use dataframe$column.
# # The pipe creates a “data flow” where the dataframe is transformed step by step, and .data always refers to the current state of that flow

# verifying variables to understand the nature of the variable helped me get the code to work because the dates were of a “character” nature.
# date is "character" class, I must convert it directly from string to date
class(ids$date)
View(ids)

# get legacy data from legacy_data bucket from sharepoint, formatting data
zoo <- download_sharepoint_file(
  prefix = "zoo_16_20.csv",
  options = conf$storage$sharepoint,
  bucket = "legacy_data",
  filename = TRUE
)

View(zoo)

## Data Cleaning: this code downloads historical zooplankton data from SharePoint and fixes a CSV parsing issue where commas in scientific names caused data to shift between columns
# I apply transformations because the “dowload_sharepoint_file” function confuses the “,” separator in the CSV file with data such as scientific names
# convert everything to characters
zoo <- zoo |>
  dplyr::mutate(
    stage = as.character(stage),
    ind_m3 = as.character(ind_m3), # Convert stage and ind_m3 into characters for manipulation
    has_comma = grepl(",", ind_m3, fixed = TRUE), # Create a temporary column has_comma that is TRUE if ind_m3 contains a comma
    taxa = dplyr::case_when(
      grepl("[0-9]{4}", stage) ~ paste0(taxa, ", ", stage),
      TRUE ~ taxa
    ), # Reconstructs the name of the taxa: if stage contains 4 consecutive digits (probably data that ended up there by mistake), it adds it to taxa, otherwise it leaves taxa as it is
    stage = dplyr::case_when(
      has_comma ~ sub(",.*", "", ind_m3),
      TRUE ~ stage
    ), # Corrects stage: if there is a comma in ind_m3, it takes everything before the comma and puts it in stage, otherwise it leaves stage as it is
    ind_m3 = dplyr::case_when(
      has_comma ~ sub(".*,", "", ind_m3),
      TRUE ~ ind_m3
    ), # Correct ind_m3: if there is a comma, take everything after the comma otherwise, leave ind_m3 as it is
    stage = dplyr::na_if(stage, "#N/D"),
    ind_m3 = dplyr::na_if(ind_m3, "#N/D"), # Converts the text #N/D (Not Available, equivalent to #N/A in Excel) to NA (missing value in R)
    ind_m3 = as.numeric(ind_m3),
    date = lubridate::mdy(date) # Convert dates
  ) |>
  dplyr::select(-has_comma) # Removes the temporary column: has_comma

class(zoo)
head(zoo)
View(zoo)

#### ask to Lorenzo????? ####
dates <-
  zoo |>
  dplyr::select(-c(1:10), "dat_id")

# This code performs taxonomic validation:
# It takes the names of taxa from your data (which may contain errors, synonyms, old names)
# searches for them in the international WoRMS database
# obtains standardized and verified names
# keeps track of which taxa were not found.

# match taxa down to worms
reported_taxa <-
  as.character(unique(zoo$taxa)) # Extracts all unique values from the taxa column (removes duplicates) and converts them to characters (text strings).
reported_taxa

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
      error = function(e) NULL
    )
    if (
      # If any of these conditions are true → no match found
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
    }
    # If the match has been found
    res[[1]] %>% dplyr::mutate(original = taxon, .before = 1) # res[[1]]: Extracts the first search result
  } # mutate(original = taxon, .before = 1): adds the original column with the original name searched for as the first column
)

worms_matched
View(worms_matched)

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
    "stage"
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
  dplyr::rename(
    eventID = "sample_id",
    eventDate = "date",
    individualCount = "ind_m3",
    lifeStage = "stage"
  ) |>
  dplyr::distinct()

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
  dplyr::pull()


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


# export csv and parquet tidy files to hot storage bucket
formats <- c("parquet", "csv")

purrr::walk(formats, function(fmt) {
  filename <- paste0("McZoo_84-13.", fmt)

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
