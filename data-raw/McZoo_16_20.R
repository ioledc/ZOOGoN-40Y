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
    date = lubridate::mdy(.data$date)
  )

# Understanding the nature of the variable helped me get the code to work because the dates were of a “character” nature.
# date is "character" class, I must convert it directly from string to date
class(ids$date)
View(ids)

# get legacy data from legacy_data bucket from sharepoint, formatting data
zoo <- download_sharepoint_file(
  prefix = "merge_taxa_16_20.csv",
  options = conf$storage$sharepoint,
  bucket = "legacy_data",
  filename = TRUE
)

# I apply transformations because the “dowload_sharepoint_file” function confuses the “,” separator in the CSV file with data such as scientific names
# convert everything to characters
zoo <- zoo |>
  dplyr::mutate(
    stage = as.character(stage),
    ind_m3 = as.character(ind_m3),
    has_comma = grepl(",", ind_m3, fixed = TRUE),              # Check if ind_m3 contains a comma
    taxa = dplyr::case_when(
      grepl("[0-9]{4}", stage) ~ paste0(taxa, ", ", stage),
      TRUE ~ taxa
    ),                                                         # I reconstruct taxa
    stage = dplyr::case_when(
      has_comma ~ sub(",.*", "", ind_m3),
      TRUE ~ stage
    ),                                                         # I correct stage
    ind_m3 = dplyr::case_when(
      has_comma ~ sub(".*,", "", ind_m3),
      TRUE ~ ind_m3
    ),                                                         # I correct Ind_m3
    stage = dplyr::na_if(stage, "#N/D"),
    ind_m3 = dplyr::na_if(ind_m3, "#N/D"),
    ind_m3 = as.numeric(ind_m3),                               # I manage #N/A
    date = lubridate::mdy(date)                                # Convert dates
  ) |>
  dplyr::select(-has_comma)                                    

class(zoo)
head(zoo)
View(zoo)
zoo[7494:7504,]
###########################################################################################

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
