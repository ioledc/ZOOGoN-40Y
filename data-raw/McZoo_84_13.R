## code to prepare zoo data (1984-2015)

# load configuration parameters
devtools::load_all()
conf <- read_config()

# get legacy data from legacy_data bucket
ids <-
  download_sharepoint_file(
    prefix = "ids_84_15.csv",
    options = conf$storage$sharepoint$credentials,
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
    options = conf$storage$sharepoint$credentials,
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


unmatched_fixed <-
  download_sharepoint_file(
    prefix = "unmatched_worms_84_15.xlsx",
    options = conf$storage$sharepoint$credentials,
    bucket = conf$storage$sharepoint$buckets$unmatched_bucket,
    filename = TRUE
  ) |>
  janitor::clean_names()

bio <-
  bio |>
  dplyr::left_join(unmatched_fixed, by = c("TAXA" = "reported_taxa")) |>
  dplyr::mutate(
    TAXA = dplyr::case_when(
      TAXA = !is.na(accepted_scientific_name) ~ accepted_scientific_name,
      TRUE ~ TAXA
    ),
    stage = dplyr::case_when(!is.na(lifestage) ~ lifestage, TRUE ~ stage)
  ) |>
  dplyr::select(-c(accepted_scientific_name, aphia_id, lifestage))

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
      .data$lifeStage == "larvae" ~ "lar",
      .data$lifeStage == "eggs" ~ "egg",
      TRUE ~ .data$lifeStage
    )
  ) |>
  # remove NA counts
  dplyr::filter(!is.na(.data$individualCount))

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
    data = tidy_data,
    prefix = filename,
    bucket = conf$storage$sharepoint$buckets$hot_bucket,
    options = conf$storage$sharepoint$credentials,
    format = fmt,
    filename = TRUE
  )
})