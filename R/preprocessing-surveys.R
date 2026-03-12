#' Preprocess MC Surveys
#'
#' This function preprocesses raw MC survey data.
#' It performs data cleaning and transformation
#'
#' @param raw_data Data to preprocess.
#'
#' @return Preprocessed data
#'
#'
#' @keywords workflow preprocessing
#' @examples
#' \dontrun{
#' preprocess_surveys()
#' }
#' @export
preprocess_surveys <- function(raw_data = NULL) {
  conf <- read_config()

  logger::log_info("Downloading raw survey data...", namespace = "ZooGoN")
  raw_surveys <-
    download_sharepoint_file(
      prefix = conf$ingestion$surveys$raw$file_prefix,
      options = conf$storage$sharepoint$credentials,
      bucket = conf$storage$sharepoint$buckets$automation_bucket,
      format = "csv"
    ) |>
    # remove kobo metadata columns
    dplyr::select(
      -dplyr::starts_with("_"),
      -dplyr::starts_with("meta/"),
      -dplyr::all_of(c("formhub/uuid", "start", "end", "today"))
    )

  logger::log_debug("Raw surveys: {nrow(raw_surveys)} rows", namespace = "ZooGoN")

  logger::log_info("Extracting cruise info...", namespace = "ZooGoN")
  cruise_info <-
    raw_surveys |>
    dplyr::select("submission_id", !dplyr::starts_with("group_taxa")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_cruise/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_environment/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_cruise/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_abundance/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_sample/")) |>
    dplyr::rename_with(
  ~ ifelse(stringr::str_detect(., stringr::fixed("Filtered_Volume")), "filtered_volume_m3", .)
) |>
    #dplyr::rename(filtered_volume_m3 = "Filtered_Volume_m") |>
    dplyr::mutate(sampling_id = as.integer(.data$sampling_id)) |>
    #janitor::clean_names() |>
    dplyr::relocate(
      "filtered_volume_m3",
      .before = "water_column_sampled"
    ) |>
    # remove legacy columns
    dplyr::select(-"sampling_name")

  logger::log_info("Extracting taxa info...", namespace = "ZooGoN")
  taxa_info <-
    raw_surveys |>
    dplyr::select("submission_id", dplyr::starts_with("group_taxa")) |>
    reshape_kobo_repeat(group_name = "group_taxa") |>
    dplyr::mutate(
      taxon = dplyr::coalesce(
        .data$taxon_cope_sel,
        .data$taxon_noncope_sel
        # TO DO: fix missing other_taxon
        #.data$other_taxon
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "taxon_cope_sel",
        "taxon_noncope_sel"
        # TO DO: fix missing other_taxon
        #"other_taxon"
      ))
    ) |>
    dplyr::relocate("taxon", .after = "n_sample") |>
    # remove old duplicated labels
    dplyr::mutate(taxon = stringr::str_replace(.data$taxon, "_1", ""))

  preprocessed_survey <-
    dplyr::full_join(cruise_info, taxa_info, by = "submission_id") |>
    janitor::clean_names()

  # process abundances (ind_m3)
  preprocessed_complete <-
    preprocessed_survey |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(
          "n_male",
          "n_female",
          "n_copepodite",
          "n_undetermined",
          "n_larvae",
          "n_eggs"
        ),
        ~ as.numeric(.x) *
          (.data$total_volume / .data$subsample_volume) /
          .data$filtered_volume_m3
      )
    )

  clean_21_24 <-
    preprocessed_complete |>
    dplyr::select(
      "site_name",
      "sampling_id",
      "sampling_date",
      "taxon",
      dplyr::starts_with("n_")
    ) |>
    dplyr::mutate(eventID = paste0(.data$site_name, .data$sampling_id)) |>
    dplyr::relocate("eventID", .before = "site_name") |>
    dplyr::select(-c("site_name", "sampling_id")) |>
    dplyr::arrange(.data$sampling_date) |>
    dplyr::rename(
      eventDate = "sampling_date",
      aphiaID = "taxon"
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), as.numeric)) |>
    # TO DO: Ideally there should be no NAs unless the net was empty(!), to clarify. Meanwhile we drop all NAs
    dplyr::filter(!is.na(.data$aphia_id))

  logger::log_debug("Cleaned data: {nrow(clean_21_24)} rows", namespace = "ZooGoN")

  # Get unique AphiaID
  unique_aphia_ids <- as.numeric(unique(clean_21_24$aphia_id))
  logger::log_info("Querying WoRMS for {length(unique_aphia_ids)} unique AphiaIDs...", namespace = "ZooGoN")

  worms_records <-
    unique_aphia_ids |>
    purrr::map_dfr(worrms::wm_record) |>
    dplyr::select(aphia_id = "AphiaID", "scientificname", "lsid") |>
    dplyr::mutate(aphia_id = as.character(.data$aphia_id))

  # Data harmonization as other matrices
  tidy_data <-
    clean_21_24 |>
    dplyr::left_join(worms_records, by = "aphia_id") |>
    dplyr::relocate("scientificname", "lsid", .after = "aphia_id") |>
    # Pivot abundance columns to long format
    tidyr::pivot_longer(
  cols = dplyr::any_of(c(  # add to debug
    "n_male",
    "n_female",
    "n_copepodite",
    "n_undetermined",
    "n_larvae",
    "n_eggs",
    "n_nauplius"
  )),
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
    dplyr::filter(!is.na(.data$individual_count)) |>
    dplyr::rename(
      scientificName = "scientificname",
      eventID = "event_id",
      eventDate = "event_date",
      lifeStage = "life_stage",
      individualCount = "individual_count"
    ) |>
    dplyr::distinct() |>
    # remove NA counts & IDs
    dplyr::filter(
      !is.na(.data$individualCount),
      !is.na(.data$eventDate),
      !is.na(.data$eventID)
    ) |>
    # remove duplicates
    dplyr::group_by(
      .data$eventID,
      .data$eventDate,
      .data$scientificName,
      .data$lsid,
      .data$lifeStage
    ) |>
    dplyr::summarise(
      individualCount = sum(.data$individualCount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::relocate("individualCount", .before = "lifeStage")

  logger::log_info("Preprocessed data: {nrow(tidy_data)} rows, {length(unique(tidy_data$scientificName))} unique taxa", namespace = "ZooGoN")
  logger::log_info("Uploading preprocessed data (CSV + Parquet)...", namespace = "ZooGoN")
  c("csv", "parquet") |>
    purrr::walk(
      ~ upload_sharepoint_df(
        data = tidy_data,
        prefix = conf$ingestion$surveys$preprocessed$file_prefix,
        options = conf$storage$sharepoint$credentials,
        bucket = conf$storage$sharepoint$buckets$automation_bucket,
        format = .
      )
    )
  logger::log_success("preprocess_surveys complete", namespace = "ZooGoN")
  # upload_sharepoint_df(
  #   data = preprocessed_complete,
  #   prefix = conf$ingestion$survey$preprocessed$file_prefix,
  #   options = conf$storage$sharepoint$credentials,
  #   bucket = conf$storage$sharepoint$buckets$automation_bucket,
  #   format = "csv"
  # )
}

# TO DO:
# belongs to the function reinsert after Lorenzo's check
# upload_sharepoint_df(
# data = preprocessed_survey,
# prefix = conf$ingestion$surveys$preprocessed$file_prefix,
# options = conf$storage$sharepoint,
# bucket = conf$storage$sharepoint$aut_bucket,
# format = "csv"
# )
#}

#' Prepare repeat answers from Kobo survey forms
#'
#' Takes the repeating sample answers in MC survey exports (as used in
#' `preprocess_surveys()`) and lays them out so each repeat sits on its own
#' line, making the survey easier to read and join with other info.
#'
#' @details Technical: pivots columns starting with the repeat group prefix,
#' splits out the repeat number and field name, spreads them back to columns,
#' and orders the rows by the ID column and repeat number.
#'
#' @param data Downloaded Kobo survey data frame.
#' @param group_name Prefix of the repeat group to unfold (e.g. `"group_taxa"`).
#' @param id_col Column used to order results (default: `submission_id`).
#'
#' @return Data frame with one row per submission and repeat entry.
#' @export
reshape_kobo_repeat <- function(data, group_name, id_col = "submission_id") {
  data |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(group_name),
      names_to = c("n_sample", "variable"),
      names_pattern = paste0(group_name, "\\.(\\d+)\\.", group_name, "/(.+)"),
      values_to = "value",
      values_drop_na = TRUE,
      values_transform = as.character
    ) |>
    dplyr::mutate(n_sample = as.integer(.data$n_sample)) |>
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "value"
    ) |>
    dplyr::arrange(.data[[id_col]], .data$n_sample)
}
