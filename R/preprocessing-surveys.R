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

  raw_surveys <-
    download_sharepoint_file(
      prefix = conf$ingestion$surveys$raw$file_prefix,
      options = conf$storage$sharepoint,
      bucket = conf$storage$sharepoint$aut_bucket,
      format = "csv"
    )

  cruise_info <-
    raw_surveys |>
    dplyr::select("submission_id", !dplyr::starts_with("group_taxa")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_cruise/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_environment/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_cruise/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_abundance/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_sample/"))

  taxa_info <-
    raw_surveys |>
    dplyr::select("submission_id", dplyr::starts_with("group_taxa")) |>
    reshape_kobo_repeat(group_name = "group_taxa") |>
    dplyr::mutate(
      taxon = dplyr::coalesce(
        .data$taxon_cope_sel,
        .data$taxon_noncope_sel,
        .data$other_taxon
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "taxon_cope_sel",
        "taxon_noncope_sel",
        "other_taxon"
      ))
    ) |>
    dplyr::relocate("taxon", .after = "n_sample")

  preprocessed_survey <-
    dplyr::full_join(cruise_info, taxa_info, by = "submission_id") |>
    janitor::clean_names()

  # process abundances

  upload_sharepoint_df(
    data = preprocessed_survey,
    prefix = conf$ingestion$surveys$preprocessed$file_prefix,
    options = conf$storage$sharepoint,
    bucket = conf$storage$sharepoint$aut_bucket,
    format = "csv"
  )
}


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
