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

  cruise_info <-
    raw_data |>
    dplyr::select("submission_id", !dplyr::starts_with("group_taxa")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_cruise/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_environment/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_cruise/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_abundance/")) |>
    dplyr::rename_with(~ stringr::str_remove(., "group_sample/"))

  taxa_info <-
    raw_data |>
    dplyr::select("submission_id", dplyr::starts_with("group_taxa")) |>
    reshape_kobo_repeat(group_name = "group_taxa")

  tidy_survey <-
    dplyr::full_join(cruise_info, taxa_info, by = "submission_id") |>
    janitor::clean_names()
}
reshape_kobo_repeat <- function(
  data = NULL,
  group_name = NULL,
  id_col = "submission_id"
) {
  data |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(group_name),
      names_to = c("n_sample", "variable"),
      names_pattern = paste0(group_name, "\\.(\\d+)\\.", group_name, "/(.+)"),
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(n_sample = as.integer(n_sample)) |>
    tidyr::pivot_wider(
      names_from = variable,
      values_from = value
    ) |>
    dplyr::arrange(!!rlang::sym(id_col), n_sample) |>
    dplyr::select(-c("is_copepod", "order_group", "class_group")) |>
    dplyr::mutate(
      aphiaid = dplyr::coalesce(
        valid_name_cope,
        valid_name_choice,
        valid_name_noncope
      )
    ) |>
    dplyr::select(-c(dplyr::starts_with("valid_name")))
}
