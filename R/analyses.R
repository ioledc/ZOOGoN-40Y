summaries <- function(data = NULL) {
  mean_sd_df <-
    data |>
    dplyr::mutate(year = lubridate::year(eventDate)) |>
    dplyr::group_by(year, scientificName) |>
    dplyr::summarise(
      mean_abundance = mean(individualCount),
      sd = sd(individualCount)
    ) |>
    dplyr::arrange(-mean_abundance)

  data |>
    dplyr::select(eventID, eventDate, scientificName, individualCount) |>
    tidyr::pivot_wider(
      names_from = scientificName,
      values_from = individualCount
    )
}
