dat <- readxl::read_xlsx(
  "/Users/lore/Downloads/Species_ID_2019.xlsx",
  .name_repair = "minimal"
) |>
  dplyr::mutate(dat_id = seq_len(dplyr::n())) |>
  janitor::clean_names()


ids <-
  readxl::read_xlsx(
    "/Users/lore/Downloads/ids_2019.xlsx",
    .name_repair = "minimal"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = lubridate::as_date(date),
    id = janitor::make_clean_names(id)
  )

merged <- dat |>
  tidyr::pivot_longer(
    cols = -c(dat_id, taxa, stage),
    names_to = "id",
    values_to = "ind_m3"
  ) |>
  dplyr::distinct() |>
  dplyr::full_join(ids, by = "id") |>
  dplyr::select(sample_id = "id", "date", "taxa", "stage", "ind_m3") |>
  dplyr::arrange(date, taxa)


taxaa <- worrms::wm_records_taxamatch(unique(merged$taxa)[20:60])


unique(merged$taxa)[1:5] |>
  purrr::map(worrms::wm_records_taxamatch)


test <-
  merged |>
  dplyr::filter(!is.na(ind_m3)) |>
  dplyr::select(date, taxa, ind_m3) |>
  dplyr::group_by(date, taxa) |>
  dplyr::summarise(ind_m3 = sum(ind_m3)) |>
  dplyr::ungroup()


our_taxa <- c(
  "Abylopsis tetragona (Otto, 1823)",
  "Aetideus armatus (Boeck, 1872)",
  "Amphipoda"
)

final_test <-
  test |>
  dplyr::filter(taxa %in% our_taxa)


library(ggplot2)

ggplot(
  final_test |> dplyr::filter(taxa == "Aetideus armatus (Boeck, 1872)"),
  aes(x = date, y = ind_m3, color = taxa)
) +
  geom_line()


plot_vis <- function(df, selected_taxa) {
  df_filtered <- df |>
    dplyr::filter(taxa == selected_taxa)

  ggplot(df_filtered, aes(x = date, y = ind_m3, color = taxa)) +
    geom_line()
}

plot_vis(df = final_test, selected_taxa = "Aetideus armatus (Boeck, 1872)")


purrr::map(our_taxa, plot_vis, df = final_test)
