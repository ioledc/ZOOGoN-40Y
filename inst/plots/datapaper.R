library(ggplot2)
library(patchwork)
library(scales)


conf <- read_config()
logger::log_info("Starting Darwin Core conversion...", namespace = "ZooGoN")

logger::log_info(
  "Downloading tidy data...",
  namespace = "ZooGoN"
)
merged_data <-
  download_sharepoint_file(
    prefix = conf$ingestion$tidy_data$file_prefix,
    options = conf$storage$sharepoint$credentials,
    bucket = conf$storage$sharepoint$buckets$automation_bucket,
    format = "parquet"
  )

classification_taxa_worms <- function(aphia_list = NULL) {
  worms_classified <- purrr::map2_dfr(
    .x = seq_along(aphia_list),
    .y = aphia_list,
    .f = function(i, taxon) {
      res <- tryCatch(
        worrms::wm_classification(taxon),
        error = function(e) NULL
      )
      if (is.null(res) || nrow(res) == 0) {
        return(dplyr::tibble(
          original = taxon,
          AphiaID = NA_integer_,
          rank = NA_character_,
          scientificname = NA_character_
        ))
      }

      res |> dplyr::mutate(original = taxon, .before = 1)
    }
  )

  worms_classified
}

df <-
  merged_data |>
  dplyr::filter(Abundance > 0) |>
  dplyr::mutate(
    year = lubridate::year(eventDate),
    week = lubridate::week(eventDate)
  ) |>
  dplyr::group_by(year, week) |>
  dplyr::summarise(
    tot_zoo = sum(Abundance),
    n_species = dplyr::n_distinct(lsid),
    .groups = "drop"
  ) |>
  dplyr::select(year, week, tot_zoo, n_species) |>
  janitor::clean_names()


# Plot 1: Abundance
p1 <- df |>
  ggplot(aes(x = week, y = year, fill = tot_zoo)) +
  geom_tile(color = "white") +
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    labels = label_comma()
  ) +
  scale_y_continuous(breaks = seq(1984, 2024, by = 4), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 53, by = 5), expand = c(0, 0)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "top",
    plot.margin = margin(5, 2, 5, 5) # Tighten right margin
  ) +
  labs(fill = expression(Abundance ~ (ind %.% m^-3)), y = "Year", x = NULL) + # x = NULL
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# Plot 2: N Species
p2 <- df |>
  ggplot(aes(x = week, y = year, fill = n_species)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  scale_y_continuous(breaks = seq(1984, 2024, by = 4), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 53, by = 5), expand = c(0, 0)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(5, 5, 5, 2) # Tighten left margin
  ) +
  labs(fill = "N. species", x = NULL) + # x = NULL
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# Combine and Center "Week"
combined <- p1 +
  p2 +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    caption = "Week",
    theme = theme(
      plot.caption = element_text(
        size = 12,
        hjust = 0.55,
        margin = margin(t = 5)
      )
    )
  )

combined
