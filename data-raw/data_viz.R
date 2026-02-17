# test plot
# load all packages and configurations
devtools::load_all()
conf <- read_config()

# require needed packages
library(magrittr)
library(ggplot2)
library(dplyr)
library(scales)
library(purrr)
library(ggsci)
library(tidyr)
library(readr)
library(lubridate)
library(tidyverse)
#install.packages("viridis")
library(viridis)
#install.packages("cowplot")
library(cowplot)

# load all developed matrices
# 84-15
legacy_84_15 <-
  download_sharepoint_file(
    prefix = "McZoo_84-15.parquet",
    options = conf$storage$sharepoint$credentials,
    bucket = conf$storage$sharepoint$buckets$hot_bucket,
    filename = TRUE
  )
# 16-20
legacy_16_20 <-
  download_sharepoint_file(
    prefix = "McZoo_16-20.parquet",
    options = conf$storage$sharepoint$credentials,
    bucket = conf$storage$sharepoint$buckets$hot_bucket,
    filename = TRUE
  )
# 21-24
legacy_21_24 <-
  download_sharepoint_file(
    prefix = "McZoo_21-24.parquet",
    options = conf$storage$sharepoint$credentials,
    bucket = conf$storage$sharepoint$buckets$hot_bucket,
    filename = TRUE
  )

# merge into a single databse
legacy_84_24 <-
  dplyr::bind_rows(legacy_84_15, legacy_16_20, legacy_21_24)

# data visualization on Acartia calusi and Temora stylifera
# select only the variables of interest
zoo_data <-
  legacy_84_24 |>
  dplyr::filter(scientificName %in% c("Acartia clausi", "Temora stylifera")) |>
  dplyr::select(eventDate, scientificName, individualCount, lifeStage)

# heatmap
# Prepare data
zoo_heatmap <-
  zoo_data |>
  dplyr::mutate(
    week = lubridate::week(eventDate), # Extract week (1-52)
    year = lubridate::year(eventDate) # Extract year
  ) |>
  dplyr::group_by(year, week, scientificName, lifeStage) |> # Group by year, week, species and life stage
  dplyr::summarise(
    abundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  ) |> # Sum abundances
  dplyr::mutate(log_abundance = log(abundance + 1)) # Log-transformation (+1 to avoid log(0))


# Function to create heatmap for a single species
plot_species_heatmap <- function(species_name) {
  data <-
    zoo_heatmap |>
    dplyr::filter(scientificName == species_name) # Filter by selected species

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = week, y = year, fill = log_abundance)
  ) +
    ggplot2::geom_tile(color = "grey90") + # Tiles with grey border
    ggplot2::facet_wrap(~lifeStage, ncol = 3) + # Split by life stage (f, m, j)
    ggplot2::scale_fill_gradientn(
      # Custom color gradient
      colours = c("blue", "cyan", "green", "yellow", "orange", "red"),
      na.value = "grey90",
      name = expression(ind ~ m^-3)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, by = 4)) + # Week labels
    ggplot2::scale_y_continuous(breaks = seq(1984, 2024, by = 1)) + # All years from 1984 to 2024
    ggplot2::coord_cartesian(expand = FALSE) + # Remove empty space
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = bquote(italic(.(species_name))), # Title in italic
      x = "Weeks",
      y = "Years"
    ) +
    ggplot2::theme(
      legend.position = "right", # Legend on the right
      panel.grid.major = ggplot2::element_line(
        color = "grey80",
        linewidth = 0.3
      ), # Major grid lines
      panel.grid.minor = ggplot2::element_blank(), # Remove minor grid lines
      strip.text = ggplot2::element_text(face = "bold", size = 18), # Life stage labels bold and larger
      axis.text.x = ggplot2::element_text(size = 16), # X axis text larger
      axis.text.y = ggplot2::element_text(size = 16), # Y axis text larger
      axis.title.x = ggplot2::element_text(size = 18, face = "bold"), # X axis title larger and bold
      axis.title.y = ggplot2::element_text(size = 18, face = "bold"), # Y axis title larger and bold
      plot.title = ggplot2::element_text(size = 18, face = "bold.italic"), # Plot title larger, bold and italic
      legend.title = ggplot2::element_text(size = 18, face = "bold"), # Legend title larger and bold
      legend.text = ggplot2::element_text(size = 16) # Legend text larger
    )
}

# create heatmaps for both species
heatmap_acartia <- plot_species_heatmap("Acartia clausi")
heatmap_temora <- plot_species_heatmap("Temora stylifera")
# display
heatmap_acartia
heatmap_temora

# TO DO: improve the plot's quality, legend ecc
# heatmap with more graphics # TO DO: improve the plot's quality
# Function to create complex heatmap for a single species with life stage separation
plot_species_heatmap_complex <- function(species_name, log = TRUE) {
  # Filter data for selected species
  data <- zoo_heatmap |>
    dplyr::filter(scientificName == species_name)

  # Legend text based on log parameter
  legend_text <- ifelse(
    isTRUE(log),
    expression(log(ind ~ m^-3)),
    expression(ind ~ m^-3)
  )

  # Create list to store plots for each life stage
  combined_plots <- list()

  # Get unique life stages
  life_stages <- unique(data$lifeStage)

  # Loop through each life stage
  for (stage in life_stages) {
    # Filter data for current life stage
    stage_data <- data |>
      dplyr::filter(lifeStage == stage) |>
      dplyr::mutate(
        abundance_plot = dplyr::case_when(
          isTRUE(log) ~ log_abundance,
          TRUE ~ abundance
        )
      )

    # Main heatmap
    heatmap_plot <- stage_data |>
      ggplot2::ggplot(ggplot2::aes(x = week, y = year, fill = abundance_plot)) +
      ggplot2::geom_tile(color = "grey90") +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_gradientn(
        colours = c("blue", "cyan", "green", "yellow", "orange", "red"),
        na.value = "grey90"
      ) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = seq(1, 52, by = 4)) +
      ggplot2::scale_y_continuous(breaks = seq(1984, 2024, by = 2)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.3, "cm"),
        legend.title = ggplot2::element_text(size = 10),
        legend.box.just = "left"
      ) +
      ggplot2::labs(
        x = "Weeks",
        y = "Years",
        fill = legend_text
      )

    # Marginal line plot (top)
    line <- stage_data |>
      dplyr::group_by(week) |>
      dplyr::summarise(abundance_plot = mean(abundance_plot, na.rm = TRUE))

    # Calcola min e max considerando TUTTI i valori (anche 0)
    minmax <- line |>
      dplyr::mutate(
        label_row = dplyr::case_when(
          abundance_plot == min(abundance_plot, na.rm = TRUE) ~ "min",
          abundance_plot == max(abundance_plot, na.rm = TRUE) ~ "max",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(label_row)) |>
      dplyr::arrange(abundance_plot)

    line_plot <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::stat_smooth(
        data = line,
        ggplot2::aes(week, abundance_plot),
        method = "loess",
        color = "#d64d4d",
        se = FALSE,
        span = 0.25,
        linewidth = 0.5
      ) +
      ggplot2::geom_area(
        data = line,
        ggplot2::aes(week, abundance_plot),
        fill = "grey90",
        color = "transparent"
      ) +
      ggplot2::geom_point(
        data = line,
        ggplot2::aes(week, abundance_plot, color = abundance_plot)
      ) +
      ggplot2::annotate(
        "text",
        x = minmax$week[1],
        y = minmax$abundance_plot[1],
        label = round(minmax$abundance_plot[1], 1),
        vjust = -1,
        color = "firebrick",
        size = 3
      ) +
      ggplot2::annotate(
        "text",
        x = minmax$week[nrow(minmax)],
        y = minmax$abundance_plot[nrow(minmax)],
        label = round(minmax$abundance_plot[nrow(minmax)], 1),
        vjust = +1,
        hjust = 1.5,
        color = "firebrick",
        size = 3
      ) +
      ggplot2::annotate(
        "point",
        x = minmax$week[1],
        y = minmax$abundance_plot[1],
        color = "firebrick"
      ) +
      ggplot2::annotate(
        "point",
        x = minmax$week[nrow(minmax)],
        y = minmax$abundance_plot[nrow(minmax)],
        color = "firebrick"
      ) +
      ggplot2::scale_color_gradientn(
        colours = c("blue", "cyan", "green", "yellow", "orange", "red")
      ) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = margin(t = 5, b = 0, l = 48, r = 50)
      ) +
      ggplot2::labs(subtitle = paste("Life stage:", stage))

    # Side panel (anomalies)
    side_tile <- stage_data |>
      dplyr::group_by(year) |>
      dplyr::summarise(abundance_plot = mean(abundance_plot, na.rm = TRUE)) |>
      dplyr::mutate(
        mean_abundance = mean(abundance_plot, na.rm = TRUE),
        mean_anomaly = (abundance_plot - mean_abundance) / mean_abundance * 100,
        mean_anomaly_scale = dplyr::case_when(
          mean_anomaly < -100 ~ -100,
          mean_anomaly > 100 ~ 100,
          TRUE ~ mean_anomaly
        )
      )

    side_plot <- side_tile |>
      dplyr::mutate(dummy = "group") |>
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        ggplot2::aes(x = dummy, y = year, fill = mean_anomaly_scale),
        alpha = 0.6,
        color = "grey90"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = dummy,
          y = year,
          label = ifelse(
            !is.na(mean_anomaly),
            paste0(round(mean_anomaly, 0), "%"),
            "-"
          )
        ),
        size = 2.5,
        color = "grey30"
      ) +
      ggplot2::theme_void() +
      ggplot2::scale_fill_gradient2(
        low = "#56a8a9",
        mid = "white",
        high = "#a95756",
        midpoint = 0,
        na.value = "white"
      ) +
      ggplot2::scale_y_continuous(breaks = seq(1984, 2024, by = 2)) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(fill = "Mean difference (%)") +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = margin(t = 0, r = +12, b = 0, l = -3.5),
        legend.key.width = unit(0.5, "cm"),
        legend.key.size = unit(0.25, "cm"),
        legend.title = ggplot2::element_text(size = 10),
        legend.direction = "horizontal",
        legend.box.just = "left"
      )

    # Combine heatmap and side plot
    p1 <- cowplot::plot_grid(
      heatmap_plot,
      side_plot,
      ncol = 2,
      rel_widths = c(12, 1),
      align = "h"
    )

    # Combine with top line plot
    combined <- cowplot::plot_grid(
      line_plot,
      p1,
      nrow = 2,
      rel_heights = c(1, 5)
    )

    combined_plots[[stage]] <- combined
  }

  # Combine all life stages HORIZONTALLY
  final_plot <- cowplot::plot_grid(
    plotlist = combined_plots,
    ncol = length(life_stages),
    labels = NULL
  )

  # Add overall title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      bquote(italic(.(species_name))),
      fontface = 'bold.italic',
      size = 18,
      x = 0,
      hjust = 0
    ) +
    ggplot2::theme(
      plot.margin = margin(0, 0, 0, 7)
    )

  cowplot::plot_grid(
    title,
    final_plot,
    ncol = 1,
    rel_heights = c(0.05, 1)
  )
}

# Esempio di utilizzo:
plot_species_heatmap_complex(species_name = "Acartia clausi", log = TRUE)

# time series - trend Ab
# Function to create temporal trend plot
plot_temporal_trend <- function(species_name) {
  # Filter data by species
  data <-
    zoo_data |>
    dplyr::filter(scientificName == species_name) # Filter by selected species

  # Aggregate abundance by sampling date (all life stages combined)
  data_aggregated <-
    data |>
    dplyr::mutate(year = lubridate::year(eventDate)) |> # Extract year
    dplyr::group_by(eventDate, year) |> # Group by date and year
    dplyr::summarise(
      abundance = sum(individualCount, na.rm = TRUE),
      .groups = "drop"
    ) |> # Sum all life stages
    dplyr::arrange(eventDate) # Sort by date

  # Define separate periods
  period1 <- data_aggregated |> dplyr::filter(year >= 1984 & year <= 1990) # First period
  period2 <- data_aggregated |> dplyr::filter(year >= 1995 & year <= 2024) # Second period

  # Linear models for each period independently
  lm1 <- lm(abundance ~ eventDate, data = period1) # Linear model period 1
  lm2 <- lm(abundance ~ eventDate, data = period2) # Linear model period 2

  coef1 <- summary(lm1)$coefficients["eventDate", "Estimate"] # Coefficient period 1
  pval1 <- summary(lm1)$coefficients["eventDate", "Pr(>|t|)"] # P-value period 1
  coef2 <- summary(lm2)$coefficients["eventDate", "Estimate"] # Coefficient period 2
  pval2 <- summary(lm2)$coefficients["eventDate", "Pr(>|t|)"] # P-value period 2

  # Caption text with trend test results
  caption_text <- paste0(
    "Trend test 1984-1990: coef = ",
    round(coef1, 4),
    ", p-value = ",
    signif(pval1, 3),
    "\n",
    "Trend test 1995-2024: coef = ",
    round(coef2, 4),
    ", p-value = ",
    signif(pval2, 3),
    "\nSignificance level α = 0.05"
  )

  # X axis breaks every 2 years
  breaks_period1 <- seq(
    as.Date("1984-01-01"),
    as.Date("1990-12-31"),
    by = "2 years"
  ) # Breaks for period 1
  breaks_period2 <- seq(
    as.Date("1996-01-01"),
    as.Date("2024-12-31"),
    by = "2 years"
  ) # Breaks for period 2
  x_breaks <- c(breaks_period1, breaks_period2) # Combine breaks

  # Create plot
  ggplot2::ggplot() +

    # Vertical spikes (grey background)
    ggplot2::geom_segment(
      data = data_aggregated,
      ggplot2::aes(x = eventDate, xend = eventDate, y = 0, yend = abundance),
      color = "grey70",
      alpha = 0.4
    ) +

    # Blue regression lines - SEPARATE for each period
    ggplot2::geom_smooth(
      data = period1,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "lm", # Linear regression
      se = FALSE, # No confidence interval
      color = "blue",
      linewidth = 1.2
    ) +

    ggplot2::geom_smooth(
      data = period2,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "lm", # Linear regression
      se = FALSE, # No confidence interval
      color = "blue",
      linewidth = 1.2
    ) +

    # Red local polynomial regression - SEPARATE for each period
    ggplot2::geom_smooth(
      data = period1,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "loess", # Local polynomial regression
      se = FALSE, # No confidence interval
      color = "red",
      linewidth = 1.2
    ) +

    ggplot2::geom_smooth(
      data = period2,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "loess", # Local polynomial regression
      se = FALSE, # No confidence interval
      color = "red",
      linewidth = 1.2
    ) +

    # X axis: every 2 years
    ggplot2::scale_x_date(breaks = x_breaks, date_labels = "%Y") +

    # Y axis: 0 to 3000 with breaks at 0, 1500, 3000
    ggplot2::scale_y_continuous(
      limits = c(0, 3000),
      breaks = c(0, 1500, 3000)
    ) +

    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = bquote(italic(.(species_name))), # Title in italic
      x = "Years",
      y = expression(ind ~ m^-3), # Y axis label
      caption = caption_text # Caption with test results
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, face = "bold.italic"), # Title style
      axis.title.x = ggplot2::element_text(size = 16, face = "bold"), # X axis title
      axis.title.y = ggplot2::element_text(size = 16, face = "bold"), # Y axis title
      axis.text = ggplot2::element_text(size = 14), # Axis text
      plot.caption = ggplot2::element_text(
        size = 12,
        hjust = 0,
        face = "italic"
      ), # Caption style
      panel.grid.major = ggplot2::element_line(color = "grey90") # Grid lines
    )
}

# Create plots for both species
trend_acartia <- plot_temporal_trend("Acartia clausi")
trend_temora <- plot_temporal_trend("Temora stylifera")

# Display
trend_acartia
trend_temora

# seasonality
# Function to create annual boxplot
plot_annual_boxplot <- function(species_name) {
  data <-
    zoo_data |>
    dplyr::filter(scientificName == species_name) |> # Filter by species
    dplyr::mutate(year = lubridate::year(eventDate)) |> # Extract year
    dplyr::group_by(year, eventDate) |> # Group by year and date
    dplyr::summarise(
      abundance = sum(individualCount, na.rm = TRUE),
      .groups = "drop"
    ) # Sum abundances

  # Create a complete sequence of years from 1984 to 2024
  all_years <- data.frame(year = 1984:2024) # Complete year sequence

  # Join with data to preserve the gap
  data_complete <-
    all_years |>
    dplyr::left_join(data, by = "year") # Join to show gap (1991-1994)

  ggplot2::ggplot(
    data_complete,
    ggplot2::aes(x = abundance, y = factor(year))
  ) + # Horizontal boxplots
    ggplot2::geom_boxplot(
      fill = "white",
      color = "black",
      outlier.color = "red",
      outlier.shape = 16,
      na.rm = TRUE
    ) + # White boxplots with red outliers
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = bquote(italic(.(species_name))), # Title in italic
      x = expression(ind ~ m^-3), # X axis label
      y = "Year" # Y axis label
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold.italic"), # Title style
      axis.title = ggplot2::element_text(size = 14, face = "bold"), # Axis titles
      axis.text.x = ggplot2::element_text(size = 12), # X axis text
      axis.text.y = ggplot2::element_text(size = 10), # Y axis text (years)
      panel.grid.major.y = ggplot2::element_blank() # Remove horizontal grid
    )
}

# Test
annual_acartia <- plot_annual_boxplot("Acartia clausi")
annual_acartia


# Function to create monthly boxplot
plot_monthly_boxplot <- function(species_name) {
  data <-
    zoo_data |>
    dplyr::filter(scientificName == species_name) |> # Filter by species
    dplyr::mutate(
      month = lubridate::month(eventDate, label = TRUE, abbr = TRUE)
    ) |> # Extract month names
    dplyr::group_by(month, eventDate) |> # Group by month and date
    dplyr::summarise(
      abundance = sum(individualCount, na.rm = TRUE),
      .groups = "drop"
    ) # Sum abundances

  ggplot2::ggplot(data, ggplot2::aes(x = month, y = abundance)) +
    ggplot2::geom_boxplot(
      fill = "white",
      color = "black",
      outlier.color = "red",
      outlier.shape = 16
    ) + # White boxplots with red outliers
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = bquote(italic(.(species_name))), # Title in italic
      x = "Month",
      y = expression(ind ~ m^-3) # Y axis label
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold.italic"), # Title style
      axis.title = ggplot2::element_text(size = 14, face = "bold"), # Axis titles
      axis.text = ggplot2::element_text(size = 12) # Axis text
    )
}

# Test
monthly_acartia <- plot_monthly_boxplot("Acartia clausi")
monthly_acartia

# Function to create weekly boxplot
plot_weekly_boxplot <- function(species_name) {
  data <-
    zoo_data |>
    dplyr::filter(scientificName == species_name) |> # Filter by species
    dplyr::mutate(week = lubridate::week(eventDate)) |> # Extract week (1-52)
    dplyr::group_by(week, eventDate) |> # Group by week and date
    dplyr::summarise(
      abundance = sum(individualCount, na.rm = TRUE),
      .groups = "drop"
    ) # Sum abundances

  ggplot2::ggplot(data, ggplot2::aes(x = factor(week), y = abundance)) +
    ggplot2::geom_boxplot(
      fill = "white",
      color = "black",
      outlier.color = "red",
      outlier.shape = 16
    ) + # White boxplots with red outliers
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = bquote(italic(.(species_name))), # Title in italic
      x = "Week of the year",
      y = expression(ind ~ m^-3) # Y axis label
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold.italic"), # Title style
      axis.title = ggplot2::element_text(size = 14, face = "bold"), # Axis titles
      axis.text.x = ggplot2::element_text(size = 8), # X axis text smaller
      axis.text.y = ggplot2::element_text(size = 12) # Y axis text
    )
}
