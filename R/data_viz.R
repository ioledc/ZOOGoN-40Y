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
install.packages("viridis")
library(viridis)

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
