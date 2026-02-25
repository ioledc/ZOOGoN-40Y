# test plot
# load all packages and configurations
devtools::load_all()
conf <- read_config()

# require needed packages
pacman::p_load(
  magrittr,
  scales,
  ggsci,
  lubridate,
  tidyverse,
  viridis,
  cowplot,
  worrms,
  stringr,
  vegan,
  reader,
  readr,
  readxl,
  stringr
)

# TO DO: automate load from sharepoint, zoogon_mc-dev folder contain "ranks" files with complete taxonomy for each taxon
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

# add Class and Order from Worms
# extrac AphiaID to lsid
df <-
  legacy_84_24 |>
  dplyr::mutate(AphiaID = as.integer(gsub(".*:(\\d+)$", "\\1", lsid)))

# unique id
unique_ids <- unique(df$AphiaID)

# recover taxonomy
get_class_order <- function(id) {
  tryCatch(
    {
      cls <- wm_classification(id)
      # extract scientificname where rank matches, return NA if not found
      class_val <- cls$scientificname[cls$rank == "Class"]
      order_val <- cls$scientificname[cls$rank == "Order"]
      data.frame(
        AphiaID = id,
        class = if (length(class_val) > 0) class_val[1] else NA_character_,
        order = if (length(order_val) > 0) order_val[1] else NA_character_,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      data.frame(
        AphiaID = id,
        class = NA_character_,
        order = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  )
}

ranks_df <- bind_rows(lapply(unique_ids, get_class_order))

# merge
zoo_84_24 <-
  df |>
  dplyr::left_join(ranks_df, by = "AphiaID") |>
  dplyr::select(-c(isCopepod, AphiaID)) |>
  dplyr::relocate(class, order, .before = scientificName) |>
  dplyr::rename(rankCalss = class, rankOrder = order)

# at the end export in parquet files to generate data viz report
# setwd("C:/Users/andre/OneDrive/Desktop/Data Visualization")
# arrow::write_parquet(zoo_84_24, "data_viz_zoo.parquet")

# data organization
zoo_heatmap <-
  zoo_84_24 |>
  dplyr::mutate(
    week = lubridate::week(eventDate), # Extract week (1-52)
    year = lubridate::year(eventDate) # Extract year
  ) |>
  dplyr::group_by(year, week, rankCalss, rankOrder, scientificName) |> # Group by year, week, species and life stage
  dplyr::summarise(
    abundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  ) |> # Sum abundances
  dplyr::mutate(log_abundance = log(abundance + 1)) # Log-transformation (+1 to avoid log(0))

### heatmap function
plot_heatmap_complex <- function(
  species_name = NULL,
  rank_class = NULL,
  rank_order = NULL,
  log = TRUE
) {
  # apply taxonomic filters first (if provided), then aggregate or filter by species
  data <-
    zoo_heatmap |>
    dplyr::filter(
      if (!is.null(rank_class)) rankCalss == rank_class else TRUE, # filter by class if provided
      if (!is.null(rank_order)) rankOrder == rank_order else TRUE, # filter by order if provided
      if (!is.null(species_name) && species_name != "All") {
        scientificName == species_name
      } else {
        TRUE
      } # filter by species if provided
    )

  # aggregate all species if species_name is NULL or "All"
  if (is.null(species_name) || species_name == "All") {
    data <-
      data |>
      dplyr::group_by(year, week) |>
      dplyr::summarise(
        abundance = sum(abundance, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        abundance_plot = if (log) log(abundance + 1) else abundance
      )
  } else {
    # filter by specific species
    data <-
      data |>
      dplyr::mutate(
        abundance_plot = if (log) log_abundance else abundance
      )
  }

  # build dynamic title based on what is being plotted
  plot_title <- if (!is.null(species_name) && species_name != "All") {
    species_name
  } else if (!is.null(rank_order)) {
    rank_order
  } else if (!is.null(rank_class)) {
    rank_class
  } else {
    "All Zooplankton"
  }

  # legend text and theme
  legend_text <- expression(bold(ind ~ m^-3))

  legend_theme <- ggplot2::theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.direction = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.title = ggplot2::element_text(size = 16, face = "bold"),
    legend.text = ggplot2::element_text(size = 16, face = "bold")
  )

  # dynamic breaks: visually centered on pseudo-log scale regardless of data range
  min_val <- min(data$abundance_plot[data$abundance_plot > 0], na.rm = TRUE)
  max_val <- max(data$abundance_plot, na.rm = TRUE)

  # compute midpoint on the transformed scale, then back-transform
  sigma <- 5
  mid_transformed <- (asinh(min_val / sigma) + asinh(max_val / sigma)) / 2
  mid_val <- round(sinh(mid_transformed) * sigma) # back-transform to original scale

  breaks_vals <- c(0, mid_val, round(max_val))

  # main heatmap: weeks (x) vs years (y), colored by abundance
  heatmap_plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(x = week, y = year, fill = abundance_plot)) +
    ggplot2::geom_tile(color = "grey90") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradientn(
      colours = c("blue", "cyan", "green", "yellow", "orange", "red"),
      trans = scales::pseudo_log_trans(sigma = 5),
      breaks = breaks_vals, # dynamic breaks adapted to the plotted data
      labels = scales::label_number(),
      na.value = "grey90"
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, by = 5)) +
    ggplot2::scale_y_continuous(breaks = seq(1984, 2024, by = 4)) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.box.just = "left",
      legend.box.margin = margin(t = 0, r = 350, b = 0, l = -200),
      axis.text.x = ggplot2::element_text(size = 18, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 18, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 18, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 18, face = "bold"),
      plot.title = ggplot2::element_text(size = 18, face = "bold")
    ) +
    legend_theme +
    ggplot2::labs(
      x = "Weeks",
      y = "Years",
      fill = legend_text
    ) +
    guides(alpha = "none")

  # top marginal plot: mean abundance by week across all years
  line <-
    data |>
    dplyr::group_by(week) |>
    dplyr::summarise(abundance_plot = mean(abundance_plot, na.rm = TRUE))

  # identify min and max values for annotation
  minmax <-
    line |>
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
      fill = "grey95",
      color = "grey70"
    ) +
    ggplot2::geom_point(
      data = line,
      ggplot2::aes(week, abundance_plot, color = abundance_plot)
    ) +
    ggplot2::geom_text(
      data = minmax,
      ggplot2::aes(
        x = week,
        y = abundance_plot,
        label = round(abundance_plot, 1),
        vjust = ifelse(label_row == "max", 1.5, -0.5)
      ),
      color = "firebrick",
      size = 6
    ) +
    ggplot2::geom_point(
      data = minmax,
      ggplot2::aes(x = week, y = abundance_plot),
      color = "firebrick"
    ) +
    ggplot2::scale_color_gradientn(
      colours = c("blue", "cyan", "green", "yellow", "orange", "red")
    ) +
    ggplot2::scale_x_continuous(limits = c(1, 52), expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(1, 52), expand = FALSE, clip = "off") +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = margin(t = 15, b = 5, l = 70, r = 69)
    )

  # right side panel: annual anomalies as % deviation from mean
  side_tile <-
    data |>
    dplyr::group_by(year) |>
    dplyr::summarise(abundance_plot = mean(abundance_plot, na.rm = TRUE)) |>
    dplyr::mutate(
      mean_abundance = mean(abundance_plot, na.rm = TRUE),
      mean_anomaly = (abundance_plot - mean_abundance) / mean_abundance * 100,
      mean_anomaly_scale = pmax(pmin(mean_anomaly, 100), -100)
    )

  side_plot <-
    side_tile |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(x = "group", y = year, fill = mean_anomaly_scale),
      alpha = 0.6,
      color = "grey90"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = "group",
        y = year,
        label = ifelse(
          !is.na(mean_anomaly),
          paste0(round(mean_anomaly, 0), "%"),
          "-"
        )
      ),
      size = 4,
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
    ggplot2::scale_y_continuous(breaks = seq(1984, 2024, by = 4)) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(fill = "Mean difference (%)") +
    ggplot2::theme(
      plot.margin = margin(t = 0, r = 12, b = 0, l = -3.5),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.box.just = "right",
      legend.box.margin = margin(t = 0, r = 200, b = 0, l = -100)
    ) +
    legend_theme +
    guides(alpha = "none")

  # combine panels
  p1 <- cowplot::plot_grid(
    heatmap_plot,
    side_plot,
    ncol = 2,
    rel_widths = c(12, 1),
    align = "h"
  )

  combined <- cowplot::plot_grid(line_plot, p1, nrow = 2, rel_heights = c(1, 5))

  # add title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      bquote(italic(.(plot_title))), # dynamic title based on what is being plotted
      fontface = 'bold',
      size = 20,
      x = 0,
      hjust = 0
    ) +
    ggplot2::theme(
      plot.margin = margin(0, 0, 0, 7)
    )

  # return final plot
  cowplot::plot_grid(
    title,
    combined,
    ncol = 1,
    rel_heights = c(0.05, 1)
  )
}

# dispaly
plot_heatmap_complex(species_name = "All", log = F)
plot_heatmap_complex(species_name = "Temora stylifera", log = F)
plot_heatmap_complex(rank_class = "Sagittoidea", log = F)
plot_heatmap_complex(rank_class = "Copepoda", log = F)
plot_heatmap_complex(rank_order = "Onychopoda", log = F)

### trend function
plot_temporal_trend <- function(species_name = NULL, rank_class = NULL, rank_order = NULL) {

  # apply taxonomic filters first (if provided)
  zoo_filtered <-
    zoo_84_24 |>
    dplyr::filter(
      if (!is.null(rank_class)) rankCalss == rank_class else TRUE,   # filter by class if provided
      if (!is.null(rank_order)) rankOrder == rank_order else TRUE,   # filter by order if provided
      if (!is.null(species_name) && species_name != "All") scientificName == species_name else TRUE  # filter by species if provided
    )

  # aggregate all species if species_name is NULL or "All"
  if (is.null(species_name) || species_name == "All") {
    filtered_data <-
      zoo_filtered |>
      dplyr::group_by(eventDate) |>
      dplyr::summarise(
        abundance = sum(individualCount, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    filtered_data <-
      zoo_filtered |>
      dplyr::filter(scientificName == species_name) |>
      dplyr::group_by(eventDate) |>
      dplyr::summarise(
        abundance = sum(individualCount, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # build dynamic title based on what is being plotted
  plot_title <- if (!is.null(species_name) && species_name != "All") {
    species_name
  } else if (!is.null(rank_order)) {
    rank_order
  } else if (!is.null(rank_class)) {
    rank_class
  } else {
    "All Zooplankton"
  }

  # calculate deseasonalized anomalies with quarterly stratification
  data_df <-
    filtered_data |>
    dplyr::mutate(
      month = lubridate::month(eventDate),
      quarter = lubridate::quarter(eventDate), # Q1-Q4 for seasonal analysis
      year = lubridate::year(eventDate),
      month_date = lubridate::floor_date(eventDate, "month"),
      series = ifelse(year < 1993, "before", "after") # temporal split
    ) |>
    dplyr::group_by(series, month) |>
    dplyr::mutate(
      mean_abundance = mean(abundance),
      anomaly = abundance - mean_abundance # remove seasonal cycle
    ) |>
    dplyr::group_by(series, month_date) |>
    dplyr::summarise(
      quarter = dplyr::first(quarter),
      month = dplyr::first(month),
      anomaly = mean(anomaly),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      quarter = as.character(quarter)
    ) |>
    dplyr::ungroup()

  # split into before/after periods
  data_df <- split(data_df, data_df$series)

  # linear regression by quarter (post-1993 only)
  trend_res <-
    data_df$after |>
    dplyr::group_by(quarter) |>
    dplyr::do(broom::tidy(lm(anomaly ~ month_date, data = .))) |>
    dplyr::filter(term == "month_date") |>
    dplyr::select(quarter, estimate, p.value)

  # format statistical results as text annotations
  annotations_text <-
    trend_res |>
    dplyr::arrange(quarter) |>
    dplyr::mutate(
      label = sprintf("Q%s: %.4f, p=%.4f", quarter, estimate, p.value)
    ) |>
    dplyr::pull(label) |>
    paste(collapse = "\n")

  # time series plot with quarterly color coding
  ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    # points for "before" period (pre-1993)
    ggplot2::geom_point(
      data = data_df$before,
      ggplot2::aes(month_date, anomaly, color = quarter, alpha = abs(anomaly))
    ) +
    # points for "after" period (post-1993)
    ggplot2::geom_point(
      data = data_df$after,
      ggplot2::aes(month_date, anomaly, color = quarter, alpha = abs(anomaly))
    ) +
    # connecting lines
    ggplot2::geom_line(
      data = data_df$before,
      ggplot2::aes(month_date, anomaly),
      alpha = 0.3,
      linewidth = 0.1
    ) +
    ggplot2::geom_line(
      data = data_df$after,
      ggplot2::aes(month_date, anomaly),
      alpha = 0.3,
      linewidth = 0.1
    ) +
    # zero reference line (climatological mean)
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    # linear trend lines by quarter (post-1993 only)
    ggplot2::stat_smooth(
      data = data_df$after,
      ggplot2::aes(month_date, anomaly, group = quarter, color = quarter),
      method = "lm",
      se = FALSE,
      linewidth = 0.8
    ) +
    # JAMA color palette for quarterly distinction
    ggsci::scale_color_jama() +
    ggplot2::scale_x_date(
      date_breaks = "4 years",
      labels = scales::label_date(format = "%b\n%Y", locale = "en_US.UTF-8")
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    # statistical results annotation
    ggplot2::annotate(
      "text",
      x = as.Date("1985-12-01"),
      y = max(data_df$after$anomaly, na.rm = TRUE),
      label = annotations_text,
      hjust = 0,
      vjust = 1,
      color = "grey40"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(alpha = "none") +
    ggplot2::labs(
      title = bquote(italic(.(plot_title))),  # dynamic title based on what is being plotted
      x = "Date",
      y = "Monthly mean deviation\n(Abundance)",
      color = "Annual quarter"
    )
}

# dispaly
plot_temporal_trend(species_name = "All")
plot_temporal_trend(species_name = "Temora stylifera")
plot_temporal_trend(rank_class = "Copepoda")
plot_temporal_trend(rank_order = "Onychopoda")
plot_temporal_trend(rank_order = "Cyclopoida")

#####################################################################################








#####################################################################################
# first try
# data visualization on Acartia calusi and Temora stylifera
# select only the variables of interest
zoo_data <-
  legacy_84_24 |>
  dplyr::filter(scientificName %in% c("Acartia clausi", "Temora stylifera")) |>
  dplyr::select(eventDate, scientificName, individualCount, lifeStage)


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

# time series - trend Ab
# Function to create temporal trend plot
plot_trend_Ab <- function(species_name = NULL) {
  # if species_name is NULL or "All" aggregate all species
  if (is.null(species_name) || species_name == "All") {
    data <-
      zoo_84_24 |>
      dplyr::mutate(year = lubridate::year(eventDate)) |>
      dplyr::group_by(eventDate, year) |>
      dplyr::summarise(
        abundance = sum(individualCount, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(eventDate)

    species_name <- "All Copepods" # for the title
  } else {
    # Filter data by specific species
    data <-
      zoo_84_24 |>
      dplyr::filter(scientificName == species_name) |>
      dplyr::mutate(year = lubridate::year(eventDate)) |>
      dplyr::group_by(eventDate, year) |>
      dplyr::summarise(
        abundance = sum(individualCount, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(eventDate)
  }

  # Define separate periods
  period1 <- data |> dplyr::filter(year >= 1984 & year <= 1990)
  period2 <- data |> dplyr::filter(year >= 1995 & year <= 2024)

  # Linear models for each period independently
  lm1 <- lm(abundance ~ eventDate, data = period1)
  lm2 <- lm(abundance ~ eventDate, data = period2)

  coef1 <- summary(lm1)$coefficients["eventDate", "Estimate"] # coef: how change the abundance day by day, if negative it shows an Ab decrement over time
  pval1 <- summary(lm1)$coefficients["eventDate", "Pr(>|t|)"] # p-value: probability that the trend is due to random chance (p>0.05 not significant/p<0.05 significant)
  coef2 <- summary(lm2)$coefficients["eventDate", "Estimate"]
  pval2 <- summary(lm2)$coefficients["eventDate", "Pr(>|t|)"]

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
    by = "4 years"
  )
  breaks_period2 <- seq(
    as.Date("1996-01-01"),
    as.Date("2024-12-31"),
    by = "4 years"
  )
  x_breaks <- c(breaks_period1, breaks_period2)

  # Create plot
  ggplot2::ggplot() +
    # Vertical spikes
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(x = eventDate, xend = eventDate, y = 0, yend = abundance),
      color = "grey70",
      alpha = 0.4
    ) +
    # Blue regression lines - SEPARATE for each period
    # linear model : linear regression a linear relationship over time
    ggplot2::geom_smooth(
      data = period1,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "lm",
      se = FALSE,
      color = "blue",
      linewidth = 1.2
    ) +
    ggplot2::geom_smooth(
      data = period2,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "lm",
      se = FALSE,
      color = "blue",
      linewidth = 1.2
    ) +
    # Red local polynomial regression - SEPARATE for each period
    # local regression: it adapts to data that may show fluctuations over time
    ggplot2::geom_smooth(
      data = period1,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "loess",
      se = FALSE,
      color = "red",
      linewidth = 1.2
    ) +
    ggplot2::geom_smooth(
      data = period2,
      ggplot2::aes(x = eventDate, y = abundance),
      method = "loess",
      se = FALSE,
      color = "red",
      linewidth = 1.2
    ) +
    # X axis: every 4 years
    ggplot2::scale_x_date(breaks = x_breaks, date_labels = "%Y") +
    # Y axis: dynamic limits based on data
    ggplot2::scale_y_continuous(
      limits = c(0, max(data$abundance, na.rm = TRUE) * 1.1),
      breaks = scales::pretty_breaks(n = 3)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = bquote(italic(.(species_name))),
      x = "Years",
      y = expression(bold(ind ~ m^-3)),
      caption = caption_text
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
      axis.text = ggplot2::element_text(size = 14, "bold"),
      plot.caption = ggplot2::element_text(
        size = 12,
        hjust = 0,
        face = "bold"
      ),
      panel.grid.major = ggplot2::element_line(color = "grey90")
    )
}

# display
plot_temporal_trend(species_name = "All") # it's same for every species
plot_temporal_trend(species_name = "Pleuromamma gracilis")
