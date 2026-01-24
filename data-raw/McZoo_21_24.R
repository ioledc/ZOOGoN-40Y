# McZoo 21_24
# Load packages and configurations
devtools::load_all()
conf <- read_config()

# Data ingestion
# Upload raw data from kobo toolbox
ingest_surveys()

# Download and preview raw data
results <- preprocess_surveys()
results |> View()

# Data Cleaning 
# Select relevant columns and create event identifiers
Clean_21_24 <-
  results |>
  dplyr::select(3:5, 19, 21:26) |>
  dplyr::mutate(EventID = paste0(site_name, sampling_id)) |>
  dplyr::relocate(EventID, .before = site_name) |>
  dplyr::select(-c(2:3)) |>
  dplyr::arrange(sampling_date) |>
  dplyr::rename(
    EventDate = "sampling_date",
    AphiaID = "taxon"
  ) |>
  janitor::clean_names()

# WORMS TAXONOMIC INFORMATION RETRIEVAL
# Function to retrieve scientific name and lsid from WoRMS API
get_worms_info <- function(aphia_id) {
  url <- paste0(
    "https://www.marinespecies.org/rest/AphiaRecordByAphiaID/",
    aphia_id
  )

  tryCatch(
    {
      response <- httr::GET(url)

      if (httr::status_code(response) == 200) {
        data <- jsonlite::fromJSON(httr::content(
          response,
          "text",
          encoding = "UTF-8"
        ))

       # Build complete scientific name with authority
        scientific_name_full <- if (
          !is.null(data$authority) &&
            !is.na(data$authority) &&
            data$authority != ""
        ) {
          authority <- data$authority

           # Remove parentheses if already present in authority
          authority_clean <- gsub("^\\((.+)\\)$", "\\1", authority)

          paste0(data$scientificname, " (", authority_clean, ")")
        } else {
          data$scientificname
        }

        return(list(
          scientific_name = scientific_name_full,
          lsid = data$lsid
        ))
      } else {
        warning(paste(
          "Errore per AphiaID",
          aphia_id,
          "- Status:",
          httr::status_code(response)
        ))
        return(list(scientific_name = NA, lsid = NA))
      }
    },
    error = function(e) {
      warning(paste("Errore per AphiaID", aphia_id, ":", e$message))
      return(list(scientific_name = NA, lsid = NA))
    }
  )

  Sys.sleep(0.1) # Rate limiting
}

# Filter rows with missing AphiaID
# TO DO: Remove this filter when all data is loaded
Clean_21_24_temp <-
  Clean_21_24 |>
  dplyr::filter(!is.na(aphia_id))

# Get unique AphiaID
unique_aphia_ids <- unique(Clean_21_24_temp$aphia_id)

# Retrieve taxonomic information from WoRMS
worms_info <- lapply(unique_aphia_ids, function(id) {
  get_worms_info(id)
})

# Create mapping dataframe
name_mapping <- data.frame(
  aphia_id = unique_aphia_ids,
  scientific_name = sapply(worms_info, function(x) x$scientific_name),
  lsid = sapply(worms_info, function(x) x$lsid),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Data harmonization as other matrices
Zoo_21_24 <-
  Clean_21_24_temp |>
  dplyr::left_join(name_mapping, by = "aphia_id") |>
  dplyr::relocate(scientific_name, lsid, .after = aphia_id) |>
   # Pivot abundance columns to long format
  tidyr::pivot_longer(                       
    cols = c(n_male, n_female, n_copepodite, n_undetermined, n_larvae, n_eggs),
    names_to = "life_stage_temp",
    values_to = "individual_count"
  ) |>           
  # Convert abundance to numeric and map life stages
  dplyr::mutate(
    individual_count = as.numeric(individual_count),
    life_stage = dplyr::case_when(
      life_stage_temp == "n_male" ~ "m",
      life_stage_temp == "n_female" ~ "f",
      life_stage_temp == "n_copepodite" ~ "j",
      life_stage_temp == "n_undetermined" ~ "fmj",
      life_stage_temp == "n_larvae" ~ "lar",
      life_stage_temp == "n_eggs" ~ "egg",
      TRUE ~ NA_character_
    )
  ) |>
  # Clean and organize columns
  dplyr::select(-life_stage_temp, -aphia_id) |>
  dplyr::filter(!is.na(individual_count)) |>
  dplyr::relocate(life_stage, .before = "individual_count") |>
  dplyr::rename(
    eventID = event_id,
    eventDate = event_date,
    scientificname = scientific_name,
    lifeStage = life_stage,
    individualCount = individual_count
  ) |>
  dplyr::distinct()

Zoo_21_24 |> View()
