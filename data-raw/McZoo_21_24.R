# McZoo_21_24
# Load packages and configurations (including credentials from .env)
devtools::load_all()
conf <- read_config()

# Upload raw data from kobo
ingest_surveys()

# Download raw data 
results <- preprocess_surveys()
results |> View()

# Clean raw data 
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

# TO DO: prendi i nomi scientifici da worms e uniscili aggiungendo una nuova colonna, poi crea la colonna stage e individual count per le abbondanze 


