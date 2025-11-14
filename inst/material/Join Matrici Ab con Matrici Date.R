# Join Matrici Ab con Matrici Date 
getwd()
setwd("C:/Users/andre/OneDrive/Desktop/Progetto GdN_40Y_DTO Bio-Flow/ZOOGoN-40Y/script andrea")

install.packages("readxl")
library(readxl)

# carico le matrici su cui devo lavorare 
dat <- readxl::read_xlsx("C:/Users/andre/OneDrive/Desktop/Progetto GdN_40Y_DTO Bio-Flow/ZOOGoN-40Y/script andrea/ID_Ab/Species_ID_2019.xlsx",
  .name_repair = "minimal"                       # dice a readxl di non modificare i nomi delle colonne, anche con nomi, spazie e caratteri speciali 
) |>                                               # aggiunge una nuova colonna "dat_id" che contiene una sequenza di numeri da 1 al numero totola di righe del ds "dplyr::n()"
  dplyr::mutate(dat_id = seq_len(dplyr::n())) |>    # è un modo per creare un identificator eunivoco per ogni riga 
  janitor::clean_names()                          # pulisce i nomi delle varianili per renderli coerenti e "tidy", es. tutto minuscolo, underscore dove c'è lo spazio, caratteri speciali rimossi                      

View(dat)

ids <-
  readxl::read_xlsx(
    "C:/Users/andre/OneDrive/Desktop/Progetto GdN_40Y_DTO Bio-Flow/ZOOGoN-40Y/script andrea/date_id_volume/ids_2019.xlsx",
    .name_repair = "minimal"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(                        # applica rasformazioni alle colonne 
    date = lubridate::as_date(date),    # converte la colonna delle date in un oggetto di classe "Date", serve per essere sicuri che erre la interpreti come data e non stringa o numero excel 
    id = janitor::make_clean_names(id)  # pulisce i valori della colonna id, vengono puliti i contenuti non il nome della colonna 
  )                                     # trasforma ogni valore in minuscolo, sostituisce spazi con underscore e rimuove simboli strani 

View(ids)

library(tidyr)
library(dplyr)
merged <- dat |>                        # trasforma il ds da wide a long 
  tidyr::pivot_longer(                  # non serve usare questa funzione in questo caso, perché le matrici su cui sto lavorando le ho già convertite in fomrato long e caricate in quel formato 
    cols = -c(dat_id, taxa, stage),
    names_to = "id",
    values_to = "ind_m3"
  ) 
  
# unisco e strutturo il dataset come voglio io 
  merged <- dat |>
  dplyr::distinct() |>                                                      # rimuove eventuali righe duplicate che possono nascere anche dopo il pivot
  dplyr::full_join(ids, by = "id") |>                                       # unisce il ds con la tabella ids, usando come chiave la colonna id e unisce al ds le colonne provenienti da ids, tipo le date, mentiene tutti gli id presenti sia in uno che nell'altro dataset
  dplyr::select(sample_id = "id", "date", "taxa", "stage", "ind_m3") |>     # rinomina la colonna id come "sample _id"
  dplyr::arrange(date, taxa)                                                # ordina le righe, prima date e poi taxa

View(merged)


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