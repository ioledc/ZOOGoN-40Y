getwd()
setwd("C:/Users/andre/OneDrive/Desktop/Progetto GdN_40Y_DTO Bio-Flow/ZOOGoN-40Y/script andrea")
getwd()
# carico questo per caricare file .xlsx
install.packages("xlsx")
library(xlsx)

# carico questo pacchetto per manipolare i dati 
install.packages("dplyr")
library(dplyr)


# carico il db Matrice 2019
matrix_2019 <- xlsx::read.xlsx("Matrice di base_2019.xlsx", 
                              sheetIndex = 1,
                              header = F)


head(matrix_2019)
View(matrix_2019)

# estraggo soltanto la prima riga con ID del campionamento e data   
class(matrix_2019)

row1_3_9 <- matrix_2019 %>% slice(c(1,3,9))
row1_3_9
View(row1_3_9)

# pulisco tenendo solo i casi che servono 
IDs_2019 <- row1_3_9 %>% select(X3:X42)
IDs_2019
View(IDs_2019)
class(IDs_2019)

# me lo imposto come dataframe a tre colonne 
# trasformo le colonne in righe 

install.packages("tidyr")
library(tidyr)

ID <- IDs_2019 %>% slice(1) %>% select(X3:X42) %>% unlist(use.names = FALSE) %>% as.character()
ID
class(ID)

Date_2019 <- IDs_2019 %>% slice(2) %>% select(X3:X42) %>% unlist(use.names = FALSE) %>% as.character()
Date_2019
class(Date)

Filtered_Volume_m3 <- IDs_2019 %>% slice(3) %>% select(X3:X42) %>% unlist(use.names = F) %>% as.numeric()
class(Filtered_Volume_m3)
Filtered_Volume_m3


# ottengo il dataframe come lo voglio 

ids_2019 <- tibble(
  ID = ID,
  Date = Date,
  FilteredVolumeM3 = Filtered_Volume_m3
)

View(ids_2019)
# converto le date in fomrato normale 
library(lubridate)

Date_2019 <- ifelse(
  grepl("^[0-9]+$", Date_2019),                                   # se è solo numeri
  as.character(as.Date(as.numeric(Date_2019), origin = "1899-12-30")),  # converto da seriale Excel
  Date_2019                                                        
)
Date_2019

# ottengo il dataframe come lo voglio 

ids_2019 <- tibble(
  ID = ID,
  Date = Date_2019,
  FilteredVolumeM3 = Filtered_Volume_m3
) 
ids_2019

# standardizzo 
ids_2019 <- janitor::clean_names(ids_2019) |>
  dplyr::mutate(id = gsub("_", "", janitor::make_clean_names(id)))

ids_2019

# salvo il file 
xlsx::write.xlsx(ids_2019, "ids_2019.xlsx", sheetName = "Sheet1", col.names = T)

# 2016
# carico il db Matrice 2016
matrix_2016 <- xlsx::read.xlsx("Matrice di base_2016_dettagliata.xlsx", 
                              sheetIndex = 1,
                              header = F)


head(matrix_2016)
View(matrix_2016)

# estraggo soltanto la prima riga con ID del campionamento e data   
class(matrix_2019)

row2_3 <- matrix_2016 %>% slice(c(2,3))
row2_3
View(row2_3)

# pulisco tenendo solo i casi che servono 
IDs_2016 <- row2_3 %>% select(X3:X49)
View(IDs_2016)

# me lo imposto come dataframe a tre colonne 
# trasformo le colonne in righe 

install.packages("tidyr")
library(tidyr)

ID <- IDs_2016 %>% slice(1) %>% select(X3:X49) %>% unlist(use.names = FALSE) %>% as.character()
ID
class(ID)

Date <- IDs_2016 %>% slice(2) %>% select(X3:X49) %>% unlist(use.names = FALSE) %>% as.character()
Date

# converto le date in fomrato normale 
library(lubridate)
Date <- as.numeric(Date) 
Date
class(Date)

date_num <- c(42383, 42388, 42393, 42402, 42418, 42423, 42432, 42444, 42450, 42458, 42465, 42472, 42479, 42486, 42494, 42500, 42508, 42515, 42521, 42527, 42535, 42542, 42549, 42556, 42563, 42570, 42577, 42584, 42591, 42607, 42612, 42620, 42626, 42633, 42640, 42647, 42654, 42661, 42669, 42678, 42682, 42691, 42696, 42704, 42710, 42718, 42727)

# Converto in Date usando l'origine Excel
date <- as_date(date_num, origin = "1899-12-30")

# Mostro le date
date_real

# ottengo il dataframe come lo voglio 

ids_2016 <- tibble(
  ID = ID,
  Date = date,
) 


class(ids_2016)
View(ids_2016)

# elimino una riga uguale
ids_2016 <-ids_2016 %>% slice(-36)
View(ids_2016)

# standardizzazione nomi variabili e casi della variabili id
install.packages("janitor")
library(janitor)

ids_2016 <- janitor::clean_names(ids_2016) |> 
  dplyr::mutate(id = gsub("_", "", janitor::make_clean_names(id)))

View(ids_2016)

# salvo il file 
xlsx::write.xlsx(ids_2016, "ids_2016.xlsx", sheetName = "Sheet1", col.names = T)

# 2017
# carico il db Matrice 2017
matrix_2017 <- xlsx::read.xlsx("Matrice di base_2017_dettagliata.xlsx", 
                              sheetIndex = 1,
                              header = F)

matrix_2017
head(matrix_2017)
View(matrix_2017)

# estraggo soltanto la prima riga con ID del campionamento e data   
row1_3 <- matrix_2017 %>% slice(c(1,3))
row1_3
View(row1_3)

# pulisco tenendo solo i casi che servono 
IDs_2017 <- row1_3 %>% select(X3:X46)
View(IDs_2017)

# me lo imposto come dataframe a tre colonne 
# trasformo le colonne in righe 
install.packages("tidyr")
library(tidyr)

ID <- IDs_2017 %>% slice(1) %>% select(X3:X46) %>% unlist(use.names = FALSE) %>% as.character()
ID
class(ID)

Date <- IDs_2017 %>% slice(2) %>% select(X3:X46) %>% unlist(use.names = FALSE) %>% as.character()
Date
class(Date)
date <- as.numeric(Date)
class(date)

# modifico le data in da julina day in fomato normale 
library(lubridate)

date_num <- c(42745, 42752, 42759, 42766, 42774, 42783, 42787, 42794, 42803, 42808, 42815, 42822, 42830, 42836, 42843, 42851, 42860, 42864, 42873, 42878, 42885, 42892, 42899, 42906, 42913, 42920, 42927, 42934, 42943, 42948, 42954, 42978, 42983, 42991, 42999, 43004, 43011, 43018, 43025, 43031, 43060, 43068, 43073, 43088)

# Converto in Date usando l'origine Excel
date_real <- as_date(date_num, origin = "1899-12-30")

date_real

# ottengo il dataframe come lo voglio 

ids_2017 <- tibble(
  ID = ID,
  Date = date_real,
) 


class(ids_2017)
View(ids_2017)

# standardizzo 
ids_2017 <- janitor::clean_names(ids_2017) |>
  dplyr::mutate(id = gsub("_", "", janitor::make_clean_names(id)))

# salvo il file 
xlsx::write.xlsx(ids_2017, "ids_2017.xlsx", sheetName = "Sheet1", col.names = T)

# 2018
matrix_2018 <- xlsx::read.xlsx("Matrice di base_2018.xlsx", 
                              sheetIndex = 2,
                              header = F)
View(matrix_2018)

# estraggo soltanto la prima riga con ID del campionamento e data   
row1_3_9 <- matrix_2018 %>% slice(c(1,3,9))
row1_3_9
View(row1_3_9)

# pulisco tenendo solo i casi che servono 
IDs_2018 <- row1_3_9 %>% select(X3:X39)
View(IDs_2018)

# me lo imposto come dataframe a tre colonne 
# trasformo le colonne in righe 
install.packages("tidyr")
library(tidyr)

ID <- IDs_2018 %>% slice(1) %>% select(X3:X39) %>% unlist(use.names = FALSE) %>% as.character()
ID
class(ID)

Date <- IDs_2018 %>% slice(2) %>% select(X3:X39) %>% unlist(use.names = FALSE) %>% as.character()
Date
class(Date)


Filtered_volume_m3 <- IDs_2018 %>% slice(3) %>% select(X3:X39) %>% unlist(use.names = F) %>% as.numeric()
Filtered_volume_m3
class(Filtered_volume_m3)

# ottengo il dataframe come lo voglio 

ids_2018 <- tibble(
  ID = ID,
  Date = Date,
  FilteredVolumeM3 = Filtered_volume_m3
) 


class(IDs_2018)
View(ids_2018)

# standardizzo 
ids_2018 <- janitor::clean_names(ids_2018) |>
  dplyr::mutate(id = gsub("_", "", janitor::make_clean_names(id)))

# salvo il file 
xlsx::write.xlsx(ids_2018, "ids_2018.xlsx", sheetName = "Sheet1", col.names = T)

# 2020

matrix_2020 <- xlsx::read.xlsx("Matrice di base_2020.xlsx", 
                              sheetIndex = 1,
                              header = F)
View(matrix_2020)

# estraggo soltanto la prima riga con ID del campionamento e data   
row1_3_9 <- matrix_2020 %>% slice(c(1,3,9))
row1_3_9
View(row1_3_9)

# pulisco tenendo solo i casi che servono 
IDs_2020 <- row1_3_9 %>% select(X3:X25)
View(IDs_2020)

# me lo imposto come dataframe a tre colonne 
# trasformo le colonne in righe 
install.packages("tidyr")
library(tidyr)

ID <- IDs_2020 %>% slice(1) %>% select(X3:X25) %>% unlist(use.names = FALSE) %>% as.character()
ID
class(ID)

Date <- IDs_2020 %>% slice(2) %>% select(X3:X25) %>% unlist(use.names = FALSE) %>% as.character()
Date
class(Date)


Filtered_volume_m3 <- IDs_2020 %>% slice(3) %>% select(X3:X25) %>% unlist(use.names = F) %>% as.numeric()
Filtered_volume_m3
class(Filtered_volume_m3)

# ottengo il dataframe come lo voglio 

ids_2020 <- tibble(
  ID = ID,
  Date = Date,
  FilteredVolumeM3 = Filtered_volume_m3
) 


class(IDs_2020)
View(ids_2020)

# standardizzo 
ids_2020 <- janitor::clean_names(ids_2020) |>
  dplyr::mutate(id = gsub("_", "", janitor::make_clean_names(id)))

# salvo il file 
xlsx::write.xlsx(ids_2020, "ids_2020.xlsx", sheetName = "Sheet1", col.names = T)
