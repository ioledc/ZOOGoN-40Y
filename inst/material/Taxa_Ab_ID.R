# MATRICI 

# 2019
library(xlsx)
library(dplyr)

# carico la matrice 
matrix_2019 <- xlsx::read.xlsx("Matrice di base_2019.xlsx", 
                              sheetIndex = 1,
                              header = F)

View(matrix_2019)

# Estraggo ID campionamento
data_ID <- matrix_2019 %>% slice(1)
data_ID
View(data_ID)

# Estraggo specie con le relative Ab
data_species <- matrix_2019 %>% slice(c(11:416))
data_species
View(data_species)

# seleziono solo gli ID di campionamento 
ID <- data_ID %>% select(c(X3:X42))
ID
View(ID)

# selezione solo le specie, stage e Ab 
Taxa <- data_species %>% select(c(X1:X42))
class(Taxa)
View(Taxa)
head(Taxa)

# creo vettore con gli ID di campionamento che diventeranno variabili insieme a taxa e stage 
Sampling <- c("MC1314", "MC1315", "MC1316", "MC1317", "MC1318", "MC1319", "MC1320", "MC1321", "MC1322", "MC1323", "MC1324", "MC1325", "MC1326", "MC1327", "MC1328", "MC1329", "MC1330", "MC1331", "MC1332", "MC1333", "MC1334", "MC1335", "MC1336", "MC1337", "MC1338", "MC1339", "MC1340", "MC1341", "MC1342", "MC1343", "MC1344", "MC1345", "MC1346", "MC1347", "MC1348", "MC1349", "MC1350", "MC1351", "MC1352", "MC1353")
Sampling
class(Sampling)

# cambio il nome alle variabili 
colnames(Taxa) <- c("Taxa", "Stage", Sampling)
names(Taxa)
View(Taxa)

# elimino la riga che non mi interessa 
Taxa <- Taxa %>% slice(-1)

View(Taxa)

# standardizzo i nomi delle variabili 
library(janitor)
Taxa <- janitor::clean_names(Taxa)
View(Taxa)

# data tidying 
install.packages("tidyverse")
library(tidyverse)

# formato long
TAXA <- Taxa|>
  tidyr::pivot_longer(             
    cols = starts_with("mc"),                     # cols = specifica quali colonne devono essere ruotate
    names_to = "id",                              # names_to assegna un nome alla variabile memorizzata nella colonna names
    values_to = "ind/m3"                          # values_to assegna un nome alla variabile memorizzata nei valori delle celle
  )

View(TAXA)

# rimuovo i duplicati 
TAXA <- dplyr::distinct(TAXA)
View(TAXA)

str(TAXA)

# converto tutte la colonna ind/m3 in numerica 
TAXA <- TAXA %>%
  mutate(across(
    .cols = -c(taxa, stage, id),   # tutte le colonne tranne queste due
    .fns = as.numeric              # applica la conversione
  ))

# mutate() → modifica o aggiunge colonne nel data frame
# across() → applica una funzione a un gruppo di colonne
# .cols = -c(taxa, stage) → esclude taxa e stage
# .fns = as.numeric → converte ogni colonna selezionata in numerico

str(TAXA)
View(TAXA)

# salvo il file 
xlsx::write.xlsx(TAXA, "Species_ID_2019.xlsx", sheetName = "Sheet1", col.names = T)


# 2016

# carico la matrice
matrix_2016 <- xlsx::read.xlsx("Matrice di base_2016_dettagliata.xlsx",
                              sheetIndex = 1,
                              header = F)

View(matrix_2016)

# Estraggo ID e Ab
Data_ID_Ab <- matrix_2016 %>% slice(c(2, 11:412)) 
Data_ID_Ab
View(Data_ID_Ab)

# Cambio il nome alle variabili 
Sampling <- c("MC1185","MC1186","MC1187","MC1188","MC1189","MC1190","MC1191","MC1192","MC1193","MC1194","MC1195","MC1196","MC1197","MC1198","MC1199","MC1200","MC120","MC1202","MC1203","MC1204","MC1205","MC1206","MC1207","MC1208","MC1209","MC1210","MC1211","MC1212","MC1213","MC1214","MC1215","MC1216","MC1217","MC1218","MC1219","MC1220","MC1221","MC1222","MC1223","MC1224","MC1225","MC1226","MC1227","MC1228","MC1229","MC1230","MC1231")

# pulisco il df 
ID_Ab <- Data_ID_Ab %>% slice(-c(1,2))
ID_Ab <- Data_ID_Ab %>% select(-c(X50:X53))
ID_Ab
View(ID_Ab)
class(ID_Ab)

# rinomino le colonne e finisco di pulire il db
colnames(ID_Ab) <- c("taxa", "Stage", Sampling)

ID_Ab_2016 <- ID_Ab %>% slice(-c(1,2))
View(ID_Ab_2016)

# standardizzo i nomi delle variabili 
library(janitor)
Species_ID_2016 <- janitor::clean_names(ID_Ab_2016)
View(Species_ID_2016)

# data tidying 
install.packages("tidyverse")
library(tidyverse)

Species_ID_2016 <- Species_ID_2016 |>
  tidyr::pivot_longer(             
    cols = starts_with("mc"),                     
    names_to = "id",                              
    values_to = "ind/m3"                          
  )

View(Species_ID_2016)

# rimuovo i duplicati 
Species_ID_2016 <- dplyr::distinct(Species_ID_2016)
View(Species_ID_2016)

str(Species_ID_2016)

# converto in numerica la variabile ind/m3 
# usando uno di questi due approcci 
# 1
Species_ID_2016[ , !(names(Species_ID_2016) %in% c("taxa", "stage")) ] <-
  lapply(Species_ID_2016[ , !(names(Species_ID_2016) %in% c("taxa", "stage")) ], as.numeric)

# names(Species_ID_2016) Restituisce un vettore con i nomi di tutte le colonne del data frame.
# names(Species_ID_2016) %in% c("taxa", "stage") Crea un vettore logico (TRUE/FALSE) che indica se ogni nome è tra quelli specificati.
# !(...) Il simbolo ! nega i valori logici: TRUE diventa FALSE, FALSE diventa TRUE.
# Species_ID_2016[ , condizione ] È la selezione per colonne. Le righe non sono specificate, quindi prendiamo tutte ([ , ...] = tutte le righe, solo alcune colonne).
# lapply(..., as.numeric) Applica la funzione as.numeric() a ogni colonna selezionata. lapply restituisce una lista della stessa lunghezza (una per colonna), con i valori convertiti in numerici.

# 2
Species_ID_2016 <- Species_ID_2016 %>%
  mutate(across(
    .cols = -c(taxa, stage, id),   # tutte le colonne tranne queste 3
    .fns = as.numeric              # applica la conversione
  ))

# mutate() → modifica o aggiunge colonne nel data frame
# across() → applica una funzione a un gruppo di colonne
# .cols = -c(taxa, stage) → esclude taxa e stage
# .fns = as.numeric → converte ogni colonna selezionata in numerico

str(Species_ID_2016)
View(Species_ID_2016)
glimpse(Species_ID_2016)  # stessa funzionalità di str()

View(Species_ID_2016)


# salvo il file 
xlsx::write.xlsx(Species_ID_2016, "Species_ID_2016.xlsx", sheetName = "Sheet1", col.names = T)

# 2017
# carico la matrice
matrix_2017 <- xlsx::read.xlsx("Matrice di base_2017_dettagliata.xlsx",
                              sheetIndex = 1,
                              header = F)

View(matrix_2017)

# estraggo quello che mi interessa 
ID_Abb <- matrix_2017 %>% slice(c(1, 11:413)) %>% select(-c(X47:X77))
ID_Abb
View(ID_Abb)


# Creo vettore da utilizzare per il nome delle variabili 
cruise <- c("MC1233","MC1234","MC1235","MC1236","MC1237","MC1238","MC1239","MC1240","MC1241","MC1242","MC1243","MC1244","MC1245","MC1246","MC1247","MC1248","MC1249","MC1250","MC1251","MC1252","MC1253","MC1254","MC1255","MC1256","MC1257","MC1258","MC1259","MC1260","MC1261","MC1262","MC1263","MC1264","MC1265","MC1266","MC1267","MC1268","MC1269","MC1270","MC1271","MC1272","MC1273","MC1274","MC1275","MC1276")

# pulisco db
ID_Abb <- ID_Abb %>% slice(-c(1,2))
View(ID_Abb)

# rinomino le variabili 
colnames(ID_Abb) <- c("taxa", "stage", cruise)

# standardizzo 
ID_Abb <- janitor::clean_names(ID_Abb)

# data tidyng 

Species_ID_2017 <- ID_Abb |>
  tidyr::pivot_longer(             
    cols = starts_with("mc"),                     
    names_to = "id",                              
    values_to = "ind/m3"                          
  )

View(Species_ID_2017)

# elimino duplicati 
dplyr::distinct(Species_ID_2017)


# converto i valori di Ab in numerici 
Species_ID_2017 <- Species_ID_2017 %>%
  mutate(across(
    .cols = -c(taxa, stage, id),  
    .fns = as.numeric          
  ))

str(Species_ID_2017)


# salvo il file 
xlsx::write.xlsx(Species_ID_2017, "Species_ID_2017.xlsx", sheetName = "Sheet1", col.names = T)

# 2018
# carico la matrice 
matrix_2018 <- xlsx::read.xlsx("Matrice di base_2018.xlsx", 
                              sheetIndex = 2, 
                              header = F)

View(matrix_2018)

# estraggo i dati che mi servono 
ID_Ab <- matrix_2018 %>% slice(c(1, 11:414))
ID_Ab
View(ID_Ab)

# vettore per nome variabili
samp <- c("MC1277","MC1278","MC1279","MC1280","MC1281","MC1282","MC1283","MC1284","MC1285","MC1286","MC1287","MC1288","MC1289","MC1290","MC1291","MC1292","MC1293","MC1294","MC1295","MC1296","MC1297","MC1298","MC1299","MC1300","MC1301","MC1302","MC1303","MC1304","MC1305","MC1306","MC1307","MC1308","MC1309","MC1310","MC1311","MC1312","MC1313")

# pulisco db
ID_Ab <- ID_Ab %>% slice(-c(1:2))

# rinomino le variabili 
colnames(ID_Ab) <- c("TAXA", "STAGE", samp)

# standardizzo nomi 
ID_Ab <- janitor::clean_names(ID_Ab)

# data tidyng

Species_ID_2018 <- ID_Ab |>
  pivot_longer(cols = starts_with("mc"), 
  names_to = "id",                              
  values_to = "ind/m3"
  )

View(Species_ID_2018)

# elimino duplicati 
dplyr::distinct(Species_ID_2018)

str(Species_ID_2018)

# convertire i valori di Ab in numero
Species_ID_2018 <- Species_ID_2018 %>% 
  mutate(across(
    .cols = -c(taxa, stage, id), 
    .fns = as.numeric
  ))

# salvo il file 
xlsx::write.xlsx(Species_ID_2018, "Species_ID_2018.xlsx", sheetName = "2018")


# 2020
# carico la matrice 
matrix_2020 <- xlsx::read.xlsx("Matrice di base_2020.xlsx", 
                              sheetIndex = 1, 
                              header = F)

View(matrix_2020)

# estraggo i dati che mi servono 
ID_Ab <- matrix_2020 %>% slice(c(1, 11:414)) %>% select(-c(X26,X27))
ID_Ab
View(ID_Ab)

# vettore per nome variabili
samp <- c("MC1354","MC1355","MC1356","MC1357","MC1358","MC1359","MC1360","MC1361","MC1362","MC1363","MC1364","MC1365","MC1366","MC1367","MC1368","MC1369","MC1370","MC1371","MC1372","MC1373","MC1374","MC1375","MC1376")

# pulisco db
ID_Ab <- ID_Ab %>% slice(-c(1:2))

# rinomino le variabili 
colnames(ID_Ab) <- c("TAXA", "STAGE", samp)

# standardizzo nomi 
ID_Ab <- janitor::clean_names(ID_Ab)

# data tidyng
Species_ID_2020 <- ID_Ab |>
  pivot_longer(cols = starts_with("mc"), 
  names_to = "id",                              
  values_to = "ind/m3"
  )

View(Species_ID_2020)

# elimino duplicati 
dplyr::distinct(Species_ID_2020)

str(Species_ID_2020)

# convertire i valori di Ab in numero
Species_ID_2020 <- Species_ID_2020 %>% 
  mutate(across(
    .cols = -c(taxa, stage, id), 
    .fns = as.numeric
  ))

# salvo il file 
xlsx::write.xlsx(Species_ID_2020, "Species_ID_2020.xlsx", sheetName = "2020")
