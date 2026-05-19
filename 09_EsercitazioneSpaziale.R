############################################################
# ESERCITAZIONE – ANALISI SPAZIALE (CHECKLIST)
############################################################

# Obiettivo:
# creare un dataset, pulirlo, unirlo e analizzarlo nello spazio


############################################################
# FASE 1 – CREAZIONE DATI
############################################################

# 1) creare dataset con colonne:
# siteID, x, y, SR

# 2) usare coordinate realistiche

# 3) salvare come .csv


############################################################
# FASE 2 – IMPORT E CHECK
############################################################

# 4) caricare tutti i file


nomi_files <- list.files(path = "SpatData/esercitazione/", 
           pattern = ".csv",
           full.names = T) 

files <- list()
for(i in 1:length(nomi_files)){
  files[[i]] <- read.csv(nomi_files[i])
}

nomi_files |> 
  map(read.csv) |> 
  list_rbind()

file1 <- read.csv("SpatData/esercitazione/MDM.csv")|> 
  select(siteID = 1, x = 2, y = 3, SR = 4) |>
  mutate(x = gsub(",", ".", x),
         y = gsub(",", ".", y)) |>
  mutate(x = as.numeric(x),
         y = as.numeric(y))
file2 <- read.csv("SpatData/esercitazione/MDM2.csv") |> 
  select(siteID = 1, x = 2, y = 3, SR = 4) |>
  mutate(x = gsub(",", ".", x),
         y = gsub(",", ".", y)) |>
  mutate(x = as.numeric(x),
         y = as.numeric(y))
  #|> 
  # write.csv("SpatData/esercitazione/MDM2_mod.csv", row.names = F)
  # 
pippo <-list(file1, file2) |> 
  list_rbind() 

pippo_vect <- st_as_sf(pippo, coords = c("x", "y"), crs = 4326)
plot(pippo_vect)
# 5) controllare per ciascun dataset:
# - nomi colonne
# - tipi variabili
# - NA
# - coerenza dati

# 6) uniformare:
# - stessi nomi
# - stessi tipi
# - stessa struttura


############################################################
# FASE 3 – UNIONE
############################################################

# 7) unire tutti i dataset

# 8) controllare:
# - numero osservazioni
# - duplicati
# - coordinate anomale
# - valori SR plausibili


############################################################
# FASE 4 – VETTORIALE
############################################################

# 9) convertire in oggetto spaziale (punti)


vect_01 <- st_as_sf(file1, coords = c("x", "y"), crs = 4326)

# 10) verificare:
# - CRS
# - geometria

crs(vect_01)
st_geometry(vect_01)
st_geometry_type(vect_01)

# 11) plot dei punti
plot(vect_01)

############################################################
# FASE 5 – RASTER
############################################################

# 12) caricare DEM

dem <- rast("SpatData/raster/elevation/ITA_elv_msk.tif")

# 13) ritagliare sull’area dei punti

# 14) visualizzare raster


############################################################
# FASE 6 – INTEGRAZIONE
############################################################

# 15) estrarre quota nei punti
extract(dem, pippo_vect)
pippo_elev <- terra::extract(dem, pippo_vect) 
# 16) aggiungere quota al dataset

pippo_vect$elev <- pippo_elev$ITA_elv_msk

pippo_vect2 <- pippo_vect |> 
  add_column(elev = pippo_elev$ITA_elv_msk, .after = "SR") |> 
  #drop_na()
  filter(!is.na(elev))

# 17) controllare:
# - valori realistici
# - NA
# - distribuzione


############################################################
# FASE 7 – MAPPE
############################################################

# 18) mappa raster + punti

# 19) mappa punti colorati per SR


############################################################
# FASE 8 – ANALISI ESPLORATIVE
############################################################

# 20) grafico:
# elevazione (asse x) vs SR (asse y)
ggplot(pippo_vect, aes(x = elev, y = SR)) +
  geom_point() +
  theme_minimal()
# 21) interpretare relazione


############################################################
# FASE 9 – EXTENSION (opzionale)
############################################################

# 22) aggiungere colonna "uncertainty"

# 23) creare buffer

# 24) estrarre quota media nel buffer

# 25) confrontare:
# quota punto vs quota buffer