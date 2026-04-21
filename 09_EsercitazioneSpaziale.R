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

# 10) verificare:
# - CRS
# - geometria

# 11) plot dei punti


############################################################
# FASE 5 – RASTER
############################################################

# 12) caricare DEM

# 13) ritagliare sull’area dei punti

# 14) visualizzare raster


############################################################
# FASE 6 – INTEGRAZIONE
############################################################

# 15) estrarre quota nei punti

# 16) aggiungere quota al dataset

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

# 21) interpretare relazione


############################################################
# FASE 9 – EXTENSION (opzionale)
############################################################

# 22) aggiungere colonna "uncertainty"

# 23) creare buffer

# 24) estrarre quota media nel buffer

# 25) confrontare:
# quota punto vs quota buffer