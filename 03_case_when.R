############################################################
# TIDYVERSE – LEZIONE 2
# mutate(), transmute(), case_when() e introduzione ai loop
############################################################

library(dplyr)
library(palmerpenguins)

data(penguins)

# Guardiamo il dataset
glimpse(penguins)


############################################################
# 1 MUTATE
############################################################

# mutate() serve per creare nuove colonne
# oppure modificare colonne esistenti

penguins |>
  mutate(
    body_mass_kg = body_mass_g / 1000
  ) |>
  head()

# Possiamo creare più colonne

penguins |>
  mutate(
    body_mass_kg = body_mass_g / 1000,
    bill_ratio = bill_length_mm / bill_depth_mm
  ) |>
  head()

# Possiamo anche modificare una colonna esistente

penguins |>
  mutate(
    body_mass_g = body_mass_g / 1000
  ) |>
  head()


############################################################
# 2 TRANSMUTE
############################################################

# transmute() crea nuove colonne
# ma elimina quelle vecchie

penguins |>
  transmute(
    species,
    body_mass_kg = body_mass_g / 1000
  ) |>
  head()

# Differenza:
# mutate() mantiene tutto
# transmute() tiene solo le nuove colonne


############################################################
# 3 CLASSIFICARE VARIABILI CON MUTATE
############################################################

# Possiamo creare categorie

penguins |>
  mutate(
    big_penguin = body_mass_g > 5000
  ) |>
  select(species, body_mass_g, big_penguin) |>
  head()


############################################################
# 4 INTRODUZIONE AI LOOP
############################################################

# Prima delle funzioni vettoriali
# spesso si usavano loop

mass <- penguins$body_mass_g

size_class <- vector(length = length(mass))

for(i in 1:length(mass)){
  
  if(is.na(mass[i])){
    
    size_class[i] <- NA
    
  } else if(mass[i] > 5500){
    
    size_class[i] <- "large"
    
  } else if(mass[i] > 4000){
    
    size_class[i] <- "medium"
    
  } else {
    
    size_class[i] <- "small"
    
  }
  
}

head(size_class)

# aggiungiamo la colonna

penguins$size_class_loop <- size_class


############################################################
# 5 CASE_WHEN
############################################################

# case_when() è il modo tidyverse
# per fare classificazioni multiple

penguins |>
  mutate(
    size_class = case_when(
      body_mass_g > 5500 ~ "large",
      body_mass_g > 4000 ~ "medium",
      body_mass_g <= 4000 ~ "small",
      TRUE ~ NA_character_
    )
  ) |>
  select(species, body_mass_g, size_class) |>
  head()


############################################################
# 6 ALTRO ESEMPIO DI CLASSIFICAZIONE
############################################################

penguins |>
  mutate(
    flipper_class = case_when(
      flipper_length_mm > 220 ~ "very_long",
      flipper_length_mm > 200 ~ "long",
      flipper_length_mm > 180 ~ "medium",
      TRUE ~ "short"
    )
  ) |>
  select(species, flipper_length_mm, flipper_class) |>
  head()


############################################################
# 7 PIPELINE COMPLETA
############################################################

penguins |>
  filter(!is.na(body_mass_g)) |>
  mutate(
    size_class = case_when(
      body_mass_g > 5500 ~ "large",
      body_mass_g > 4000 ~ "medium",
      TRUE ~ "small"
    )
  ) |>
  select(species, island, body_mass_g, size_class) |>
  head()


############################################################
# ESERCIZI
############################################################

# usare sempre il dataset penguins
# e il pipe nativo |>


### ESERCIZIO 1
# creare una colonna body_mass_kg


### ESERCIZIO 2
# creare una variabile bill_ratio
# = bill_length_mm / bill_depth_mm


### ESERCIZIO 3
# usare transmute per ottenere
# species e body_mass_kg


### ESERCIZIO 4
# creare una variabile big_penguin
# TRUE se body_mass_g > 5000


### ESERCIZIO 5
# classificare body_mass_g
# >5500  heavy
# >4500  medium
# else   light


### ESERCIZIO 6
# classificare flipper_length_mm
# >220 long
# >200 medium
# else short


### ESERCIZIO 7
# creare una variabile bill_long
# TRUE se bill_length_mm > 45


### ESERCIZIO 8
# creare una variabile species_code
# Adelie = A
# Gentoo = G
# Chinstrap = C


### ESERCIZIO 9
# filtrare body_mass_g > 4500
# poi creare size_class


### ESERCIZIO 10
# creare una variabile heavy_flipper
# body_mass_g > 5000 AND flipper_length_mm > 210


############################################################
# PROMEMORIA
############################################################

# mutate()      -> crea o modifica colonne
# transmute()   -> crea colonne e elimina le vecchie
# case_when()   -> alternativa leggibile a molti if
# for loop      -> metodo classico ma meno elegante