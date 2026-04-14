library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(patchwork)
library(palmerpenguins)

data(penguins)

############################################################
# 1) IDEA DELLA LEZIONE
############################################################

# Useremo sempre lo stesso piccolo obiettivo:
#
# "per ciascuna specie di pinguino, costruire un grafico
# separato di bill_length_mm vs bill_depth_mm"
#
# Lo faremo in quattro modi:
#
# 1. con un for loop
# 2. con lapply()
# 3. con map()
# 4. senza liste di grafici, ma ristrutturando i dati
#    e usando facet_wrap()
#
# In questo modo:
# - che cos'è una ripetizione
# - che un loop non è sbagliato
# - che esistono approcci più compatti
# - che nel tidyverse spesso si preferisce cambiare
#   struttura ai dati invece di ripetere manualmente


############################################################
# 2) PREPARAZIONE DATI
############################################################

# Teniamo solo le colonne che ci servono e rimuoviamo gli NA
penguins_clean <- penguins |>
  select(species, island, bill_length_mm, bill_depth_mm,
         flipper_length_mm, body_mass_g, sex) |>
  filter(
    !is.na(species),
    !is.na(bill_length_mm),
    !is.na(bill_depth_mm),
    !is.na(flipper_length_mm),
    !is.na(body_mass_g),
    !is.na(sex)
  )
#drop_na()

# Controlliamo rapidamente
glimpse(penguins_clean)

# Quali specie ci sono?
species_list <- unique(penguins_clean$species)
species_list


############################################################
# 3) PRIMO PASSO: COSTRUIRE UN SOLO GRAFICO
############################################################

# Prima di parlare di loop, è utile mostrare il caso singolo:
# facciamo un grafico solo per Adelie

penguins_clean |>
  filter(species == "Adelie") |>
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  labs(
    title = "Adelie",
    x = "Bill length (mm)",
    y = "Bill depth (mm)"
  ) +
  theme_minimal()

# "ok, ma se volessi lo stesso grafico per tutte le specie?"


############################################################
# 4) APPROCCIO 1: FOR LOOP
############################################################

for(i in 1:5) {
  print(i)
}

numeri <- vector("list", 5)

for(i in 1:5) {
  numeri[[i]] <- i^2
}

numeri

# Qui creiamo una lista vuota dove salveremo i grafici
plot_list_for <- vector("list", length(species_list))

# Diamo anche un nome agli elementi della lista
names(plot_list_for) <- species_list

# Cicliamo sulle specie una per volta
for(i in seq_along(species_list)) {
  
  # estraggo il nome della specie corrente
  sp <- species_list[i]
  
  # filtro il dataset per la specie corrente
  dat_i <- penguins_clean |>
    filter(species == sp)
  
  # costruisco il grafico
  p_i <- ggplot(dat_i, aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point() +
    labs(
      title = sp,
      x = "Bill length (mm)",
      y = "Bill depth (mm)"
    ) +
    theme_minimal()
  
  # salvo il grafico nella lista
  plot_list_for[[i]] <- p_i
}

# Guardiamo la lista dei grafici
plot_list_for

# Mostriamo il primo grafico
plot_list_for[[1]]

# Montiamo insieme tutti i grafici con patchwork
wrap_plots(plot_list_for)


############################################################
# 5) APPROCCIO 2: LAPPly
############################################################

# lapply() fa sempre una ripetizione,
# ma in modo più compatto.
#
# L'idea è:
# "applica una funzione a tutti gli elementi di una lista/vettore"

lapply(X, function(x) { ... })


plot_list_lapply <- lapply(species_list, function(sp) {
  
  dat_i <- penguins_clean |>
    filter(species == sp)
  
  ggplot(dat_i, aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point() +
    labs(
      title = sp,
      x = "Bill length (mm)",
      y = "Bill depth (mm)"
    ) +
    theme_minimal()
  
})

# lapply restituisce una lista
plot_list_lapply

# Mostriamo tutti i grafici
wrap_plots(plot_list_lapply)

# - stessa logica del loop
# - meno codice "di controllo"
# - più enfasi sulla funzione da applicare


############################################################
# 6) PICCOLA DIGRESSIONE SU APPLY
############################################################

# apply() è utile soprattutto per matrici o array.

# Prendiamo alcune colonne numeriche
num_mat <- penguins_clean |>
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) |>
  as.matrix()

# Media per colonna
apply(num_mat, 2, mean)

# Media per riga
apply(num_mat, 1, mean) |>
  head()

# Spiegazione:
# MARGIN = 1 -> righe
# MARGIN = 2 -> colonne
#
# apply() è molto utile, ma non è lo strumento migliore
# per costruire tanti grafici separati a partire da gruppi.
# Per quello, liste + lapply/map sono spesso più naturali.


############################################################
# 7) APPROCCIO 3: MAP
############################################################

# map() di purrr è molto simile a lapply(),
# ma è molto usato nel tidyverse
# e spesso risulta più leggibile in pipeline moderne.

plot_list_map <- map(species_list, function(sp) {
  
  dat_i <- penguins_clean |>
    filter(species == sp)
  
  ggplot(dat_i, aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point() +
    labs(
      title = sp,
      x = "Bill length (mm)",
      y = "Bill depth (mm)"
    ) +
    theme_minimal()
  
})

# Mostriamo i grafici
wrap_plots(plot_list_map)


# Punti didattici:
# - map() è concettualmente simile a lapply()
# - è molto diffuso nel tidyverse
# - spesso è più leggibile in workflow moderni


############################################################
# 8) CONFRONTO CONCETTUALE
############################################################

# for loop:
# - molto esplicito
# - ottimo per imparare la logica
# - utile quando servono passaggi intermedi chiari
#
# lapply():
# - più compatto
# - restituisce una lista
#
# map():
# - simile a lapply()
# - sintassi spesso più gradevole nel tidyverse



############################################################
# PIVOT_LONGER + FACET_WRAP
############################################################

# Finora abbiamo fatto "un grafico per gruppo"
# ripetendo più volte la stessa operazione.
#
# Nel tidyverse spesso si può invece:
# 1. ristrutturare i dati
# 2. fare un unico grafico
# 3. separare i pannelli con facet_wrap()

# Creiamo un dataset lungo con due misure del becco
penguins_long <- penguins_clean |>
  select(species, bill_length_mm, bill_depth_mm) |>
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm),
    names_to = "measure",
    values_to = "value"
  )

penguins_long |>
  head()

# Grafico unico con facet_wrap:
# un pannello per misura
ggplot(penguins_long, aes(x = species, y = value)) +
  geom_boxplot() +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Two variables in long format",
    x = "Species",
    y = "Value"
  ) +
  theme_minimal()

# Invece di fare due grafici separati "a mano",
# abbiamo cambiato il formato dei dati e lasciato
# che ggplot costruisse i pannelli per noi.


############################################################
# 11) ESEMPIO FINALE PIÙ FORTE
############################################################

# Facciamo un piccolo riassunto per specie
# e poi pivot_longer + facet_wrap

penguins_summary <- penguins_clean |>
  group_by(species) |>
  summarise(
    mean_bill_length = mean(bill_length_mm),
    mean_bill_depth = mean(bill_depth_mm),
    mean_flipper_length = mean(flipper_length_mm)
  )

penguins_summary

penguins_summary_long <- penguins_summary |>
  pivot_longer(
    cols = c(mean_bill_length, mean_bill_depth, mean_flipper_length),
    names_to = "trait",
    values_to = "mean_value"
  )

ggplot(penguins_summary_long,
       aes(x = species, y = mean_value, fill = species)) +
  geom_col() +
  facet_wrap(~ trait, scales = "free_y") +
  labs(
    title = "Summary values by species",
    x = "Species",
    y = "Mean value"
  ) +
  theme_minimal()


############################################################
# 12) RIASSUNTO FINALE DELLA LEZIONE
############################################################

# for loop     -> ripetizione esplicita
# lapply()     -> ripetizione su lista/vettore
# map()        -> versione tidyverse della stessa idea
# pivot_longer -> cambia forma ai dati
# facet_wrap() -> evita di costruire tanti grafici a mano

###ESERCIZI

library(palmerpenguins)

palmerpenguins::penguins

#Creare una lista con i cubi dei numeri da 1 a 5 con loop e lapply
#####
out <- vector("list", 5)

for(i in 1:5) {
  out[[i]] <- i^3
}

out

out <- lapply(1:5, function(i)i^3)

#Cosa cambia tra i due? 


#Cosa restituisce questo codice?
lapply(1:4, function(x) x + 10)

#Perchè questo codice non funziona?

out <- vector("list", 3)

for(i in 1:3) {
  out[i] <- i^2
}

#invece dei grafici clacolare la media di bill_lenght_mm per specie

lapply(species_list, function(sp) {
  penguins_clean |>
    filter(species == sp) |>
    summarise(mean_bill = mean(bill_length_mm, na.rm = TRUE))
})



#trasforma questo for i in lapply() e map()
#####
for(i in seq_along(species_list)) { 
  
  sp <- species_list[i] 
  
  print(sp) }

species_list <- lapply(species_list, function(sp) print(sp))


map(species_list, function(sp){
  
  out<- print(sp)
  
  return(out)
  
})

