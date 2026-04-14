############################################################
# LEZIONE: WRITING FUNCTIONS IN R
library(dplyr)
library(tidyr)
library(ggplot2)
library(palmerpenguins)
library(patchwork)

data(penguins)

############################################################
# 1) PICCOLO RECAP
############################################################

# Nelle lezioni precedenti abbiamo visto che con tidyverse possiamo:
# - selezionare colonne         -> select()
# - filtrare righe              -> filter()
# - creare nuove colonne        -> mutate()
# - creare categorie            -> case_when()
# - riassumere per gruppi       -> group_by() + summarise()
# - cambiare forma ai dati      -> pivot_longer() / pivot_wider()

penguins |> 
  glimpse()

penguins |> 
  str()

penguins |> 
  dim()

############################################################
# 2) PERCHÉ SCRIVERE UNA FUNZIONE?
############################################################

# Immaginiamo di voler fare un grafico solo per la specie Adelie

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

# Funziona.
# Ma se vogliamo rifarlo per Gentoo, Chinstrap, oppure per ogni isola,
# stiamo riscrivendo quasi sempre lo stesso codice.

# Qui nasce l'idea di funzione:
# "metto in una scatola il codice che si ripete,
# così posso riusarlo cambiando solo qualche pezzo"


############################################################
# 3) STRUTTURA BASE DI UNA FUNZIONE
############################################################

# Una funzione si costruisce così:
#
# nome_funzione <- function(argomento) {
#   codice
# }

# Facciamo il primo esempio più semplice possibile

say_hello <- function(nome) {
  paste("Hello", nome)
}

say_hello("Michele")
say_hello("Adelie")

# In questo caso:
# - say_hello è il nome della funzione
# - nome è l'argomento
# - la funzione restituisce una frase


############################################################
# 4) FUNZIONE CHE RESTITUISCE UN DATA FRAME PER OGNI SPECIE
############################################################

# Ora facciamo una funzione un po' più interessante:
# dato il nome di una specie,
# restituisce alcune statistiche riassuntive

species_summary <- function(species_name) {
  
  penguins |>
    filter(species == species_name) |>
    summarise(
      n = n(),
      mean_bill_length = mean(bill_length_mm, na.rm = T),
      mean_bill_depth = mean(bill_depth_mm, na.rm = T),
      mean_flipper = mean(flipper_length_mm, na.rm = T),
      mean_body_mass = mean(body_mass_g, na.rm = T)
    )
  
}

species_summary("Adelie")
species_summary("Gentoo")

# Questa funzione non restituisce più un numero singolo,
# ma una piccola tabella

############################################################
# 5) FUNZIONE CHE CREA UN GRAFICO
############################################################

# Adesso iniziamo con qualcosa di più divertente:
# una funzione che fa uno scatterplot per una specie scelta

plot_species <- function(species_name) {
  
  penguins |>
    filter(species == species_name) |>
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point(size = 2) +
    labs(
      title = paste("Penguins of species:", species_name),
      x = "Bill length (mm)",
      y = "Bill depth (mm)"
    ) +
    theme_minimal()
  
}

plot_species("Adelie")
plot_species("Gentoo")

# Questa è una funzione molto utile da far vedere,
# perché gli studenti capiscono subito il vantaggio:
# stesso schema, specie diversa


############################################################
# 6) SECONDA FUNZIONE GRAFICA: ISOLA PER ISOLA - RETURN
############################################################

plot_island <- function(island_name) {
  
  # 1) filtro i dati
  dat_i <- penguins |>
    filter(island == island_name)
  
  # 2) creo il grafico
  p_i <- ggplot(dat_i, aes(x = flipper_length_mm, y = body_mass_g)) +
    geom_point(size = 2) +
    labs(
      title = paste("Penguins on island:", island_name),
      x = "Flipper length (mm)",
      y = "Body mass (g)"
    ) +
    theme_minimal()
  
  # 3) restituisco entrambi come lista
  return(list(
    data = dat_i,
    plot = p_i
  ))
}


plot_island("Dream")
plot_island("Biscoe")

############################################################
# 7) FUNZIONE CON DUE ARGOMENTI
############################################################

plot_island2 <- function(data = penguins, island_name) {
  
  # 1) filtro i dati
  dat_i <- data |>
    filter(island == island_name)
  
  # 2) creo il grafico
  p_i <- ggplot(dat_i, aes(x = flipper_length_mm, y = body_mass_g)) +
    geom_point(size = 2) +
    labs(
      title = paste("Penguins on island:", island_name),
      x = "Flipper length (mm)",
      y = "Body mass (g)"
    ) +
    theme_minimal()
  
  # 3) restituisco entrambi come lista
  return(list(
    data = dat_i,
    plot = p_i
  ))
}


plot_island2(data = penguins , "Dream")
plot_island2(data = penguins , "Biscoe")

############################################################
# 8) FUNZIONE CON N ARGOMENTI (N>2)
############################################################


plot_species_vars <- function(data, species_name, x_var, y_var) {
  
  dat_i <- penguins |>
    filter(species == species_name)
  
  fig <- ggplot(dat_i, aes_string(x = x_var, y = y_var)) +
    geom_point(size = 2) +
    labs(
      title = paste(species_name, "-", x_var, "vs", y_var),
      x = x_var,
      y = y_var
    ) +
    theme_minimal()
  
  rtn <- list(df = dat_i, figure = fig)
  return(rtn)
}

# Esempi
plot_species_vars(data = penguins, "Adelie", "bill_length_mm", "bill_depth_mm")
plot_species_vars(data = penguins, "Gentoo", "flipper_length_mm", "body_mass_g")

############################################################
# 9) FUNZIONE CON IF ELSE
############################################################

plot_species_type <- function(data, species_name, plot_type) {
  
  dat_i <- data |>
    filter(species == species_name)
  
  if(plot_type == "scatter") {
    
    p <- ggplot(dat_i, aes(x = bill_length_mm, y = bill_depth_mm)) +
      geom_point() +
      labs(title = paste("Scatter:", species_name))
    
  } else if(plot_type == "hist") {
    
    p <- ggplot(dat_i, aes(x = body_mass_g)) +
      geom_histogram(bins = 20) +
      labs(title = paste("Histogram:", species_name))
    
  } else if(plot_type == "box") {
    
    p <- ggplot(dat_i, aes(x = sex, y = body_mass_g)) +
      geom_boxplot() +
      labs(title = paste("Boxplot:", species_name))
    
  }
  
  return(p + theme_minimal())
}

plot_species_type(data = penguins, "Adelie", "scatter")
plot_species_type(data = penguins, "Gentoo", "hist")
plot_species_type(data = penguins, "Chinstrap", "box")

############################################################
# 9) FUNZIONE CON LOOP INSIDE
############################################################

plot_hist_by_species_var <- function(data, var) {
  
  species_list <- unique(data$species)
  plot_list <- vector("list", length(species_list))
  
  for(i in seq_along(species_list)) {
    
    sp <- species_list[i]
    
    dat_i <- data |>
      filter(species == sp)
    
    p_i <- ggplot(dat_i, aes(x = .data[[var]])) +
      geom_histogram(bins = 20) +
      labs(
        title = paste(var, "-", sp),
        x = var,
        y = "Count"
      ) +
      theme_minimal()
    
    plot_list[[i]] <- p_i
  }
  
  wrap_plots(plot_list)
}

plot_hist_by_species_var(penguins, "body_mass_g")
plot_hist_by_species_var(penguins, "flipper_length_mm")

############################################################
# 11) UNIRE PIÙ GRAFICI
############################################################

# Possiamo anche usare le funzioni per costruire rapidamente
# più grafici e poi unirli

plot_species("Adelie") +
  plot_species("Gentoo") +
  plot_species("Chinstrap")

# Oppure
fun_body_class_plot("Adelie") +
  fun_body_class_plot("Gentoo")

############################################################
# 11) LOOP FUNCTION
############################################################

c("Adelie", "Gentoo") |>
  map(plot_species)

c("Dream", "Biscoe") |>
  map(plot_island2, data = penguins)

############################################################
# 13) PROMEMORIA SULLA LOGICA DELLE FUNZIONI
############################################################

# Una funzione:
# - prende uno o più argomenti
# - esegue codice
# - restituisce un output
#
# Gli argomenti sono i pezzi che vogliamo cambiare.
# Tutto il resto del codice rimane stabile.


############################################################
# 16) ESERCIZI
############################################################

# Usare sempre penguins_clean


### ESERCIZIO 1
# Scrivere una funzione che, dato il nome di una specie,
# restituisca il numero di osservazioni


### ESERCIZIO 2
# Scrivere una funzione che, dato il nome di un'isola,
# restituisca la media di body_mass_g


### ESERCIZIO 3
# Scrivere una funzione che faccia uno scatterplot
# di flipper_length_mm vs body_mass_g per una specie


### ESERCIZIO 4
# Scrivere una funzione che crei una colonna con classi
# di bill_length_mm (short / medium / long) usando case_when()


### ESERCIZIO 5
# Scrivere una funzione che per una specie restituisca
# una tabella long con le medie di almeno tre variabili numeriche


### ESERCIZIO 6
# Scrivere una funzione con due argomenti:
# species_name e point_color,
# che costruisca uno scatterplot personalizzato


### ESERCIZIO 7
# Scrivere una funzione che restituisca un boxplot
# di body_mass_g per sex in una specie scelta


### ESERCIZIO 8
# Scrivere una funzione "giocosa" che inventi tre classi
# personalizzate sulla base di una variabile numerica
# e poi le rappresenti con un barplot

