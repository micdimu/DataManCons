library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)
library(ggspatial)
library(palmerpenguins)
library(tidygeocoder)
library(tibble)
library(ggrepel)

data(penguins)

glimpse(penguins)

# n observation per island

isl_obs <- penguins |>
  count(island) |> 
  mutate(island = paste(island, "Island")) |> 
  add_column(state = "Antarctica", .after = "island")

island_coords <- isl_obs |>
  geocode(
    street = island,
    method = "arcgis",
    country = state,
    return_input = F,
    limit = NULL) |> 
  distinct(street, .keep_all = T) |>
  select(island = street, long = long, lat = lat) |> 
  full_join(isl_obs, by = "island") 

# verificareNULL# verificare che tutte le isole abbiano coordinate valide.

island_coords |>
  select(island, long, lat)

# Trasformiamo le coordinate delle isole in oggetto spaziale sf.

island_coords_sf <- island_coords |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = 3031) |> 
  (\(.) add_column(., long = st_coordinates(.)[,1],
                   lat = st_coordinates(.)[,2],
                   .after = "island"))() 
  

island_coords |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = 3031) %>%
  add_column( coord = st_coordinates(.), .after = "island")


island_A <- island_coords |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = 3031)

island_A |> 
  add_column(coord = st_coordinates(.), .after = "island")
  

# Natural Earth fornisce confini geografici semplici e leggeri.

Antarctica <- ne_countries(scale = 10, returnclass = "sf", continent = "Antarctica")|> 
  st_transform(crs = 3031)

#### How to create a beautiful map? #####

zoom_bbox <- island_coords_sf |> 
  st_buffer(dist = 100000) |>
  st_bbox()

#### MAPPA PRINCIPALE ####

p_main <- ggplot() +
  geom_sf(
    data = Antarctica,
    fill = "#7f93ad",
    color = NA
  ) +
  geom_sf(
    data = island_coords_sf,
    aes(colour = island),
    alpha = 0.9,
    size = 3
  ) +
  geom_text_repel(
    data = island_coords_sf,
    aes(x = long, y = lat, label = island, colour = island),
    size = 5,
    fontface = "bold"
  ) +
  scale_color_discrete(palette = "Dark2")+
  coord_sf(
    xlim = zoom_bbox[c(1,3)],
    ylim = zoom_bbox[c(2,4)],
    expand = FALSE
  ) +
  labs(
    title = "Penguins in Palmer Archipelago",
    subtitle = "Nesting islands of penguins recorded between 2007 and 2009",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.position = "none")
  
p_main
 

# CRS polare antartico
crs_ant <- 3031



grat <- st_graticule(
  lon = seq(-180, 180, by = 10),
  lat = seq(-90, -40, by = 5),
  crs = st_crs(4326)
) |> 
  st_transform(3031)

# Inset corretto
p_inset <- ggplot() +
  geom_sf(
    data = Antarctica,,
    fill = "#a9b7c7",
    color = NA
  ) +
  geom_sf(
    data = island_coords_sf,
    fill = NA,
    color = "black",
    linewidth = 0.7
   ) +
  # geom_sf(data = grat,
  #         color = "gray30",
  #         linewidth = 0.1,
  #         linetype = 2) +
  theme_void()

p_inset



p_final <- p_main +
  inset_element(
    p_inset,
    left = 0,
    bottom = 0,
    right = 0.45,
    top = 0.5
  )

p_final
