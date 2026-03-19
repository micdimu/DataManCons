library(ggplot2)
library(dplyr)

# =========================================================
# UOVO DI PASQUA con ggplot2
# =========================================================
t <- seq(0, 2*pi, length.out = 1000)

egg <- tibble(
  x = sin(t) * (1 - 0.25 * cos(t)),  # 👈 cambia il segno qui
  y = cos(t)
) |>
  mutate(
    x = x * 0.7,
    y = y * 1.1 - 0.1
  )

# ---------------------------------------------------------
# Funzione: punti dentro l'uovo (IMPORTANTE)
# ---------------------------------------------------------
inside_egg <- function(x, y) {
  # stessa logica della forma (approssimata)
  ymax <- 1.05 * sqrt(pmax(0, 1 - (x / 0.75)^2)) - 0.1
  abs(y) < ymax
}

# ---------------------------------------------------------
# POIS (generati random ma filtrati dentro l’uovo)
# ---------------------------------------------------------

set.seed(42)

pois <- tibble(
  x = c(-0.28,  0.00,  0.28,
        -0.20,  0.20,
        -0.30,  0.00,  0.30),
  y = c( 0.58,  0.62,  0.58,
         -0.02,  -0.02,
         -0.56, -0.60, -0.56),
  size = c(6, 8, 6, 7, 7, 6, 8, 6),
  fill = c("#F4D35E", "#EE964B", "#F95738",
           "#8D99AE", "#70C1B3",
           "#F4D35E", "#EE964B", "#F95738")
)

# 4. Fiorellini semplici
flowers <- tibble(
  x = c(-0.38, 0.38, -0.34, 0.34),
  y = c(0.30, 0.30, -0.28, -0.28),
  size = c(7, 7, 7, 7)
)

# 5. Ombra sotto l'uovo
shadow <- tibble(
  x = seq(-0.40, 0.40, length.out = 300),
  y = -1.24 + 0.05 * sqrt(pmax(0, .5 - (seq(-0.38, 0.38, length.out = 300)/0.38)^2))
)

# ---------------------------------------------------------
# Bande decorative
# ---------------------------------------------------------
make_band <- function(y_center, amp = 0.03, freq = 12, phase = 0) {
  x <- seq(-0.9, 0.9, length.out = 1000)
  
  y <- y_center + amp * sin(freq * x + phase)
  
  # riporta y alla scala interna del parametro cos(t)
  u <- (y + 0.1) / 1.1
  
  # semi-larghezza dell'uovo a quella quota y
  xmax <- 0.7 * sqrt(pmax(0, 1 - u^2)) * (1 - 0.25 * u)
  
  tibble(
    x = x,
    y = ifelse(abs(x) <= xmax, y, NA)
  )
}

band1 <- make_band( 0.45, freq = 16)
band2 <- make_band( 0.15, freq = 12, phase = 1)
band3 <- make_band(-0.15, freq = 18, phase = 2)
band4 <- make_band(-0.45, freq = 10)
band5 <- make_band(-0.75, freq = 10)

# =========================================================
# PLOT
# =========================================================

ggplot() +
  annotate("rect", xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
           fill = "#FFF8F0", color = NA) +
  geom_polygon(data = shadow, aes(x, y),
               fill = "grey70", alpha = 0.25) +
  geom_polygon(data = egg, aes(x, y),
               fill = "#FCECC9",
               color = "#8C5E3C",
               linewidth = 1.4) +
  # bande
  geom_line(data = band1, aes(x, y), color = "#D1495B", linewidth = 2) +
  geom_line(data = band2, aes(x, y), color = "#00798C", linewidth = 2) +
  geom_line(data = band3, aes(x, y), color = "#EDAe49", linewidth = 2) +
  geom_line(data = band4, aes(x, y), color = "#66A182", linewidth = 2) +
  geom_line(data = band5, aes(x, y), color = "#E3475F", linewidth = 2) +
  
  # pois
  geom_point(data = pois,
             aes(x, y, size = size, fill = fill),
             shape = 21, color = "#8C5E3C", stroke = 1.1,
             show.legend = FALSE) +
  scale_fill_identity() +
  scale_size_identity() +
  
  # fiori (overlay sui pois)
  geom_point(data = flowers,
             aes(x, y),
             shape = 8, size = 5,
             color = "#D1495B", stroke = 1.3) +
  geom_point(data = flowers,
             aes(x, y),
             shape = 16, size = 1.8,
             color = "#FCECC9") +
  
  annotate("text", x = 0, y = 1.25,
           label = "Buona Pasqua",
           size = 8, fontface = "bold",
           color = "#8C5E3C") +
  
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.3, 1.4)) +
  theme_void()
  
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html
