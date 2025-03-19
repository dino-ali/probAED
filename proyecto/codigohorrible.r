#PROYECTO AED: 2025-1
library(readr)
MSSS2024 <- read_csv("ali♡/Most Streamed Spotify Songs 2024.csv")
View(MSSS2024)

library(skimr)
skim(MSSS2024)
summary(MSSS2024)

library(dplyr)
# Eliminar varias columnas
MSSS2024 <- MSSS2024 %>% select(-ISRC)
library(janitor)
MSSS2024 <- MSSS2024 %>% clean_names()
View(MSSS2024)

# Cargar la librería ggplot2
library(ggplot2)

# Crear gráfico de barras
ggplot(MSSS2024, aes(x = artist)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Cantidad de Canciones por Artista", x = "Artista", y = "Cantidad")
MSSS2024 %>%
  count(artist, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(artist, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top 10 Artistas con más Canciones", x = "Artista", y = "Cantidad") +
  coord_flip()  # Para girar el gráfico y hacerlo más legible
