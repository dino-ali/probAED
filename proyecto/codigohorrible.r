#Proyecto AED
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
Songs <- read_csv("macc/2025-1/a n á l i s i s  e s t a d í s t i c o  d e  d a t o s/Most Streamed Spotify Songs 2024.csv", col_types = cols(`Release Date` = col_date(format = "%m/%d/%Y"), ISRC = col_skip(), `TIDAL Popularity` = col_skip()))
View(Songs)

dim(Songs)
lapply(Songs, class) #para saber de que tipo es cada variable
summary(Songs)

# ✨UNIVARIADO✨_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#HISTOGRAMAS________________________________________________________________________________________________________________________________________-
par(mfrow=c(2,3))  # Divide la ventana en 2x3 para ver varios histogramas juntos

#¿Cómo se distribuye la cantidad de reproducciones en Spotify?
hist(Songs$`Spotify Streams`, main="Distribución de Streams en Spotify",
     xlab="Número de Streams", ylab="Frecuencia", col="skyblue", breaks=30)

#¿Existen canciones con muchas más vistas que otras?
hist(Songs$`YouTube Views`, main="Distribución de Vistas en YouTube",
     xlab="Número de Vistas", ylab="Frecuencia", col="lightcoral", breaks=30)

#¿Cómo varía la cantidad de vistas en TikTok entre las canciones?
hist(Songs$`TikTok Views`, main="Distribución de Reproducciones en TikTok",
     xlab="Número de Vistas", ylab="Frecuencia", col="lightgreen", breaks=30)

#¿Cómo se distribuye la cantidad de reproducciones en SoundCloud?
hist(Songs$`Soundcloud Streams`, main="Distribución de Streams en SoundCloud",
     xlab="Número de Reproducciones", ylab="Frecuencia", col="plum", breaks=30)

#¿Cuántas veces han sido identificadas las canciones en Shazam?
hist(Songs$`Shazam Counts`, main="Distribución de Búsquedas en Shazam",
     xlab="Cantidad de Búsquedas en Shazam", ylab="Frecuencia", col="purple", breaks=30)

#¿Cómo se distribuye el puntaje de popularidad en Spotify?
hist(Songs$`Spotify Popularity`, main="Distribución de Popularidad en Spotify",
     xlab="Popularidad en Spotify", ylab="Frecuencia", col="gold", breaks=30)

# Cuantas canciones se lanzaron por año
Songs$Release_Year = format(Songs$`Release Date`, "%Y")  # Extraer año
hist(as.numeric(Songs$Release_Year), breaks=10, col="purple",
     main="Cantidad de Canciones Lanzadas por Año",
     xlab="Año", ylab="Cantidad de Canciones", border="white")


par(mfrow=c(1,1))  # Restablece la vista a 1 gráfico

#DIAGRAMAS DE BARRA_______________________________________________________________________________________________________________________________________
# Tabla de frecuencia de artistas
artistas_freq = table(Songs$Artist)

# Artistas con más de 10 canciones
artistas_mas_10 <- artistas_freq[artistas_freq > 10]
barplot(artistas_mas_10, las=2, col="palevioletred", main="Artistas con más de 10 canciones",
        xlab="Artista", ylab="Cantidad de Canciones")

# Ordenar de mayor a menor y seleccionar los 10 más frecuentes
top_artistas <- sort(artistas_freq, decreasing=TRUE)[1:10]
barplot(top_artistas, main="Top 10 Artistas con Más Canciones",
        xlab="Artista", ylab="Cantidad de Canciones", col="orchid", las=2)

# Crear tabla con frecuencias absolutas y relativas de artistas
tabla_artistas = as.data.frame(artistas_freq)
colnames(tabla_artistas) = c("Artist", "Frequency")
tabla_artistas$Relative_Frequency <- prop.table(artistas_freq) * 100
print(tabla_artistas)

# Canciones explícitas vs. no explícitas
barplot(table(Songs$`Explicit Track`), 
        main="Cantidad de Canciones Explícitas vs. No Explícitas", 
        col=c("lightblue", "salmon"),
        ylab="Frecuencia",
        xlab="Tipo de Canción")

barplot(table(Songs$`All Time Rank`), 
        main="Distribución de Rankings de Canciones",
        xlab="Ranking", ylab="Frecuencia", col="gold")

# Tabla de frecuencia para el top 20 álbumes
album_counts = table(Songs$`Album Name`)  
top_albums = as.data.frame(sort(album_counts, decreasing=TRUE)[1:20])  
colnames(top_albums) = c("Album", "Freq") 

barplot(top_albums$Freq, names.arg=top_albums $Album, las=2, col="thistle", 
        main="10 Álbumes más representados", ylab="Frecuencia", cex.names=0.8)


total_streams = colSums(Songs[, c("Spotify Streams", "YouTube Views", "TikTok Views", "Soundcloud Streams")], na.rm=TRUE)

barplot(total_streams, 
        main="Total de Streams por Plataforma", 
        ylab="Número de Streams",
        col=c("blueviolet", "steelblue", "orchid", "deeppink"),
        names.arg=c("Spotify", "YouTube", "TikTok", "SoundCloud"))

#DIAGRAMAS DE CAJA_____________________________________________________________________________________________
boxplot(Songs$`Spotify Streams` ~ Songs$`Explicit Track`, 
        main="Distribución de Streams según Explicit Lyrics",
        xlab="Explícita (Sí/No)", 
        ylab="Número de Streams",
        col=c("seagreen", "mediumvioletred"))


# Crear un diagrama de cajas y bigotes para la distribución de streams en varias plataformas (SIN LOGARITMO)
boxplot(Songs$`Spotify Streams`, Songs$`YouTube Views`, Songs$`TikTok Views`, Songs$`Soundcloud Streams`,
        names=c("Spotify", "YouTube", "TikTok", "SoundCloud"),  # Etiquetas para cada caja
        col=c("darkorchid", "hotpink", "plum", "lightskyblue"),  # Colores para cada plataforma
        main="Distribución de Streams por Plataforma",  # Título del gráfico
        ylab="Número de Streams",  # Etiqueta del eje Y
        notch=TRUE)  # Incluir muescas para comparar medianas

# Crear un diagrama de cajas y bigotes para la distribución de streams en varias plataformas
boxplot(log10(Songs$`Spotify Streams`), log10(Songs$`YouTube Views`), log10(Songs$`TikTok Views`), log10(Songs$`Soundcloud Streams`),
        names=c("Spotify", "YouTube", "TikTok", "SoundCloud"),
        col=c("darkorchid", "hotpink", "plum", "lightskyblue"),
        main="Distribución de Streams por Plataforma (Escala Log)",
        ylab="Log(Streams)",
        notch=TRUE)


# Crear un diagrama de cajas y bigotes para la distribución de búsquedas en Shazam (SIN LOGARITMO)
boxplot(Songs$`Shazam Counts`,
        col="purple",  # Color del gráfico
        main="Distribución de Búsquedas en Shazam",  # Título del gráfico
        ylab="Cantidad de Búsquedas",  # Etiqueta del eje Y
        notch=TRUE)  # Incluir muescas para comparar medianas
# Crear un diagrama de cajas y bigotes para la distribución de búsquedas en Shazam
boxplot(log10(Songs$`Shazam Counts`),
        col="purple",  # Color del gráfico
        main="Distribución de Búsquedas en Shazam",  # Título del gráfico
        ylab="Cantidad de Búsquedas",  # Etiqueta del eje Y
        notch=TRUE)  # Incluir muescas para comparar medianas

#graficos aparte xd____________________________
# Ordenar datos por fecha
Songs  Songs[order(Songs$`Release Date`), ] 

plot(Songs$`Release Date`, Songs$`Spotify Streams`, type="l", col="paleturquoise", lwd=2,
     main="Evolución de Streams en el Tiempo",
     xlab="Fecha de Lanzamiento", ylab="Streams")

points(Songs$`Release Date`, Songs$`Spotify Streams`, col="lightcoral", pch=16)  # Agregar puntos

# ✨BIVARIADO✨_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
# Relación entre "Spotify Streams" y "Explicit"
Songs$Explicit <- as.factor(Songs$`Explicit Track`)  # Convertir a factor

table(Songs$Explicit) # Tabla de Frecuencia

# Tabla Cruzada de Streams según si son Explícitas o no
tapply(Songs$`Spotify Streams`, Songs$Explicit, summary)

# Gráfico Comparativo (Boxplot)
boxplot(Songs$`Spotify Streams` ~ Songs$Explicit, col=c("lightblue", "pink"),
        main="Distribución de Streams según Explicit",
        xlab="¿Es Explícita?", ylab="Spotify Streams")


# Relación entre "Spotify Streams" y "Release Date"  
Songs$`Release Year` <- format(Songs$`Release Date`, "%Y")  # Extraer el año de lanzamiento

table(Songs$`Release Year`) # Tabla de Frecuencia de Canciones por Año

# Gráfico de Dispersión
plot(as.numeric(Songs$`Release Year`), Songs$`Spotify Streams`,
     main="Relación entre Release Year y Spotify Streams",
     xlab="Release Year", ylab="Spotify Streams", col="lightsteelblue", pch=16)

#_________________________________________--
#  Tabla de doble entrada:
# Crear categorías basadas en la cantidad de likes en TikTok
LikesTT = Songs %>%
  mutate(categoria_tiktok = case_when(
    `TikTok Likes` <= 1000 ~ "Bajo",
    `TikTok Likes` > 1000 & `TikTok Likes` <= 10000 ~ "Medio",
    `TikTok Likes` > 10000 ~ "Alto"
  ))

explicit_TT = table(LikesTT$Explicit, LikesTT$categoria_tiktok); explicit_TT

ggplot(LikesTT, aes(x = as.factor(Explicit), fill = categoria_tiktok)) +
  geom_bar(position = "dodge") +
  labs(title = "Relación entre contenido explícito y popularidad en TikTok",
       x = "Explícita (0=No, 1=Sí)", 
       y = "Frecuencia", 
       fill = "Popularidad en TikTok") +
  scale_fill_manual(values = c("Bajo" = "thistle", "Medio" = "paleturquoise", "Alto" = "darkcyan", "NA" = "slateblue")) +  
  theme_minimal()


ggplot(Songs, aes(x = `YouTube Views`, y = `Spotify Streams`)) +
  geom_point(alpha = 0.4, color = "slateblue") +
  labs(title = "Relación entre streams de YouTube y Spotify",
       x = "Streams en YouTube", 
       y = "Streams en Spotify") +
  scale_x_log10() + scale_y_log10() +  # Escala logarítmica para mejorar visualización
  theme_minimal()

#Streams vs. fecha de lanzamiento
ggplot(Songs, aes(x = `Release Date`, y = `Spotify Streams`)) +
  geom_point(alpha = 0.3, color = "forestgreen") +
  labs(title = "Relación entre fecha de lanzamiento y streams en Spotify",
       x = "Fecha de lanzamiento", y = "Streams Spotify") +
  scale_y_log10() +
  theme_minimal()

# Análisis Descriptivo y Exploratorio de las Variables:_________________________________________________________________________________________________________________________________________________________________________________________________________
#Medidas de Tendencia Central
#R  no tiene moda, entonces la creamos
Mode = function(x) {
  ux = unique(na.omit(x))  # Eliminar valores NA
  freq = tabulate(match(x, ux))  # Contar frecuencias
  ux[which.max(freq)]  # Devolver el más frecuente
}
# Creamos la tabla con medias, medianas y modas
summary_table <- data.frame(
  Variable = c("Spotify Streams", "YouTube Views", "TikTok Likes", "Shazam Counts", "Soundcloud Streams"),
  Media = c(mean(Songs$`Spotify Streams`, na.rm = TRUE),
            mean(Songs$`YouTube Views`, na.rm = TRUE),
            mean(Songs$`TikTok Likes`, na.rm = TRUE),
            mean(Songs$`Shazam Counts`, na.rm = TRUE),
            mean(Songs$`Soundcloud Streams`, na.rm = TRUE)),
  Mediana = c(median(Songs$`Spotify Streams`, na.rm = TRUE),
              median(Songs$`YouTube Views`, na.rm = TRUE),
              median(Songs$`TikTok Likes`, na.rm = TRUE),
              median(Songs$`Shazam Counts`, na.rm = TRUE),
              median(Songs$`Soundcloud Streams`, na.rm = TRUE)),
  Moda = c(Mode(Songs$`Spotify Streams`),
           Mode(Songs$`YouTube Views`),
           Mode(Songs$`TikTok Likes`),
           Mode(Songs$`Shazam Counts`),
           Mode(Songs$`Soundcloud Streams`))
)

library(knitr)
kable(summary_table, caption = "Medidas de tendencia central por variable")


# Cuartiles
quartiles <- data.frame(
  Variable = c("Spotify Streams", "YouTube Views", "TikTok Likes", "Shazam Counts", "Soundcloud Streams"),
  Q1 = c(quantile(Songs$`Spotify Streams`, 0.25, na.rm = TRUE),
         quantile(Songs$`YouTube Views`, 0.25, na.rm = TRUE),
         quantile(Songs$`TikTok Likes`, 0.25, na.rm = TRUE),
         quantile(Songs$`Shazam Counts`, 0.25, na.rm = TRUE),
         quantile(Songs$`Soundcloud Streams`, 0.25, na.rm = TRUE)),
  Q2 = c(quantile(Songs$`Spotify Streams`, 0.50, na.rm = TRUE),  # Mediana
         quantile(Songs$`YouTube Views`, 0.50, na.rm = TRUE),
         quantile(Songs$`TikTok Likes`, 0.50, na.rm = TRUE),
         quantile(Songs$`Shazam Counts`, 0.50, na.rm = TRUE),
         quantile(Songs$`Soundcloud Streams`, 0.50, na.rm = TRUE)),
  Q3 = c(quantile(Songs$`Spotify Streams`, 0.75, na.rm = TRUE),
         quantile(Songs$`YouTube Views`, 0.75, na.rm = TRUE),
         quantile(Songs$`TikTok Likes`, 0.75, na.rm = TRUE),
         quantile(Songs$`Shazam Counts`, 0.75, na.rm = TRUE),
         quantile(Songs$`Soundcloud Streams`, 0.75, na.rm = TRUE))
)

kable(quartiles, caption = "Cuartiles de cada variable")

