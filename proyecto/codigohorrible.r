#Proyecto AED
library(readr)
Songs <- read_csv("macc/2025-1/a n á l i s i s  e s t a d í s t i c o  d e  d a t o s/Most Streamed Spotify Songs 2024.csv", col_types = cols(`Release Date` = col_date(format = "%m/%d/%Y"), ISRC = col_skip(), `TIDAL Popularity` = col_skip()))
View(Songs)

dim(Songs)
lapply(Songs, class) #para saber de que tipo es cada variable
summary(Songs)

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
        col=c("lightblue", "salmon"))


