#Leer el archivo baseFINAGRO con la librería readr
library(readr)
baseFINAGRO <- read.csv2("URosario/MACC/Semestre 4 2024-2/Probabilidad y Estadística 2/Proyecto Final/Proyecto Proba/baseFINAGRO.csv", header = TRUE)
View(baseFINAGRO)

#Dimensiones de la base de datos
str(baseFINAGRO)
dim(baseFINAGRO)

#Datos filtrados omitiendo los valores nulos
datos_flitrados <- na.omit(baseFINAGRO)
datos_flitrados

#Librerías
library(dplyr)
library(data.table)
install.packages("DescTools")
library(DescTools)
install.packages("summarytools")
library(summarytools)
install.packages("e1071")
library(e1071)
install.packages("ggplot2")
library(ggplot2)

#Selección de las variables categórticas
var_cat <- datos_flitrados %>% select("Año", "Fuente.Colocación", "Tipo.Productor", "Línea.de.Crédito", "Género", "Municipio.de.PostConflico.")
var_cat

#Las variables tipo caracter son representadas a través de números
var_cat_año <- as.numeric(factor(var_cat$Año, levels = c("2.021", "2.022", "2.023")))
var_cat_año
var_cat_fuente <- as.numeric(factor(var_cat$Fuente.Colocación, levels = c("AGROPECUARIA", "REDESCUENTO", "SUSTITUTA")))
var_cat_fuente
var_cat_tipo <- as.numeric(factor(var_cat$Tipo.Productor, levels = c("PEQUEÑO", "PEQUEÑO PPIB", "MEDIANO", "GRANDE")))
var_cat_tipo
var_cat_linea <- as.numeric(factor(var_cat$Línea.de.Crédito, levels = c("Inversión", "Capital de Trabajo", "Normalización de Cartera")))
var_cat_linea
var_cat_gen <- as.numeric(factor(var_cat$Género, levels = c("H", "M", "S")))
var_cat_gen
var_cat_post <- as.numeric(factor(var_cat$Municipio.de.PostConflico., levels = c("N", "S")))
var_cat_post

#Se actuliza la selección de las variables categóricas ahora representas con números
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
var_cat_act

#Creación tabla de frecuencias
#Frecuencia Absoluta para todas las variabales categóricas
tablas_frec_var_cat_act <- table(var_cat_act)
View(tablas_frec_var_cat_act)

#Tabla de frecuencias para cada variable categórica
#Año
tablas_frec_var_cat_act.año <- freq(var_cat_act$Año)
tablas_frec_var_cat_act.año2 <- round(tablas_frec_var_cat_act.año, 3)
View(tablas_frec_var_cat_act.año2)

#Fuente Colocación
tablas_frec_var_cat_act.fuente <- freq(as.data.frame(var_cat_act$Fuente.Colocación))
tablas_frec_var_cat_act.fuente2 <- round(tablas_frec_var_cat_act.fuente, 3)
View(tablas_frec_var_cat_act.fuente2)

#Tipo Productor
tablas_frec_var_cat_act.prod <- freq(var_cat_act$Tipo.Productor)
tablas_frec_var_cat_act.prod2 <- round(tablas_frec_var_cat_act.prod, 3)
View(tablas_frec_var_cat_act.prod2)

#Línea de Crédito
tablas_frec_var_cat_act.linea <- freq(var_cat_act$Línea.de.Crédito)
tablas_frec_var_cat_act.linea2 <- round(tablas_frec_var_cat_act.linea, 3)
View(tablas_frec_var_cat_act.linea2)

#Género
tablas_frec_var_cat_act.gen <- freq(var_cat_act$Género)
tablas_frec_var_cat_act.gen2 <- round(tablas_frec_var_cat_act.gen, 3)
View(tablas_frec_var_cat_act.gen2)

#Municipio PostConflicto
tablas_frec_var_cat_act.post <- freq(var_cat_act$Municipio.de.PostConflicto)
tablas_frec_var_cat_act.post2 <- round(tablas_frec_var_cat_act.post, 3)
View(tablas_frec_var_cat_act.post2)

#Estadigrafos de tendencia central
#Media
media.año <- mean(var_cat_año)
media.año2 <- round(media.año, 3)
media.año2
media.fuente <- mean(var_cat_fuente)
media.fuente2 <- round(media.fuente, 3)
media.fuente2
media.tipo <- mean(var_cat_tipo)
media.tipo2 <- round(media.tipo, 3)
media.tipo2
media.linea <- mean(var_cat_linea)
media.linea2 <- round(media.linea, 3)
media.linea2
media.gen <- mean(var_cat_gen)
media.gen2 <- round(media.gen, 3)
media.gen2
media.post <- mean(var_cat_post)
media.post2 <- round(media.post, 3)
media.post2

var_cat_media <- data.frame(Año = media.año2, Fuente.Colocación = media.fuente2, Tipo.Productor = media.tipo2, Línea.de.Crédito = media.linea2, Género = media.gen2, Municipio.de.PostConflicto = media.post2)
View(var_cat_media)

#Mediana
mediana.año <- median(var_cat_año)
mediana.año2 <- round(mediana.año, 3)
mediana.año2
mediana.fuente <- median(var_cat_fuente)
mediana.fuente2 <- round(mediana.fuente, 2)
mediana.fuente2
mediana.tipo <- median(var_cat_tipo)
mediana.tipo2 <- round(mediana.tipo, 3)
mediana.tipo2
mediana.linea <- median(var_cat_linea)
mediana.linea2 <- round(mediana.linea, 3)
mediana.linea2
mediana.gen <- median(var_cat_gen)
mediana.gen2 <- round(mediana.gen, 3)
mediana.gen2
mediana.post <- median(var_cat_post)
mediana.post2 <- round(mediana.post, 3)
mediana.post2 

var_cat_mediana <- data.frame(Año = mediana.año2, Fuente.Colocación = mediana.fuente2, Tipo.Productor = mediana.tipo2, Línea.de.Crédito = mediana.linea2, Género = mediana.gen2, Municipio.de.PostConflicto = mediana.post2)
View(var_cat_mediana)

#Moda
moda.año <- Mode(var_cat_año)
moda.año2 <- round(moda.año, 2)
moda.año2
moda.fuente <- Mode(var_cat_fuente)
moda.fuente2 <- round(moda.fuente, 2)
moda.fuente2
moda.tipo <- Mode(var_cat_tipo)
moda.tipo2 <- round(moda.tipo, 2)
moda.tipo2
moda.linea <- Mode(var_cat_linea)
moda.linea2 <- round(moda.linea)
moda.linea2
moda.gen <- Mode(var_cat_gen)
moda.gen2 <- round(moda.gen, 2)
moda.gen2
moda.post <- Mode(var_cat_post)
moda.post2 <- round(moda.post, 2)
moda.post2

var_cat_moda <- data.frame(Año = moda.año2, Fuente.Colocación = moda.fuente2, Tipo.Productor = moda.tipo2, Línea.de.Crédito = moda.linea2, Género = moda.gen2, Municipio.de.PostConflicto = moda.post2)
View(var_cat_moda)

#Estadígrafos de dispersión
#Rango
rango.año <- range(var_cat_año)
rango.año2 <- round(rango.año)
rango.año2
rango.fuente <- range(var_cat_fuente)
rango.fuente2 <- round(rango.fuente)
rango.fuente2
rango.tipo <- range(var_cat_tipo)
rango.tipo2 <- round(rango.tipo, 3)
rango.tipo2
rango.linea <- range(var_cat_linea)
rango.linea2 <- round(rango.linea, 3)
rango.linea2
rango.gen <- range(var_cat_gen)
rango.gen2 <- round(rango.gen, 3)
rango.gen2
rango.post <- range(var_cat_post)
rango.post2 <- round(rango.post, 3)
rango.post2

var_cat_rango <- data.frame(Año = rango.año2, Fuente.Colocación = rango.fuente2, Tipo.Productor = rango.tipo2, Línea.de.Crédito = rango.linea2, Género = rango.gen2, Municipio.de.PostConflicto = rango.post2)
View(var_cat_rango)

#Varianza
varianza.año <- var(var_cat_año)
varianza.año2 <- round(varianza.año, 3)
varianza.año2
varianza.fuente <- var(var_cat_fuente)
varianza.fuente2 <- round(varianza.fuente, 3)
varianza.fuente2
varianza.tipo <- var(var_cat_tipo)
varianza.tipo2 <- round(varianza.tipo, 3)
varianza.tipo2
varianza.linea <- var(var_cat_linea)
varianza.linea2 <- round(varianza.linea, 3)
varianza.linea2
varianza.gen <- var(var_cat_gen)
varianza.gen2 <- round(varianza.gen, 3)
varianza.gen2
varianza.post <- var(var_cat_post)
varianza.post2 <- round(varianza.post, 3)
varianza.post2

var_cat_varianza <- data.frame(Año = varianza.año2, Fuente.Colocación = varianza.fuente2, Tipo.Productor = varianza.tipo2, Línea.de.Crédito = varianza.linea2, Género = varianza.gen2, Municipio.de.PostConflicto = varianza.post2)
View(var_cat_varianza)

#Desviación Estándar
desviacion.año <- sd(var_cat_año)
desviacion.año2 <- round(desviacion.año, 3)
desviacion.año2
desviacion.fuente <- sd(var_cat_fuente)
desviacion.fuente2 <- round(desviacion.fuente, 3)
desviacion.fuente2
desviacion.tipo <- sd(var_cat_tipo)
desviacion.tipo2 <- round(desviacion.tipo, 3)
desviacion.tipo2
desviacion.linea <- sd(var_cat_linea)
desviacion.linea2 <- round(desviacion.linea, 3)
desviacion.linea2
desviacion.gen <- sd(var_cat_gen)
desviacion.gen2 <- round(desviacion.gen, 3)
desviacion.gen2
desviacion.post <- sd(var_cat_post)
desviacion.post2 <- round(desviacion.post, 3)
desviacion.post2

var_cat_desviacion <- data.frame(Año = desviacion.año2, Fuente.Colocación = desviacion.fuente2, Tipo.Productor = desviacion.tipo2, Línea.de.Crédito = desviacion.linea2, Género = desviacion.gen2, Municipio.de.PostConflicto = desviacion.post2)
View(var_cat_desviacion)

#Coeficiente de Variación
sv.año <- sd(var_cat_año) / mean(var_cat_año)
sv.año2 <- round(sv.año, 3)
sv.año2
sv.fuente <- sd(var_cat_fuente) / mean(var_cat_fuente)
sv.fuente2 <- round(sv.fuente, 3)
sv.fuente2
sv.tipo <- sd(var_cat_tipo) / mean(var_cat_tipo)
sv.tipo2 <- round(sv.tipo, 3)
sv.tipo2
sv.linea <- sd(var_cat_linea) / mean(var_cat_linea)
sv.linea2 <- round(sv.linea, 3)
sv.linea2
sv.gen <- sd(var_cat_gen) / mean(var_cat_gen)
sv.gen2 <- round(sv.gen, 3)
sv.gen2
sv.post <- sd(var_cat_post) / mean(var_cat_post)
sv.post2 <- round(sv.post, 3)
sv.post2

var_cat_sv <- data.frame(Año = sv.año2, Fuente.Colocación = sv.fuente2, Tipo.Productor = sv.tipo2, Línea.de.Crédito = sv.linea2, Género = sv.gen2, Municipio.de.PostConflicto = sv.post2)
View(var_cat_sv)

#Rango Intercuartilico
ran_inter.año <- IQR(var_cat_año)
ran_inter.año2 <- round(ran_inter.año, 3)
ran_inter.año2
ran_inter.fuente <- IQR(var_cat_fuente)
ran_inter.fuente2 <- round(ran_inter.fuente, 3)
ran_inter.fuente2
ran_inter.tipo <- IQR(var_cat_tipo)
ran_inter.tipo2 <- round(ran_inter.tipo, 3)
ran_inter.tipo2
ran_inter.linea <- IQR(var_cat_linea)
ran_inter.linea2 <- round(ran_inter.linea, 3)
ran_inter.linea2
ran_inter.gen <- IQR(var_cat_gen)
ran_inter.gen2 <- round(ran_inter.gen, 3)
ran_inter.gen2
ran_inter.post <- IQR(var_cat_post)
ran_inter.post2 <- round(ran_inter.post, 3)
ran_inter.post2

var_cat_ran_inter <- data.frame(Año = ran_inter.año2, Fuente.Colocación = ran_inter.fuente2, Tipo.Productor = ran_inter.tipo2, Línea.de.Crédito = ran_inter.linea2, Género = ran_inter.gen2, Municipio.de.PostConflicto = ran_inter.post2)
View(var_cat_ran_inter)

#Estadígrafos de forma
#Asimetría
asimetria.año <- skewness(var_cat_año)
asimetria.año2 <- round(asimetria.año, 3)
asimetria.año2
asimetria.fuente <- skewness(var_cat_fuente)
asimetria.fuente2 <- round(asimetria.fuente, 3)
asimetria.fuente2
asimetria.tipo <- skewness(var_cat_tipo)
asimetria.tipo2 <- round(asimetria.tipo, 3)
asimetria.tipo2
asimetria.linea <- skewness(var_cat_linea)
asimetria.linea2 <- round(asimetria.linea, 3)
asimetria.linea2
asimetria.gen <- skewness(var_cat_gen)
asimetria.gen2 <- round(asimetria.gen, 3)
asimetria.gen2
asimetria.post <- skewness(var_cat_post)
asimetria.post2 <- round(asimetria.post)
asimetria.post2

var_cat_asimetria <- data.frame(Año = asimetria.año2, Fuente.Colocación = asimetria.fuente2, Tipo.Productor = asimetria.tipo2, Línea.de.Crédito = asimetria.linea2, Género = asimetria.gen2, Municipio.de.PostConflicto = asimetria.post2)
View(var_cat_asimetria)

#Curtosis
curtosis.año <- kurtosis(var_cat_año)
curtosis.año2 <- round(curtosis.año, 3)
curtosis.año2
curtosis.fuente <- kurtosis(var_cat_fuente)
curtosis.fuente2 <- round(curtosis.fuente)
curtosis.fuente2
curtosis.tipo <- kurtosis(var_cat_tipo)
curtosis.tipo2 <- round(curtosis.tipo, 3)
curtosis.tipo2
curtosis.linea <- kurtosis(var_cat_linea)
curtosis.linea2 <- round(curtosis.linea)
curtosis.linea2
curtosis.gen <- kurtosis(var_cat_gen)
curtosis.gen2 <- round(curtosis.gen, 3)
curtosis.gen2
curtosis.post <- kurtosis(var_cat_post)
curtosis.post2 <- round(curtosis.post, 3)
curtosis.post2

var_cat_curtosis <- data.frame(Año = curtosis.año2, Fuente.Colocación = curtosis.fuente2, Tipo.Productor = curtosis.tipo2, Línea.de.Crédito = curtosis.linea2, Género = curtosis.gen2, Municipio.de.PostConflicto = curtosis.post2)
View(var_cat_curtosis)

#Estadígrafos d posición
#Cuartiles
Q1.año <- quantile(var_cat_año, probs = c(0.25, 0.5, 0.75))
Q1.año
Q1.fuente <- quantile(var_cat_fuente, probs = c(0.25, 0.5, 0.75))
Q1.fuente
Q1.tipo <- quantile(var_cat_tipo, probs = c(0.25, 0.5, 0.75))
Q1.tipo
Q1.linea <- quantile(var_cat_linea, probs = c(0.25, 0.5, 0.75))
Q1.linea
Q1.gen <- quantile(var_cat_gen, probs = c(0.25, 0.5, 0.75))
Q1.gen
Q1.post <- quantile(var_cat_post, probs = c(0.25, 0.5, 0.75))
Q1.post

var_cat_Q1 <- data.frame(Año = Q1.año, Fuente.Colocación = Q1.fuente, Tipo.Productor = Q1.tipo, Línea.de.Crédito = Q1.linea, Género = Q1.gen, Municipio.de.PostConflicto = Q1.post)
View(var_cat_Q1)

#Percentiles
percentil.año <- quantile(var_cat_año, probs = c(0.1, 0.5, 0.90))
percentil.año
percentil.fuente <- quantile(var_cat_fuente, probs = c(0.1, 0.5, 0.90))
percentil.fuente
percentil.tipo <- quantile(var_cat_tipo, probs = c(0.1, 0.5, 0.90))
percentil.tipo
percentil.linea <- quantile(var_cat_linea, probs = c(0.1, 0.5, 0.90))
percentil.linea
percentil.gen <- quantile(var_cat_gen, probs = c(0.1, 0.5, 0.90))
percentil.gen
percentil.post <- quantile(var_cat_post, probs = c(0.1, 0.5, 0.90))
percentil.post

var_cat_percentil <- data.frame(Año = percentil.año, Fuente.Colocación = percentil.fuente, Tipo.Productor = percentil.tipo, Línea.de.Crédito = percentil.linea, Género = percentil.gen, Municipio.de.PostConflicto = percentil.post)
View(var_cat_percentil)

#Deciles
decil.año <- quantile(var_cat_año, probs = seq(0.1, 0.9, by = 0.1))
decil.año
decil.fuente <- quantile(var_cat_fuente, probs = seq(0.1, 0.9, by = 0.1))
decil.fuente
decil.tipo <- quantile(var_cat_tipo, probs = seq(0.1, 0.9, by = 0.1))
decil.tipo
decil.linea <- quantile(var_cat_linea, probs = seq(0.1, 0.9, by = 0.1))
decil.linea
decil.gen <- quantile(var_cat_gen, probs = seq(0.1, 0.9, by = 0.1))
decil.gen
decil.post <- quantile(var_cat_post, probs = seq(0.1, 0.9, by = 0.1))
decil.post

var_cat_decil <- data.frame(Año = decil.año, Fuente.Colocación = decil.fuente, Tipo.Productor = decil.tipo, Línea.de.Crédito = decil.linea, Género = decil.gen, Municipio.de.PostConflicto = decil.post)
View(var_cat_decil)

#Máximos
máximo.año <- max(var_cat_año)
máximo.año
máximo.fuente <- max(var_cat_fuente)
máximo.fuente
máximo.tipo <- max(var_cat_tipo)
máximo.tipo
máximo.linea <- max(var_cat_linea)
máximo.id_depto
máximo.gen <- max(var_cat_gen)
máximo.gen
máximo.post <- max(var_cat_post)
máximo.post

var_cat_máximo <- data.frame(Año = máximo.año, Fuente.Colocación = máximo.fuente, Tipo.Productor = máximo.tipo, Línea.de.Crédito = máximo.linea, Género = máximo.gen, Municipio.de.PostConflicto = máximo.post)
View(var_cat_máximo)

#Mínimo
mínimo.año <- min(var_cat_año)
mínimo.tipo_prod
mínimo.fuente <- min(var_cat_fuente)
mínimo.fuente
mínimo.tipo <- min(var_cat_tipo)
mínimo.tipo
mínimo.linea <- min(var_cat_linea)
mínimo.linea
mínimo.gen <- min(var_cat_gen)
mínimo.gen
mínimo.post <- min(var_cat_post)
mínimo.post

var_cat_mínimo <- data.frame(Año = mínimo.año, Fuente.Colocación = mínimo.fuente, Tipo.Productor = mínimo.tipo, Línea.de.Crédito = mínimo.linea, Género = mínimo.gen, Municipio.de.PostConflicto = mínimo.post)
View(var_cat_mínimo)

#Tabla resumen
as.data.frame(descr(var_cat_act))
view(descr(var_cat_act))

#Gráficos
#Gráfico tabla de frecuencia absoluta variables categóricas
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act <- table(var_cat_act)
df.tablas_frec_var_cat_act <- as.data.frame(tablas_frec_var_cat_act)
colnames(df.tablas_frec_var_cat_act) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Gráfico tabla de frecuencia año
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act.año2 <- table(var_cat_act$Año)
df.tablas_frec_var_cat_act.año <- as.data.frame(tablas_frec_var_cat_act.año2)
colnames(df.tablas_frec_var_cat_act.año) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act.año, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Gráfico tabla de frecuencia fuente colocación
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act.fuente2 <- table(var_cat_act$Fuente.Colocación)
df.tablas_frec_var_cat_act.fuente <- as.data.frame(tablas_frec_var_cat_act.fuente2)
colnames(df.tablas_frec_var_cat_act.fuente) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act.fuente, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Gráfico tabla de frecuencia tipo productor
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act.prod <- table(var_cat_act$Tipo.Productor)
df.tablas_frec_var_cat_act.prod <- as.data.frame(tablas_frec_var_cat_act.prod)
colnames(df.tablas_frec_var_cat_act.prod) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act.prod, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Gráfico tabla de frecuencia línea de crédito
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act.linea <- table(var_cat_act$Línea.de.Crédito)
df.tablas_frec_var_cat_act.linea <- as.data.frame(tablas_frec_var_cat_act.linea)
colnames(df.tablas_frec_var_cat_act.linea) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act.linea, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Gráfico tabla de frecuencia género
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act.gen <- table(var_cat_act$Género)
df.tablas_frec_var_cat_act.gen <- as.data.frame(tablas_frec_var_cat_act.gen)
colnames(df.tablas_frec_var_cat_act.gen) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act.gen, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Gráfico tabla de frecuencia municipio postconflicto
var_cat_act <- list(Año = var_cat_año, Fuente.Colocación = var_cat_fuente, Tipo.Productor = var_cat_tipo, Línea.de.Crédito = var_cat_linea, Género = var_cat_gen, Municipio.de.PostConflicto = var_cat_post)
tablas_frec_var_cat_act.post <- table(var_cat_act$Municipio.de.PostConflicto)
df.tablas_frec_var_cat_act.post <- as.data.frame(tablas_frec_var_cat_act.post)
colnames(df.tablas_frec_var_cat_act.post) <- c("Valor", "Frecuencia")
ggplot(data = df.tablas_frec_var_cat_act.post, aes(x = as.numeric(Valor), y = Frecuencia)) +
  geom_line(color = "blue") +         # Línea del polígono
  geom_point(color = "red", size = 2) + # Puntos sobre cada frecuencia
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Selección de las variables discretas
var_dis <- datos_flitrados %>% select("Id.Tipo.Prod", "Mes", "ID.Depto", "Id.Munic", "DEPCOL", "MUNCOL", "Plazo", "ID.Rubro", "X..FAG", "Cantidad")
var_dis

#Conversión de variables 
var_dis.tipo_prod <- as.numeric(var_dis$Id.Tipo.Prod)
var_dis.tipo_prod
var_dis.mes <- as.numeric(var_dis$Mes)
var_dis.mes
var_dis.id_depto <- as.numeric(var_dis$ID.Depto)
var_dis.id_depto
var_dis$Id.Munic <- as.numeric(gsub("[^0-9.]", "", var_dis$Id.Munic))
var_dis.id_munic <- as.numeric(var_dis$Id.Munic)
var_dis.id_munic
var_dis.depcol <- as.numeric(var_dis$DEPCOL)
var_dis.depcol
var_dis.muncol <- as.numeric(var_dis$MUNCOL)
var_dis.muncol
var_dis.plazo <- as.numeric(var_dis$Plazo)
var_dis.plazo
var_dis.id_rubro <- as.numeric(var_dis$ID.Rubro)
var_dis.id_rubro
var_dis.fag <- as.numeric(var_dis$X..FAG)
var_dis.fag
var_dis.cant <- as.numeric(var_dis$Cantidad)
var_dis.cant

#Lista de variables actualizadas
var_dis_act <- list(Id.Tipo.Prod = var_dis.tipo_prod, Mes = var_dis.mes, ID.Depto = var_dis.id_depto, Id.Munic = var_dis.id_depto, DEPCOL = var_dis.depcol, MUNCOL = var_dis.muncol, Plazo = var_dis.plazo, ID.Rubro = var_dis.id_rubro, X..FAG = var_dis.fag, Cantidad = var_dis.cant)
var_dis_act

#Estadigrafos de tendencia central
#Media
media.tipo_prod <- mean(var_dis.tipo_prod)
media.tipo_prod2 <- round(media.tipo_prod, 3)
media.tipo_prod2
media.mes <- mean(var_dis.mes)
media.mes2 <- round(media.mes, 3)
media.mes2
media.id_depto <- mean(var_dis.id_depto)
media.id_depto2 <- round(media.id_depto, 3)
media.id_depto2
media.id_munic <- mean(var_dis.id_munic)
media.id_munic2 <- round(media.id_munic, 3)
media.id_munic2
media.muncol <- mean(var_dis.muncol)
media.muncol2 <- round(media.muncol, 3)
media.muncol2
media.depcol <- mean(var_dis.depcol)
media.depcol2 <- round(media.depcol, 3)
media.depcol2
media.plazo <- mean(var_dis.plazo)
media.plazo2 <- round(media.plazo, 3)
media.plazo2
media.id_rubro <- mean(var_dis.id_rubro)
media.id_rubro2 <- round(media.id_rubro, 3)
media.id_rubro2
media.fag <- mean(var_dis.fag)
media.fag2 <- round(media.fag, 3)
media.fag2
media.cant <- mean(var_dis.cant)
media.cant2 <- round(media.cant, 3)
media.cant2

var_dis_media <- data.frame(Tipo.Prod = media.tipo_prod2, Mes = media.mes2, ID.Depto = media.id_depto2, Id.Munic = media.id_munic2, MUNCOL = media.muncol2, DEPCOL = media.depcol2, Plazo = media.plazo2, ID.Rubro = media.id_rubro2, X..FAG = media.fag2, Cantidad = media.cant2)
View(var_dis_media)

#Mediana
mediana.tipo_prod <- median(var_dis.tipo_prod)
mediana.tipo_prod
mediana.mes <- median(var_dis.mes)
mediana.mes
mediana.id_depto <- median(var_dis.id_depto)
mediana.id_depto
mediana.id_munic <- median(var_dis.id_munic)
mediana.id_depto
mediana.muncol <- median(var_dis.muncol)
mediana.muncol
mediana.depcol <- median(var_dis.depcol)
mediana.depcol
mediana.plazo <- median(var_dis.plazo)
mediana.plazo
mediana.id_rubro <- median(var_dis.id_rubro)
mediana.id_rubro
mediana.fag <- median(var_dis.fag)
mediana.fag
mediana.cant <- median(var_dis.cant)
mediana.cant

var_dis_mediana <- data.frame(Tipo.Prod = mediana.tipo_prod, Mes = mediana.mes, ID.Depto = mediana.id_depto, Id.Munic = mediana.id_munic, MUNCOL = mediana.muncol, DEPCOL = mediana.depcol, Plazo = mediana.plazo, ID.Rubro = mediana.id_rubro, X..FAG = mediana.fag, Cantidad = mediana.cant)
View(var_dis_mediana)

#Moda
moda.tipo_prod <- Mode(var_dis.tipo_prod)
moda.tipo_prod
moda.mes <- Mode(var_dis.mes)
moda.mes
moda.id_depto <- Mode(var_dis.id_depto)
moda.id_depto
moda.id_munic <- Mode(var_dis.id_munic)
moda.id_depto
moda.muncol <- Mode(var_dis.muncol)
moda.muncol
moda.depcol <- Mode(var_dis.depcol)
moda.depcol
moda.plazo <- Mode(var_dis.plazo)
moda.plazo
moda.id_rubro <- Mode(var_dis.id_rubro)
moda.id_rubro
moda.fag <- Mode(var_dis.fag)
moda.fag
moda.cant <- Mode(var_dis.cant)
moda.cant

var_dis_moda <- data.frame(Tipo.Prod = moda.tipo_prod, Mes = moda.mes, ID.Depto = moda.id_depto, Id.Munic = moda.id_munic, MUNCOL = moda.muncol, DEPCOL = moda.depcol, Plazo = moda.plazo, ID.Rubro = moda.id_rubro, X..FAG = moda.fag, Cantidad = moda.cant)
View(var_dis_moda)

#Estadígrafos de dispersión
#Rango
rango.tipo_prod <- range(var_dis.tipo_prod)
rango.tipo_prod
rango.mes <- range(var_dis.mes)
rango.mes
rango.id_depto <- range(var_dis.id_depto)
rango.id_depto
rango.id_munic <- range(var_dis.id_munic)
rango.id_depto
rango.muncol <- range(var_dis.muncol)
rango.muncol
rango.depcol <- range(var_dis.depcol)
rango.depcol
rango.plazo <- range(var_dis.plazo)
rango.plazo
rango.id_rubro <- range(var_dis.id_rubro)
rango.id_rubro
rango.fag <- range(var_dis.fag)
rango.fag
rango.cant <- range(var_dis.cant)
rango.cant

var_dis_rango <- data.frame(Tipo.Prod = rango.tipo_prod, Mes = rango.mes, ID.Depto = rango.id_depto, Id.Munic = rango.id_munic, MUNCOL = rango.muncol, DEPCOL = rango.depcol, Plazo = rango.plazo, ID.Rubro = rango.id_rubro, X..FAG = rango.fag, Cantidad = rango.cant)
View(var_dis_rango)

#Varianza
varianza.tipo_prod <- var(var_dis.tipo_prod)
varianza.tipo_prod2 <- round(varianza.tipo_prod, 3)
varianza.tipo_prod2
varianza.mes <- var(var_dis.mes)
varianza.mes2 <- round(varianza.mes, 3)
varianza.mes2
varianza.id_depto <- var(var_dis.id_depto)
varianza.id_depto2 <- round(varianza.id_depto, 3)
varianza.id_depto2
varianza.id_munic <- var(var_dis.id_munic)
varianza.id_munic2 <- round(varianza.id_munic, 3)
varianza.id_munic2
varianza.muncol <- var(var_dis.muncol)
varianza.muncol2 <- round(varianza.muncol, 3)
varianza.muncol2
varianza.depcol <- var(var_dis.depcol)
varianza.depcol2 <- round(varianza.depcol, 3)
varianza.depcol2
varianza.plazo <- var(var_dis.plazo)
varianza.plazo2 <- round(varianza.plazo, 3)
varianza.plazo2
varianza.id_rubro <- var(var_dis.id_rubro)
varianza.id_rubro2 <- round(varianza.id_rubro, 3)
varianza.id_rubro2
varianza.fag <- var(var_dis.fag)
varianza.fag2 <- round(varianza.fag, 3)
varianza.fag2
varianza.cant <- var(var_dis.cant)
varianza.cant2 <- round(varianza.cant, 3)
varianza.cant2

var_dis_varianza <- data.frame(Tipo.Prod = varianza.tipo_prod2, Mes = varianza.mes2, ID.Depto = varianza.id_depto2, Id.Munic = varianza.id_munic2, MUNCOL = varianza.muncol2, DEPCOL = varianza.depcol2, Plazo = varianza.plazo2, ID.Rubro = varianza.id_rubro2, X..FAG = varianza.fag2, Cantidad = varianza.cant2)
View(var_dis_varianza)

#Desviación Estándar
desviacion.tipo_prod <- sd(var_dis.tipo_prod)
desviacion.tipo_prod2 <- round(desviacion.tipo, 3)
desviacion.tipo_prod2
desviacion.mes <- sd(var_dis.mes)
desviacion.mes2 <- round(desviacion.mes, 3)
desviacion.mes2
desviacion.id_depto <- sd(var_dis.id_depto)
desviacion.id_depto2 <- round(desviacion.id_depto, 3)
desviacion.id_depto2
desviacion.id_munic <- sd(var_dis.id_munic)
desviacion.id_munic2 <- round(desviacion.id_munic, 3)
desviacion.id_munic2
desviacion.muncol <- sd(var_dis.muncol)
desviacion.muncol2 <- round(desviacion.muncol, 3)
desviacion.muncol2
desviacion.depcol <- sd(var_dis.depcol)
desviacion.depcol2 <- round(desviacion.depcol, 3)
desviacion.depcol2
desviacion.plazo <- sd(var_dis.plazo)
desviacion.plazo2 <- round(desviacion.plazo, 3)
desviacion.plazo2
desviacion.id_rubro <- sd(var_dis.id_rubro)
desviacion.id_rubro2 <- round(desviacion.id_rubro, 3)
desviacion.id_rubro2
desviacion.fag <- sd(var_dis.fag)
desviacion.fag2 <- round(desviacion.fag, 3)
desviacion.fag2
desviacion.cant <- sd(var_dis.cant)
desviacion.cant2 <- round(desviacion.cant, 3)
desviacion.cant2

var_dis_desviacion <- data.frame(Tipo.Prod = desviacion.tipo_prod2, Mes = desviacion.mes2, ID.Depto = desviacion.id_depto2, Id.Munic = desviacion.id_munic2, MUNCOL = desviacion.muncol2, DEPCOL = desviacion.depcol2, Plazo = desviacion.plazo2, ID.Rubro = desviacion.id_rubro2, X..FAG = desviacion.fag2, Cantidad = desviacion.cant2)
View(var_dis_desviacion)

#Coeficiente de Variación
sv.tipo_prod <- sd(var_dis.tipo_prod) / mean(var_dis.tipo_prod)
sv.tipo_prod2 <- round(sv.tipo_prod, 3)
sv.tipo_prod2
sv.mes <- sd(var_dis.mes) / mean(var_dis.mes)
sv.mes2 <- round(sv.mes, 3)
sv.mes2
sv.id_depto <- sd(var_dis.id_depto) / mean(var_dis.id_depto)
sv.id_depto2 <- round(sv.id_depto, 3)
sv.id_depto2
sv.id_munic <- sd(var_dis.id_munic) / mean(var_dis.id_munic)
sv.id_munic2 <- round(sv.id_munic, 3)
sv.id_munic2
sv.muncol <- sd(var_dis.muncol) / mean(var_dis.muncol)
sv.muncol2 <- round(sv.muncol, 3)
sv.muncol2
sv.depcol <- sd(var_dis.depcol) / mean(var_dis.depcol)
sv.depcol2 <- round(sv.depcol, 3)
sv.depcol2
sv.plazo <- sd(var_dis.plazo) / mean(var_dis.plazo)
sv.plazo2 <- round(sv.plazo, 3)
sv.plazo2
sv.id_rubro <- sd(var_dis.id_rubro) / mean(var_dis.id_rubro)
sv.id_rubro2 <- round(sv.id_rubro, 3)
sv.id_rubro2
sv.fag <- sd(var_dis.fag) / mean(var_dis.fag)
sv.fag2 <- round(sv.fag, 3)
sv.fag2
sv.cant <- sd(var_dis.cant) / mean(var_dis.cant)
sv.cant2 <- round(sv.cant, 3)
sv.cant2

var_dis_sv <- data.frame(Tipo.Prod = sv.tipo_prod2, Mes = sv.mes2, ID.Depto = sv.id_depto2, Id.Munic = sv.id_munic2, MUNCOL = sv.muncol2, DEPCOL = sv.depcol2, Plazo = sv.plazo2, ID.Rubro = sv.id_rubro2, X..FAG = sv.fag2, Cantidad = sv.cant2)
View(var_dis_sv)

#Rango Intercuartilico
ran_inter.tipo_prod <- IQR(var_dis.tipo_prod)
ran_inter.tipo_prod
ran_inter.mes <- IQR(var_dis.mes)
ran_inter.mes
ran_inter.id_depto <- IQR(var_dis.id_depto)
ran_inter.id_depto
ran_inter.id_munic <- IQR(var_dis.id_munic)
ran_inter.id_depto
ran_inter.muncol <- IQR(var_dis.muncol)
ran_inter.muncol
ran_inter.depcol <- IQR(var_dis.depcol)
ran_inter.depcol
ran_inter.plazo <- IQR(var_dis.plazo)
ran_inter.plazo
ran_inter.id_rubro <- IQR(var_dis.id_rubro)
ran_inter.id_rubro
ran_inter.fag <- IQR(var_dis.fag)
ran_inter.fag
ran_inter.cant <- IQR(var_dis.cant)
ran_inter.cant

var_dis_ran_inter <- data.frame(Tipo.Prod = ran_inter.tipo_prod, Mes = ran_inter.mes, ID.Depto = ran_inter.id_depto, Id.Munic = ran_inter.id_munic, MUNCOL = ran_inter.muncol, DEPCOL = ran_inter.depcol, Plazo = ran_inter.plazo, ID.Rubro = ran_inter.id_rubro, X..FAG = ran_inter.fag, Cantidad = ran_inter.cant)
View(var_dis_ran_inter)

#Estadígrafos de forma
#Asimetría
asimetria.tipo_prod <- skewness(var_dis.tipo_prod)
asimetria.tipo_prod2 <- round(asimetria.tipo_prod, 3)
asimetria.tipo_prod2
asimetria.mes <- skewness(var_dis.mes)
asimetria.mes2 <- round(asimetria.mes, 3)
asimetria.mes2
asimetria.id_depto <- skewness(var_dis.id_depto)
asimetria.id_depto2 <- round(asimetria.id_depto, 3)
asimetria.id_depto2
asimetria.id_munic <- skewness(var_dis.id_munic)
asimetria.id_munic2 <- round(asimetria.id_munic, 3)
asimetria.id_munic2
asimetria.muncol <- skewness(var_dis.muncol)
asimetria.muncol2 <- round(asimetria.muncol, 3)
asimetria.muncol2
asimetria.depcol <- skewness(var_dis.depcol)
asimetria.depcol2 <- round(asimetria.depcol, 3)
asimetria.depcol2
asimetria.plazo <- skewness(var_dis.plazo)
asimetria.plazo2 <- round(asimetria.plazo, 3)
asimetria.plazo2
asimetria.id_rubro <- skewness(var_dis.id_rubro)
asimetria.id_rubro2 <- round(asimetria.id_rubro, 3)
asimetria.id_rubro2
asimetria.fag <- skewness(var_dis.fag)
asimetria.fag2 <- round(asimetria.fag, 3)
asimetria.fag2
asimetria.cant <- skewness(var_dis.cant)
asimetria.cant2 <- round(asimetria.cant, 3)
asimetria.cant2

var_dis_asimetria <- data.frame(Tipo.Prod = asimetria.tipo_prod2, Mes = asimetria.mes2, ID.Depto = asimetria.id_depto2, Id.Munic = asimetria.id_munic2, MUNCOL = asimetria.muncol2, DEPCOL = asimetria.depcol2, Plazo = asimetria.plazo2, ID.Rubro = asimetria.id_rubro2, X..FAG = asimetria.fag2, Cantidad = asimetria.cant2)
View(var_dis_asimetria)

#Curtosis
curtosis.tipo_prod <- kurtosis(var_dis.tipo_prod)
curtosis.tipo_prod2 <- round(curtosis.tipo_prod, 3)
curtosis.tipo_prod2
curtosis.mes <- kurtosis(var_dis.mes)
curtosis.mes2 <- round(curtosis.mes, 3)
curtosis.mes2
curtosis.id_depto <- kurtosis(var_dis.id_depto)
curtosis.id_depto2 <- round(curtosis.id_depto, 3)
curtosis.id_depto2
curtosis.id_munic <- kurtosis(var_dis.id_munic)
curtosis.id_munic2 <- round(curtosis.id_munic, 3)
curtosis.id_munic2
curtosis.muncol <- kurtosis(var_dis.muncol)
curtosis.muncol2 <- round(curtosis.muncol, 3)
curtosis.muncol2
curtosis.depcol <- kurtosis(var_dis.depcol)
curtosis.depcol2 <- round(curtosis.depcol, 3)
curtosis.depcol2
curtosis.plazo <- kurtosis(var_dis.plazo)
curtosis.plazo2 <- round(curtosis.plazo, 3)
curtosis.plazo2
curtosis.id_rubro <- kurtosis(var_dis.id_rubro)
curtosis.id_rubro2 <- round(curtosis.id_rubro, 3)
curtosis.id_rubro2
curtosis.fag <- kurtosis(var_dis.fag)
curtosis.fag2 <- round(curtosis.fag, 3)
curtosis.fag2
curtosis.cant <- kurtosis(var_dis.cant)
curtosis.cant2 <- round(curtosis.cant, 3)
curtosis.cant2

var_dis_curtosis <- data.frame(Tipo.Prod = curtosis.tipo_prod2, Mes = curtosis.mes2, ID.Depto = curtosis.id_depto2, Id.Munic = curtosis.id_munic2, MUNCOL = curtosis.muncol2, DEPCOL = curtosis.depcol2, Plazo = curtosis.plazo2, ID.Rubro = curtosis.id_rubro2, X..FAG = curtosis.fag2, Cantidad = curtosis.cant2)
View(var_dis_curtosis)

#Estadígrafos d posición
#Cuartiles
Q1.tipo_prod <- quantile(var_dis.tipo_prod, probs = c(0.25, 0.5, 0.75))
Q1.tipo_prod
Q1.mes <- quantile(var_dis.mes, probs = c(0.25, 0.5, 0.75))
Q1.mes
Q1.id_depto <- quantile(var_dis.id_depto, probs = c(0.25, 0.5, 0.75))
Q1.id_depto
Q1.id_munic <- quantile(var_dis.id_munic, probs = c(0.25, 0.5, 0.75))
Q1.id_depto
Q1.muncol <- quantile(var_dis.muncol, probs = c(0.25, 0.5, 0.75))
Q1.muncol
Q1.depcol <- quantile(var_dis.depcol, probs = c(0.25, 0.5, 0.75))
Q1.depcol
Q1.plazo <- quantile(var_dis.plazo, probs = c(0.25, 0.5, 0.75))
Q1.plazo
Q1.id_rubro <- quantile(var_dis.id_rubro, probs = c(0.25, 0.5, 0.75))
Q1.id_rubro
Q1.fag <- quantile(var_dis.fag, probs = c(0.25, 0.5, 0.75))
Q1.fag
Q1.cant <- quantile(var_dis.cant, probs = c(0.25, 0.5, 0.75))
Q1.cant

var_dis_Q1 <- data.frame(Tipo.Prod = Q1.tipo_prod, Mes = Q1.mes, ID.Depto = Q1.id_depto, Id.Munic = Q1.id_munic, MUNCOL = Q1.muncol, DEPCOL = Q1.depcol, Plazo = Q1.plazo, ID.Rubro = Q1.id_rubro, X..FAG = Q1.fag, Cantidad = Q1.cant)
View(var_dis_Q1)

#Percentiles
percentil.tipo_prod <- quantile(var_dis.tipo_prod, probs = c(0.1, 0.5, 0.90))
percentil.tipo_prod
percentil.mes <- quantile(var_dis.mes, probs = c(0.1, 0.5, 0.90))
percentil.mes
percentil.id_depto <- quantile(var_dis.id_depto, probs = c(0.1, 0.5, 0.90))
percentil.id_depto
percentil.id_munic <- quantile(var_dis.id_munic, probs = c(0.1, 0.5, 0.90))
percentil.id_depto
percentil.muncol <- quantile(var_dis.muncol, probs = c(0.1, 0.5, 0.90))
percentil.muncol
percentil.depcol <- quantile(var_dis.depcol, probs = c(0.1, 0.5, 0.90))
percentil.depcol
percentil.plazo <- quantile(var_dis.plazo, probs = c(0.1, 0.5, 0.90))
percentil.plazo
percentil.id_rubro <- quantile(var_dis.id_rubro, probs = c(0.1, 0.5, 0.90))
percentil.id_rubro
percentil.fag <- quantile(var_dis.fag, probs = c(0.1, 0.5, 0.90))
percentil.fag
percentil.cant <- quantile(var_dis.cant, probs = c(0.1, 0.5, 0.90))
percentil.cant

var_dis_percentil <- data.frame(Tipo.Prod = percentil.tipo_prod, Mes = percentil.mes, ID.Depto = percentil.id_depto, Id.Munic = percentil.id_munic, MUNCOL = percentil.muncol, DEPCOL = percentil.depcol, Plazo = percentil.plazo, ID.Rubro = percentil.id_rubro, X..FAG = percentil.fag, Cantidad = percentil.cant)
View(var_dis_percentil)

#Deciles
decil.tipo_prod <- quantile(var_dis.tipo_prod, probs = seq(0.1, 0.9, by = 0.1))
decil.tipo_prod
decil.mes <- quantile(var_dis.mes, probs = seq(0.1, 0.9, by = 0.1))
decil.mes
decil.id_depto <- quantile(var_dis.id_depto, probs = seq(0.1, 0.9, by = 0.1))
decil.id_depto
decil.id_munic <- quantile(var_dis.id_munic, probs = seq(0.1, 0.9, by = 0.1))
decil.id_depto
decil.muncol <- quantile(var_dis.muncol, probs = seq(0.1, 0.9, by = 0.1))
decil.muncol
decil.depcol <- quantile(var_dis.depcol, probs = seq(0.1, 0.9, by = 0.1))
decil.depcol
decil.plazo <- quantile(var_dis.plazo, probs = seq(0.1, 0.9, by = 0.1))
decil.plazo
decil.id_rubro <- quantile(var_dis.id_rubro, probs = seq(0.1, 0.9, by = 0.1))
decil.id_rubro
decil.fag <- quantile(var_dis.fag, probs = seq(0.1, 0.9, by = 0.1))
decil.fag
decil.cant <- quantile(var_dis.cant, probs = seq(0.1, 0.9, by = 0.1))
decil.cant

var_dis_decil <- data.frame(Tipo.Prod = decil.tipo_prod, Mes = decil.mes, ID.Depto = decil.id_depto, Id.Munic = decil.id_munic, MUNCOL = decil.muncol, DEPCOL = decil.depcol, Plazo = decil.plazo, ID.Rubro = decil.id_rubro, X..FAG = decil.fag, Cantidad = decil.cant)
View(var_dis_decil)

#Máximos
máximo.tipo_prod <- max(var_dis.tipo_prod)
máximo.tipo_prod
máximo.mes <- max(var_dis.mes)
máximo.mes
máximo.id_depto <- max(var_dis.id_depto)
máximo.id_depto
máximo.id_munic <- max(var_dis.id_munic)
máximo.id_depto
máximo.muncol <- max(var_dis.muncol)
máximo.muncol
máximo.depcol <- max(var_dis.depcol)
máximo.depcol
máximo.plazo <- max(var_dis.plazo)
máximo.plazo
máximo.id_rubro <- max(var_dis.id_rubro)
máximo.id_rubro
máximo.fag <- max(var_dis.fag)
máximo.fag
máximo.cant <- max(var_dis.cant)
máximo.cant

var_dis_máximo <- data.frame(Tipo.Prod = máximo.tipo_prod, Mes = máximo.mes, ID.Depto = máximo.id_depto, Id.Munic = máximo.id_munic, MUNCOL = máximo.muncol, DEPCOL = máximo.depcol, Plazo = máximo.plazo, ID.Rubro = máximo.id_rubro, X..FAG = máximo.fag, Cantidad = máximo.cant)
View(var_dis_máximo)

#Mínimo
mínimo.tipo_prod <- min(var_dis.tipo_prod)
mínimo.tipo_prod
mínimo.mes <- min(var_dis.mes)
mínimo.mes
mínimo.id_depto <- min(var_dis.id_depto)
mínimo.id_depto
mínimo.id_munic <- min(var_dis.id_munic)
mínimo.id_depto
mínimo.muncol <- min(var_dis.muncol)
mínimo.muncol
mínimo.depcol <- min(var_dis.depcol)
mínimo.depcol
mínimo.plazo <- min(var_dis.plazo)
mínimo.plazo
mínimo.id_rubro <- min(var_dis.id_rubro)
mínimo.id_rubro
mínimo.fag <- min(var_dis.fag)
mínimo.fag
mínimo.cant <- min(var_dis.cant)
mínimo.cant

var_dis_mínimo <- data.frame(Tipo.Prod = mínimo.tipo_prod, Mes = mínimo.mes, ID.Depto = mínimo.id_depto, Id.Munic = mínimo.id_munic, MUNCOL = mínimo.muncol, DEPCOL = mínimo.depcol, Plazo = mínimo.plazo, ID.Rubro = mínimo.id_rubro, X..FAG = mínimo.fag, Cantidad = mínimo.cant)
View(var_dis_mínimo)

#Tabla resumen
as.data.frame(descr(var_dis))
view(descr(var_dis))

#Gráficos
#Estadígrafos de Tendencia Central
vector_media <- c(media.tipo_prod, media.mes, media.id_depto, media.muncol, media.depcol, media.plazo, media.id_rubro, media.fag, media.cant)
media <- mean(vector_media)
vector_mediana <- c(mediana.tipo_prod, mediana.mes, mediana.id_depto, mediana.muncol, mediana.depcol, mediana.plazo, mediana.id_rubro, mediana.fag, mediana.cant)
mediana <- median(vector_mediana)
vector_moda <- c(moda.tipo_prod, moda.mes, moda.id_depto, moda.muncol, moda.depcol, moda.plazo, moda.id_rubro, moda.fag, moda.cant)
moda <- as.numeric(names(sort(table(vector_moda), decreasing = TRUE)[1]))
mi_vector <- c(vector_media, vector_mediana, vector_moda)
datos <- data.frame(valores = mi_vector)
ggplot(datos, aes(x = valores)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = media), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = mediana), color = "green", linetype = "dotted", size = 1.2) +
  geom_vline(aes(xintercept = moda), color = "blue", linetype = "solid", size = 1.2) +
  geom_text(aes(x = media, y = 5, label = "Media"), color = "red", vjust = -0.5, hjust = -0.1) +
  geom_text(aes(x = mediana, y = 5, label = "Mediana"), color = "green", vjust = -0.5, hjust = -0.1) +
  geom_text(aes(x = moda, y = 5, label = "Moda"), color = "blue", vjust = -0.5, hjust = -0.1) +
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Estadígrafos de Dispersión
vector_rango <- c(rango.tipo_prod, rango.mes, rango.id_depto, rango.id_munic, rango.muncol, rango.depcol, rango.plazo, rango.id_rubro, rango.fag, rango.cant)
rango <- max(vector_rango) - min(vector_rango)
vector_varianza <- c(varianza.tipo_prod2, varianza.mes2, varianza.id_depto2, varianza.id_munic2, varianza.muncol2, varianza.depcol2, varianza.plazo2, varianza.id_rubro2, varianza.fag2, varianza.cant2)
varianza <- var(vector_varianza)
vector_sd <- c(desviacion.tipo_prod2, desviacion.mes2, desviacion.id_depto2, desviacion.id_munic2, desviacion.muncol2, desviacion.depcol2, desviacion.plazo2, desviacion.id_rubro2, desviacion.fag2, desviacion.cant2)
desv_estandar <- sd(vector_sd)
coef_var <- (desv_estandar / mean(vector_sd)) * 100
vector_rango_inter <- c(ran_inter.tipo_prod, ran_inter.mes, ran_inter.id_depto, ran_inter.id_munic, ran_inter.muncol, ran_inter.depcol, ran_inter.plazo, ran_inter.id_rubro, ran_inter.fag, ran_inter.cant)
rango_intercuartilico <- IQR(vector_rango_inter)
vector_dispersion <- c(rango, varianza, desv_estandar, coef_var, rango_intercuartilico)
estadigrafos <- c("Rango", "Varianza", "Desv. Estándar", "Coef. Var.", "Rango Intercuartílico")
datos_dispersion <- data.frame(valores = vector_dispersion, estadigrafo = estadigrafos)
ggplot(datos_dispersion, aes(x = estadigrafo, y = valores)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = round(valores, 2)), vjust = -0.5, color = "black") +
  labs(x = "Estadígrafos",
       y = "Valores") +
  theme_minimal()

#Estadígrafos de Forma
vector_asimetria <- na.omit(c(asimetria.tipo_prod2, asimetria.mes2, asimetria.id_depto2, asimetria.id_munic2, asimetria.muncol2, asimetria.depcol2, asimetria.plazo2, asimetria.id_rubro2, asimetria.fag2, asimetria.cant2))
vector_curtosis <- na.omit(c(curtosis.tipo_prod2, curtosis.mes2, curtosis.id_depto2, curtosis.id_munic2, curtosis.muncol2, curtosis.depcol2, curtosis.plazo2, curtosis.id_rubro2, curtosis.fag2, curtosis.cant2))
datos <- data.frame(
  valores = c(vector_asimetria, vector_curtosis),
  estadigrafo = c(rep("Asimetría", length(vector_asimetria)),
                  rep("Curtosis", length(vector_curtosis)))
)
ggplot(datos, aes(x = valores, fill = estadigrafo)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~estadigrafo, scales = "free") +
  labs(x = "Valores",
       y = "Frecuencia") +
  scale_fill_manual(values = c("Asimetría" = "lightblue", "Curtosis" = "lightgreen")) +
  theme_minimal()

#Estadígrafos de Posición
cuartiles <- c(Q1.tipo_prod, Q1.mes, Q1.id_depto, Q1.id_munic, Q1.muncol, Q1.depcol, Q1.plazo, Q1.id_rubro, Q1.fag, Q1.cant)
percentil <- seq(10, 90, by = 10)
percentil2 <- c(percentil.tipo_prod, percentil.mes, percentil.id_depto, percentil.id_munic, percentil.muncol, percentil.depcol, percentil.plazo, percentil.id_rubro, percentil.fag, percentil.cant)
percentiles <- c(percentil, percentil2)
decil <- seq(10, 90, by = 10) 
decil2 <- c(decil.tipo_prod, decil.mes, decil.id_depto, decil.id_munic, decil.muncol, decil.depcol, decil.plazo, decil.id_rubro, decil.fag, decil.cant)
deciles <- c(decil, decil2)
maximo <- c(máximo.tipo_prod, máximo.mes, máximo.id_depto, máximo.id_munic, máximo.muncol, máximo.depcol, máximo.plazo, máximo.id_rubro, máximo.fag, máximo.cant) 
minimo <- c(mínimo.tipo_prod, mínimo.mes, mínimo.id_depto, mínimo.id_munic, mínimo.muncol, mínimo.depcol, mínimo.plazo, mínimo.id_rubro, mínimo.fag, mínimo.cant)    
vector_posicion <- c(cuartiles, percentiles, deciles, maximo, minimo)
estadigrafos <- c(rep("Cuartiles", length(cuartiles)),
                  rep("Percentiles", length(percentiles)),
                  rep("Deciles", length(deciles)),
                  rep("Máximo", length(maximo)),
                  rep("Mínimo", length(minimo)))
datos_posicion <- data.frame(
  valores = vector_posicion,
  estadigrafo = estadigrafos)
ggplot(datos_posicion, aes(x = valores, fill = estadigrafo)) +
  geom_histogram(binwidth = 3, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~estadigrafo, scales = "free") +
  labs(x = "Valores",
       y = "Frecuencia") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

#Selección de las variables continuas
var_con <- datos_flitrados %>% select("Valor.Inversión", "Colocación", "Vlr.Inic.Garantía")
var_con

#Conversión de variables 
var_con$Valor.Inversión <- gsub(",", "", var_con$Valor.Inversión)
var_con.valor_inver <- as.numeric(var_con$Valor.Inversión)
var_con.valor_inver
var_con$Colocación <- gsub(",", "", var_con$Colocación)
var_con.col <- as.numeric(var_con$Colocación)
var_con.col
var_con$Vlr.Inic.Garantía <- gsub(",", "", var_con$Vlr.Inic.Garantía)
var_con.valor_garan <- as.numeric(var_con$Vlr.Inic.Garantía)
var_con.valor_garan

#Lista de variables actualizadas
var_con_act <- list(Valor.Inversión = var_con.valor_inver, Colocación = var_con.col, Vlr.Inic.Garantía = var_con.valor_garan)
var_con_act

#Estadigrafos de tendencia central
#Media
media.valor_inver <- mean(var_con.valor_inver)
media.valor_inver
media.col <- mean(var_con.col)
media.col
media.valor_garan <- mean(var_con.valor_garan)
media.valor_garan

var_con_media <- data.frame(Valor.Inversión = media.valor_inver, Colocación = media.col, Vlr.Inic.Garantía = media.valor_garan)
View(var_con_media)

#Mediana
mediana.valor_inver <- median(var_con.valor_inver)
mediana.valor_inver
mediana.col <- median(var_con.col)
mediana.col
mediana.valor_garan <- median(var_con.valor_garan)
mediana.valor_garan

var_con_mediana <- data.frame(Valor.Inversión = mediana.valor_inver, Colocación = mediana.col, Vlr.Inic.Garantía = mediana.valor_garan)
View(var_con_mediana)

#Moda
moda.valor_inver <- Mode(var_con.valor_inver)
moda.valor_inver
moda.col <- Mode(var_con.col)
moda.col
moda.valor_garan <- Mode(var_con.valor_garan)
moda.valor_garan

var_con_moda <- data.frame(Valor.Inversión = moda.valor_inver, Colocación = moda.col, Vlr.Inic.Garantía = moda.valor_garan)
View(var_con_moda)

#Estadígrafos de dispersión
#Rango
rango.valor_inver <- range(var_con.valor_inver)
rango.valor_inver
rango.col <- range(var_con.col)
rango.col
rango.valor_garan <- range(var_con.valor_garan)
rango.valor_garan

var_con_rango <- data.frame(Valor.Inversión = rango.valor_inver, Colocación = rango.col, Vlr.Inic.Garantía = rango.valor_garan)
View(var_con_rango)

#Varianza
varianza.valor_inver <- var(var_con.valor_inver)
varianza.valor_inver
varianza.col <- var(var_con.col)
varianza.col
varianza.valor_garan <- var(var_con.valor_garan)
varianza.valor_garan

var_con_varianaza <- data.frame(Valor.Inversión = varianaza.valor_inver, Colocación = varianaza.col, Vlr.Inic.Garantía = varianaza.valor_garan)
View(var_con_varianaza)

#Desviación Estándar
desviación.valor_inver <- sd(var_con.valor_inver)
desviación.valor_inver
desviación.col <- sd(var_con.col)
desviación.col
desviación.valor_garan <- sd(var_con.valor_garan)
desviación.valor_garan

var_con_desviación <- data.frame(Valor.Inversión = desviación.valor_inver, Colocación = desviación.col, Vlr.Inic.Garantía = desviación.valor_garan)
View(var_con_desviación)

#Coeficiente de Variación
sv.valor_inver <- sd(var_con.valor_inver) / mean(var_con.valor_inver)
sv.valor_inver
sv.col <- sd(var_con.col) / mean(var_con.col)
sv.col
sv.valor_gran <- sd(var_con.valor_garan) / mean(var_con.valor_garan)
sv.valor_gran

var_con_sv <- data.frame(Valor.Inversión = sv.valor_inver, Colocación = sv.col, Vlr.Inic.Garantía = sv.valor_gran)
View(var_con_sv)

#Rango Intercuartílico
ran_inter.valor_inver <- IQR(var_con.valor_inver)
ran_inter.valor_inver
ran_inter.col <- IQR(var_con.col)
ran_inter.col
ran_inter.valor_garan <- IQR(var_con.valor_garan)
ran_inter.valor_garan

var_con_ran_inter <- data.frame(Valor.Inversión = ran_inter.valor_inver, Colocación = ran_inter.col, Vlr.Inic.Garantía = ran_inter.valor_garan)
View(var_con_ran_inter)

#Estadígrafos de forma
#Asimetría
asimetria.valor_inver <- skewness(var_con.valor_inver)
asimetria.valor_inver
asimetria.col <- skewness(var_con.col)
asimetria.col
asimetria.valor_garan <- skewness(var_con.valor_garan)
asimetria.valor_garan

var_con_asimetria <- data.frame(Valor.Inversión = asimetria.valor_inver, Colocación = asimetria.col, Vlr.Inic.Garantía = asimetria.valor_garan)
View(var_con_asimetria)

#Curtosis
curtosis.valor_inver <- kurtosis(var_con.valor_inver)
curtosis.valor_inver
curtosis.col <- kurtosis(var_con.col)
curtosis.col
curtosis.valor_garan <- kurtosis(var_con.valor_garan)
curtosis.valor_garan

var_con_curtosis <- data.frame(Valor.Inversión = curtosis.valor_inver, Colocación = curtosis.col, Vlr.Inic.Garantía = curtosis.valor_garan)
View(var_con_curtosis)

#Estadígrafos de posición
#Cuartiles
Q1.valor_inver <- quantile(var_con.valor_inver, probs = c(0.25, 0.5, 0.75))
Q1.valor_inver
Q1.col <- quantile(var_con.col, probs = c(0.25, 0.5, 0.75))
Q1.col
Q1.valor_garan <- quantile(var_con.valor_garan, probs = c(0.25, 0.5, 0.75))
Q1.valor_garan

var_con_Q1 <- data.frame(Valor.Inversión = Q1.valor_inver, Colocación = Q1.col, Vlr.Inic.Garantía = Q1.valor_garan)
View(var_con_Q1)

#Percentiles
percentil.valor_inver <- quantile(var_con.valor_inver, probs = c(0.1, 0.5, 0.90))
percentil.valor_inver
percentil.col <- quantile(var_con.col, probs = c(0.1, 0.5, 0.90))
percentil.col
percentil.valor_garan <- quantile(var_con.valor_garan, probs = c(0.1, 0.5, 0.90))
percentil.valor_garan

var_con_percentil <- data.frame(Valor.Inversión = percentil.valor_inver, Colocación = percentil.col, Vlr.Inic.Garantía = percentil.valor_garan)
View(var_con_percentil)

#Deciles
decil.valor_inver <- quantile(var_con.valor_inver, probs = seq(0.1, 0.9, by = 0.1))
decil.valor_inver
decil.col <- quantile(var_con.col, probs = seq(0.1, 0.9, by = 0.1))
decil.col
decil.valor_garan <- quantile(var_con.valor_garan, probs = seq(0.1, 0.9, by = 0.1))
decil.valor_garan

var_con_decil <- data.frame(Valor.Inversión = decil.valor_inver, Colocación = decil.col, Vlr.Inic.Garantía = decil.valor_garan)
View(var_con_decil)

#Máximos
máximo.valor_inver <- max(var_con.valor_inver)
máximo.valor_inver
máximo.col <- max(var_con.col)
máximo.col
máximo.valor_garan <- max(var_con.valor_garan)
máximo.valor_garan

var_con_máximo <- data.frame(Valor.Inversión = máximo.valor_inver, Colocación = máximo.col, Vlr.Inic.Garantía = máximo.valor_garan)
View(var_con_máximo)

#Mínimo
mínimo.valor_inver <- max(var_con.valor_inver)
mínimo.valor_inver
mínimo.col <- max(var_con.col)
mínimo.col
mínimo.valor_garan <- max(var_con.valor_garan)
mínimo.valor_garan

var_con_mínimo <- data.frame(Valor.Inversión = mínimo.valor_inver, Colocación = mínimo.col, Vlr.Inic.Garantía = mínimo.valor_garan)
View(var_con_mínimo)

#Tabla resumen variables continuas
as.data.frame(descr(var_con_act))
view(descr(var_con_act))

#Graficos
#Estadígrafos de Tendencia Central
vector_media2 <- c(media.valor_inver, media.col, media.valor_garan)
media2 <- mean(vector_media2)
vector_mediana2 <- c(mediana.tipo_prod, mediana.mes, mediana.id_depto, mediana.muncol, mediana.depcol, mediana.plazo, mediana.id_rubro, mediana.fag, mediana.cant)
mediana2 <- median(vector_mediana2)
vector_moda2 <- c(moda.tipo_prod, moda.mes, moda.id_depto, moda.muncol, moda.depcol, moda.plazo, moda.id_rubro, moda.fag, moda.cant)
moda2 <- as.numeric(names(sort(table(vector_moda2), decreasing = TRUE)[1]))
mi_vector2 <- c(vector_media2, vector_mediana2, vector_moda2)
datos <- data.frame(valores = mi_vector2)
ggplot(datos, aes(x = valores)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = media), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = mediana), color = "green", linetype = "dotted", size = 1.2) +
  geom_vline(aes(xintercept = moda), color = "blue", linetype = "solid", size = 1.2) +
  geom_text(aes(x = media, y = 5, label = "Media"), color = "red", vjust = -0.5, hjust = -0.1) +
  geom_text(aes(x = mediana, y = 5, label = "Mediana"), color = "green", vjust = -0.5, hjust = -0.1) +
  geom_text(aes(x = moda, y = 5, label = "Moda"), color = "blue", vjust = -0.5, hjust = -0.1) +
  labs(x = "Valores",
       y = "Frecuencia") +
  theme_minimal()

#Estadígrafos de Dispersión
vector_rango2 <- c(rango.valor_inver, rango.col, rango.valor_garan)
rango2 <- max(vector_rango2) - min(vector_rango2)
vector_varianza2 <- c(varianaza.valor_inver, varianaza.col, varianaza.valor_garan)
varianza2 <- var(vector_varianza2)
vector_sd2 <- c(desviación.valor_inver, desviación.col, desviación.valor_garan)
desv_estandar2 <- sd(vector_sd2)
coef_var2 <- (desv_estandar2 / mean(vector_sd2)) * 100
vector_rango_inter2 <- c(ran_inter.valor_inver, ran_inter.col, ran_inter.valor_garan)
rango_intercuartilico2 <- IQR(vector_rango_inter2)
vector_dispersion <- c(rango2, varianza2, desv_estandar2, coef_var2, rango_intercuartilico2)
estadigrafos <- c("Rango", "Varianza", "Desv. Estándar", "Coef. Var.", "Rango Intercuartílico")
datos_dispersion <- data.frame(valores = vector_dispersion, estadigrafo = estadigrafos)
ggplot(datos_dispersion, aes(x = estadigrafo, y = valores)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = round(valores, 2)), vjust = -0.5, color = "black") +
  labs(x = "Estadígrafos",
       y = "Valores") +
  theme_minimal()

#Estadígrafos de Forma
vector_asimetria2 <- c(asimetria.valor_inver, asimetria.col, asimetria.valor_garan)
vector_curtosis2 <- c(curtosis.valor_inver, curtosis.col, curtosis.valor_garan)
asimetria2 <- skewness(vector_asimetria2)  
curtosis2 <- kurtosis(vector_curtosis2)  
vector_forma2 <- c(asimetria2, curtosis2)
nombres_forma <- c("Asimetría", "Curtosis")
datos_forma <- data.frame(valores = vector_forma2, estadigrafo = nombres_forma)
ggplot(datos_forma, aes(x = estadigrafo, y = valores)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = round(valores, 3)), vjust = -0.5, color = "black") +
  labs(x = "Estadígrafos",
       y = "Valores") +
  theme_minimal()

#Estadígrafos de Posición
cuartiles2 <- c(Q1.valor_inver, Q1.col, Q1.valor_garan)
percentil2 <- seq(10, 90, by = 10)
percentil22 <- c(percentil.valor_inver, percentil.col, percentil.valor_garan)
percentiles2 <- c(percentil2, percentil22)
decil2 <- seq(10, 90, by = 10)
decil22 <- c(decil.valor_inver, decil.col, decil.valor_garan)
deciles2 <- c(decil2, decil22)
maximo2 <- c(máximo.valor_inver, máximo.col, máximo.valor_garan) 
minimo2 <- c(mínimo.valor_inver, mínimo.col, mínimo.valor_garan)    
vector_posicion <- c(cuartiles2, percentiles2, deciles2, maximo2, minimo2)
estadigrafos <- c(rep("Cuartiles", length(cuartiles2)),
                  rep("Percentiles", length(percentiles2)),
                  rep("Deciles", length(deciles2)),
                  rep("Máximo", length(maximo2)),
                  rep("Mínimo", length(minimo2)))
datos_posicion <- data.frame(
  valores = vector_posicion,
  estadigrafo = estadigrafos)
ggplot(datos_posicion, aes(x = valores, fill = estadigrafo)) +
  geom_histogram(binwidth = 10000000000, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~estadigrafo, scales = "free") +
  labs(x = "Valores",
       y = "Frecuencia") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

#Identificación distribución de los datos
for (var in names(baseFINAGRO)) {
  if (is.numeric(baseFINAGRO[[var]])) {
    hist(baseFINAGRO[[var]], main = paste("Histograma de", var), xlab = var)
    plot(density(baseFINAGRO[[var]], na.rm = TRUE), main = paste("Densidad de", var), xlab = var)
  }
}

#Pruebas de Hipótesis
#Para la media de una variable cuantitativa en una población
prueba <- t.test(var_dis_media, mu = 15000)
prueba

#Para la proporción de categoría en una población
Municipio.de.Postconflicto <- c(var_cat_post)
p_0 <- 0.5  # Proporción hipotética
x <- sum(Municipio.de.Postconflicto == 1)  # Número de éxitos
n <- length(Municipio.de.Postconflicto)   # Tamaño de la muestra
prop_pob <- prop.test(x = x, n = n, p = p_0)
prop_pob

#Para una varianza
mi_muestra <- c(varianza.tipo_prod2, varianza.mes2, varianza.id_depto2, varianza.id_munic2, varianza.muncol2, varianza.depcol2, varianza.plazo2, varianza.id_rubro2, varianza.fag2, varianza.cant2)
varianza_hipotetica <- 25
# Calcular la varianza muestral y tamaño de muestra
varianza_muestral <- var(mi_muestra)
n <- length(mi_muestra)
# Estadístico chi-cuadrado
chi_cuadrado <- (n - 1) * varianza_muestral / varianza_hipotetica
# Valor p para prueba bilateral
p_value <- 2 * min(pchisq(chi_cuadrado, df = n - 1), 1 - pchisq(chi_cuadrado, df = n - 1))
# Mostrar resultados
cat("Estadístico chi-cuadrado:", chi_cuadrado, "\n")
cat("Valor p:", p_value, "\n")
#cálculo del valor p muy pequeño
p_value <- 2 * pnorm(-10)
# Mostrar el valor con más precisión
format(p_value, scientific = TRUE)

#Ajustes de Bondad
#Variable Discreta
observados <- c(var_dis.plazo)  # Frecuencias observadas en cada categoría
total <- sum(observados)
esperados <- observados / total  # Frecuencias esperadas según la hipótesis
# Prueba de chi-cuadrado
resultado <- chisq.test(observados, p = esperados / sum(esperados))
resultado

#Variabale Categórica 1
observados2 <- c(var_cat_act$Género)  # Frecuencias observadas en cada categoría
total2 <- sum(observados)
esperados2 <- observados2 / total2  # Frecuencias esperadas según la hipótesis
# Prueba de chi-cuadrado
resultados <- chisq.test(observados2, p = esperados2 / sum(esperados2))
resultados

#Variable Categórica 2
observados3 <- c(var_cat_act$Tipo.Productor)  # Frecuencias observadas en cada categoría
total3 <- sum(observados)
esperados3 <- observados3 / total3  # Frecuencias esperadas según la hipótesis
# Prueba de chi-cuadrado
resultados2 <- chisq.test(observados3, p = esperados3 / sum(esperados3))
resultados2

#Pruebas de Kolmogorov-Smirnov
#Prueba 1
var_con <- c(var_con.valor_inver)
#Agregar ruido pequeño para romper empates
var_con_jitter <- var_con + runif(length(var_con), min = -0.01, max = 0.01)
#Prueba de Kolmogorov-Smirnov para comparar con una normal(10, 2)
ks.test(var_con_jitter, "pnorm", mean = 10, sd = 2)

#Prueba 2
var_con2 <- c(var_con.col)
#Agregar ruido pequeño para romper empates
var_con_jitter2 <- var_con2 + runif(length(var_con2), min = -0.01, max = 0.01)
#Prueba de Kolmogorov-Smirnov para comparar con una normal(10, 2)
ks.test(var_con_jitter2, "pnorm", mean = 10, sd = 2)

#Prueba 3
var_con3 <- c(var_con.valor_garan)
#Agregar ruido pequeño para romper empates
var_con_jitter3 <- var_con3 + runif(length(var_con3), min = -0.01, max = 0.01)
#Prueba de Kolmogorov-Smirnov para compararla con una normal(10, 2)
ks.test(var_con_jitter3, "pnorm", mean = 10, sd = 2)

#Selección de las variables cualitativas
var_cual <- datos_flitrados %>% select("Departamento.Inversión", "Municipio.Inversión", "Departamento.de.Colocación.de.Crédito", "Municipio.Colocación.de.Crédito", "Línea.de.Producción", "Destino.de.Crédito")
var_cual