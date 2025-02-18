# Se crea una matriz 4x2 llamada Y con los datos proporcionados
Y = matrix(c(2, 12, 8, 9, 6, 9, 8, 10), ncol=2, byrow=TRUE)

# Se define un vector de medias poblacionales hipotéticas (mu_p)
mu_p = c(7, 11)

# Se define la matriz de covarianza poblacional (Sigma)
Sigma = matrix(c(8, -3.5, -3.5, 2), ncol = 2, byrow=TRUE)

# Se calcula la inversa de la matriz de covarianza
inv.Sigma = solve(Sigma); inv.Sigma

# Nivel de significancia para la prueba de hipótesis
alpha = 0.05

# Se calcula el vector de medias muestrales a partir de Y
h = colMeans(Y)

# Se obtiene el tamaño muestral (número de filas de Y)
n = nrow(Y); n

# Se obtiene el valor crítico de la distribución chi-cuadrado con 2 grados de libertad
chi = qchisq(alpha, df=2, lower.tail=FALSE); chi

# Cálculo de valores estandarizados Z para cada variable
Z1 = (h[1] - mu_p[1])/(sqrt(Sigma[1, 1])/sqrt(n)); Z1
Z2 = (h[2] - mu_p[2])/(sqrt(Sigma[2, 2])/sqrt(n)); Z2

# Prueba de hipótesis multivariada usando la distancia de Mahalanobis
chi2.0 = n*t(h - mu_p)%*%inv.Sigma%*%(h - mu_p); chi2.0

# Librería para leer archivos CSV
library(readr)

# Se carga el archivo 'hemotology.csv' desde la ruta especificada
hemotology <- read_delim("C:/Users/prestamour/Downloads/hemotology.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Se muestra el contenido del dataset en una ventana
View(hemotology)

# Estimación de máxima verosimilitud del vector de medias (mu)
mu_hat = colMeans(hemotology); mu_hat

# Estimación de máxima verosimilitud de la matriz de covarianza (Sigma)
Sigma_hat = cov(hemotology) * (nrow(hemotology)-1) / nrow(hemotology)

# Se imprimen las estimaciones de medias y covarianza
mu_hat; Sigma_hat

# Prueba de normalidad univariada: Shapiro-Wilk
pn.sw = mvn(data=hemotology, univariateTest = "SW", desc = TRUE); pn.sw

# Prueba de normalidad univariada: Anderson-Darling
pn.AD = mvn(data=hemotology, univariateTest = "AD", desc = TRUE); pn.AD

# Prueba de normalidad univariada: Kolmogorov-Smirnov Lilliefors
# (Esta prueba asume que la media y la varianza son desconocidas)
pn.L = mvn(data=hemotology, univariateTest = "Lillie", desc = TRUE); pn.L

# Prueba de normalidad multivariada usando el test de Mardia
pn.m = mvn(data = hemotology, mvnTest = "mardia"); pn.m

# Se extraen los resultados específicos de la prueba de normalidad multivariada
pn.m$multivariateNormality
