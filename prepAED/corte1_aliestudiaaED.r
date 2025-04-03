# EJERCICIOS PARA ESTUDIAR AED (corte 1)
# 1.
# a. Verifique la hiṕotesis anterior para cada una de las medias en forma separada.
#Escriba los supuestos estad́ısticos requeridos, use α = 0.05.

Y1 = matrix(c(3, 10, 6, 12, 5, 14, 10, 9), ncol = 2, byrow = TRUE); Y
mu_p1 = c(6, 11); mu_p #vector de medias
Sigma1 = matrix(c(9.2, -3, -3, 5.3), ncol = 2, byrow = TRUE); Sigma
alpha1 = 0.05; alpha

#siempre saca el calculo del vector de medias de la matriz, el tamaño muestral y la inversa de la matriz de covarianza para usarla en Mahalanobis
socks = colMeans(Y1); socks
n1 = nrow(Y1); n1
inv.Sigma1 = solve(Sigma1); inv.Sigma1

# primero saca el valor crítico de chi-cuadrado para comparar después. como tenemos 2 variables, los grados de libertad son 2, y usamos alfa = 0.05
chi1 = qchisq(alpha1, df=2, lower.tail=FALSE); chi1
#Usamos lower.tail = FALSE porque quiero el valor en la cola derecha de la distribución,
# es decir, el punto de corte para el 5% más extremo.

# Ahora reviso cada media por separado con una prueba Z.
# Básicamente, es para ver cuánto se aleja la media muestral de la media teórica, pero considerando el tamaño de la muestra y la varianza.
Z1_1 = (socks[1] - mu_p1[1]) / (sqrt(Sigma1[1, 1]) / sqrt(n1)); Z1_1
Z1_2 = (socks[2] - mu_p1[2]) / (sqrt(Sigma1[2, 2]) / sqrt(n1)); Z1_2

# Ahora, la prueba multivariada usando distancia de Mahalanobis.
# Esto sirve para ver si en conjunto las medias de la muestra son diferentes de las teóricas.
chi2_1 = n1 * t(socks - mu_p1) %*% inv.Sigma1 %*% (socks - mu_p1); chi2_1

library(skimr)
skim(Y1)
