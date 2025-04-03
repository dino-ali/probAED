# EJERCICIOS PARA ESTUDIAR AED (corte 1) ALI LLORO CON SUEÑO Y CON HAMBRE PERO LO TERMINO A LAS 23.26 DEL 20 DE FEBRERO EN LUGAR DE CELEBRAR CON SU NOVIO LOS 2 AÑOS QUE SE LA HA AGUANTADO
# 1.a. Verifique la hiṕotesis anterior para cada una de las medias en forma separada.
#Escriba los supuestos estad́ısticos requeridos, use α = 0.05.

Y1 = matrix(c(3, 10, 6, 12, 5, 14, 10, 9), ncol = 2, byrow = TRUE); Y1
mu_p1 = c(6, 11); mu_p1 #vector de medias (en realidad son nuestra hipótesis nula)
Sigma1 = matrix(c(9.2, -3, -3, 5.3), ncol = 2, byrow = TRUE); Sigma1
alpha1 = 0.05; alpha1

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

# ____________________________________________________________________________________________________________________________________________________

# 2. lo mismo del uno con otros numeros xd.
Y2 = matrix(c(2, 8, 6, 8, 12, 9, 9, 10), ncol = 2); Y2
mu_p2 = c(7, 11)
Sigma2 = matrix(c(8, -3.5, -3.5, 2), ncol = 2); Sigma2

socks2 = colMeans(Y2); socks2
n2 = nrow(Y2); n2
inv.Sigma2 = solve(Sigma2); inv.Sigma2

chi_2 = qchisq(alpha1, df=2, lower.tail=FALSE); chi_2

Z2_1 = (socks2[1] - mu_p2[1]) / (sqrt(Sigma2[1, 1]) / sqrt(n2)); Z2_1
Z2_2 = (socks2[2] - mu_p2[2]) / (sqrt(Sigma2[2, 2]) / sqrt(n2)); Z2_2

chi2_2 = n2 * t(socks2 - mu_p2) %*% inv.Sigma2 %*% (socks2 - mu_p2); chi2_2

# _____________________________________________________________________________________________________________________________________________________

# 3. sea X ~ N_3 con
Sigma3 = matrix(c(4, -3, 0, -3, 6, 0, 0, 0, 5), ncol = 3); Sigma3
dimnames(Sigma3) = list(c("X1", "X2", "X3"), c("X1", "X2", "X3")) #esto es para ponerle nombres a las filas y columnas y que se vea bonito:3.
# ¿cual de las siguientes variables son independientes?
  # a ¿X1 y X2? no son independientes porque su valor NO es 0.
  # b ¿X1 y X3? son independientes porque su valor es 0.
  # c ¿X2 y X3? son independientes porque su valor es 0.
  # d ¿(X1, X2) y X3? son independientes porque ambos por separado son independientes de X3 (como si X3 se distribuyera sobre (X1 y X2))
  # e ¿(X1, X3) y X2? NO son independientes en "total", porque X1 y X2 no son independientes.


# en general: si X_c es independiente de X_a y también es independiente de X_b, entonces el par (X_a, X_b) también es independiente de X_c.
