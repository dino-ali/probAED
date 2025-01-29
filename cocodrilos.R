datos=c(160, 64,46, 100,153, 20,9,22,30,39, 9,76, 30,22, 41,73, 13, 3.5, 17,16,20,4,164, 91,70, 90, 160, 36, 16, 42,32,57,20,72.3,40,37.3,35, 70.5,16.7,5.2, 20,15,24.6,10.5)

A=matrix(datos, ncol=11, byrow=TRUE);A
#el ;A es para que lo muestre en consola

#dizque quiero solo la media de la columna 1.
mean(A[,1])

#ahora la media de todas las columnas
colMeans(A)

#matriz de varaianzas y covarianzas
cov(A)

#matriz de correlaciones:
cor(A)

#redondeados
round(cor(A), 2)

#análisis rapido de la base
library(skimr)
skim(A)
