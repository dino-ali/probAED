library(readr)
banco <- read_csv("C:/Users/prestamour/Downloads/banco.csv")
View(banco)

banco = banco[, -1]
str(banco)

library(ggplot2)
ggplot(banco)+aes(x = Patrimonio, y = Deuda, color = Grupo) + geom_point() + theme_bw()
#dde la grafica se puede observear que aquellos clientes con un alto patrimonio tenian deudas bajas al momento de la solicitud.

#prueba de normalidad.
library(MVN)
mvn(banco[, c("Patrimonio", "Deuda")], mvnTest = "mardia")$multivariateNormality

deudores = subset(banco, Grupo == "Deudor")
Nodeudores = subset(banco, Grupo == "No deudor")
mvn(deudores[, c("Patrimonio", "Deuda")], mvnTest = "mardia")$multivariateNormality
mvn(Nodeudores[, c("Patrimonio", "Deuda")], mvnTest = "mardia")$multivariateNormality
#para ambos grupos las variables patrimonio y deuda se distribuyen normal a un nivel de significancia de 5%.

#prueba de igualdad de varianzas
library(biotools)
boxM(banco[, c("Patrimonio", "Deuda")], banco$Grupo)
#como el pvalor>alpha, NO se rechaza la hipotesis nula por tanto las matrices de varianzas y covarianzas son iguales

#modelo de analisis discriminante: 
#dado que se asume la igualdad en la matriz  de varianzas y covarianzas, se debe hacer un analisis discriminante lineal.
modelo.lda = lda(Grupo~Patrimonio+Deuda, data = banco); modelo.lda

#predicción:
new.obs = data.frame(Patrimonio = 10.1, Deuda = 9.7)
predict(object = modelo.lda, newdata = new.obs)
#de acá se observa que existe una probabilidad de 0.89 de que la persona se clasifique como deudor.

#______________________________________________________________________________________________________________________________________
#modelo usando un conjunto de datos de entrenamiento y otro de prueba
library(caret)

#entrenamiento 80%, prueba 20%
set.seed(07052025)
ind = createDataPartition(banco$Grupo, p = 0.7, list = FALSE)
entrenamiento = banco[ind,]
prueba = banco[-ind,]

#modelo
library(biotools)
boxM(entrenamiento[, c("Patrimonio", "Deuda")], entrenamiento$Grupo)
modelo.lda.ent = lda(Grupo~Patrimonio+Deuda, data = entrenamiento); modelo.lda.ent

#predicción
new.obs1 = data.frame(Patrimonio = 10.1, Deuda = 9.7)
predict(object = modelo.lda, newdata = new.obs1)
predict.LDA = predict(modelo.lda.ent, prueba)

#matriz de confusión
MC.prueba = confusionmatrix(predict.LDA$class, prueba$Grupo); MC.prueba
