#Normalidad
setwd("~/AED")

library(readxl)
Dulces = read_excel("C:/Users/prestamour/Downloads/Dulces.xls")
View(Dulces)

library(MVN)

#gráfica
his.mardia=mvn(data=Dulces,#variables
               mvnTest = "mardia",
               univariatePlot = "histogram")

qqplot.mardia=mvn(data=Dulces,#variables
                  mvnTest = "mardia",
                  univariatePlot = "qqplot")

#shapiro wilk (prueba univariada de normalidad):
pn.sw = mvn(data=Dulces, univariateTest = "SW",desc = TRUE); pn.sw

#anderson darling
pn.AD = mvn(data=Dulces, univariateTest = "AD",desc = TRUE); pn.AD

#correcion de Kolmogorov-Smirnov Lilliefors (asume que la media y la varianza son desconocidas):
pn.L = mvn(data=Dulces, univariateTest = "Lillie",desc = TRUE); pn.L

#normalidad multivariada (debe pasar ambas pruebas: asimetría y curtosis):
pn.m = mvn (data = Dulces, mvnTest = "mardia"); pn.m
pn.m$multivariateNormality
 
  """
  H0: los datos se distribuyen de forma normal multivariada
  H1: los datos no se distribuyen de forma normal multivariada
  
  alpha = 0.05
  simetria, p-valor = 0.464
  como p-valor > alpha no se rechaza H0, con lo que se cumple el supuesto de simetría.
  curtosis, p-valor = 0.645
  como p-valor > alpha no se rechaza H0, con lo que se cumple el supuesto sobre la curtosis.
  
  como pasa ambas pruebas podemos concluir que hay normalidad multivariada
  """

#gráficas de perspectiva:
Dulces = as.matrix(Dulces)
windows()
contornos = mvn(Dulces[,1:2], mvnTest = "mardia",multivariatePlot = "contour"); contornos

windows()
perspectiva = mvn(Dulces[,1:2], mvnTest = "mardia",multivariatePlot = "persp"); perspectiva

#datos atípicos
windows()
atipicos.maha = mvn(data = Dulces, mvnTest = "mardia", multivariateOutlierMethod = "quan"); atipicos.maha

# H0 y H1 deben ser mutuamente excluyentes
