#febrero 10
x1 = c (1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4)
x2 = c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0)
hs = cbind(x1,x2) ; hs
mu_est = colMeans(hs); mu_est
#el medicamento A incrementa las horas de sueño en 2.33 horas mientras que el B, 0.75 horas

s = round(cov(hs), 3); s
r = round(cor(hs), 3); r

#normalidad
setwd("/AED")

library(readx1)
candy <- read_excel(path="C:\Users\prestamour\Downloads\Dulces.xls", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
library(MVN)
#grafica
his.mardia = mvn(data = Dulces
                 mvnTest = "mardia"
                 univariatePlot = "histogram")
