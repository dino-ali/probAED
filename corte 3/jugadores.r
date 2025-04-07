library(readxl)
Jugadores_Pract <- read_excel("C:/Users/prestamour/Downloads/Jugadores Pract.xlsx")
View(Jugadores_Pract)

jugadores = Jugadores_Pract[, -c(1:4)]
row.names(jugadores) = Jugadores_Pract$Jugador
View(jugadores)

#análisis de componentes
library(FactoMineR)
res.pca = PCA(jugadores, graph = FALSE)
print(res.pca) #observa los parametros de salida de la función

#visualización e interpretación
library(factoextra)
eig.val = get_eigenvalue(res.pca); eig.val #tomamos los valores propios mayores a 1

sum(eig.val[, 1])
(eig.val[,1]/sum(eig.val[, 1]))*100  #la varianza retenida por la componente

#gráfica
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

#coordenadas de las variables, basado en correlaciones
res.pca$var$coord

#visualización
fviz_pca_var(res.pca, col.var = "cos2", gradient.cols = c("palegreen2", "royalblue", "violetred1"), repel = TRUE)

#calidad de la representación:
var = get_pca_var(res.pca); var
head(var$cos2)
