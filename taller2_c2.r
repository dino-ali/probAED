#Taller 2, Corte 2.
library(readxl)
craneo <- read_excel("C:/Users/prestamour/Downloads/craneo.xlsx")
View(craneo)

# ---------------------------------------------------------------------------------------------------------------------------------------
#Realice un diagrama de cajas y bigotes para cada variable por año.
par(mfrow=c(2,2)) 
#x1
boxplot((craneo$X1) ~ (craneo$Year), 
        main="X1 por año",
        xlab="Años", 
        ylab="medida en mm",
        col= c("darkorchid", "hotpink", "plum", "lightskyblue", "mediumvioletred"))

#x2
boxplot((craneo$X2) ~ (craneo$Year), 
        main="X2 por año",
        xlab="Años", 
        ylab="medida en mm",
        col= c("darkcyan", "tomato", "goldenrod1", "slateblue3", "seagreen3"))

#x3
boxplot((craneo$X3) ~ (craneo$Year), 
        main="X3 por año",
        xlab="Años", 
        ylab="medida en mm",
        col= c("royalblue3", "firebrick2", "gold3", "purple3", "forestgreen"))


#x4
boxplot((craneo$X4) ~ (craneo$Year), 
        main="X4 por año",
        xlab="Años", 
        ylab="medida en mm",
        col= c("steelblue", "darkorange", "chartreuse4", "mediumvioletred", "darkslategray"))

par(mfrow=c(1,1))

# ---------------------------------------------------------------------------------------------------------------------------------------
# Realice ANOVA para cada una de las variables
#ANOVA - X1
modelo1 = aov(craneo$X1 ~ craneo$Year);modelo1
ANOVA1 = anova(modelo1); ANOVA1
#ANOVA - X2
modelo2 = aov(craneo$X2 ~ craneo$Year);modelo2
ANOVA2 = anova(modelo2); ANOVA2
#ANOVA - X3
modelo3 =aov(craneo$X3 ~ craneo$Year);modelo3
ANOVA3 = anova(modelo1); ANOVA3
#ANOVA - X4
modelo4 = aov(craneo$X4 ~ craneo$Year);modelo4
ANOVA4 = anova(modelo1); ANOVA4
#ANOVA - X1
modelo1=aov(craneo$X1 ~ craneo$Year);modelo1
ANOVA1=anova(modelo1); ANOVA1


# ---------------------------------------------------------------------------------------------------------------------------------------
# Construya un MANOVA unidireccional de los datos del cráneo egipcio. Use un nivel de significancia de 0.05.
datos=as.matrix(craneo[,-c(4)])
class(datos)

ajuste=manova(datos~`Year`,data=craneo)

ajuste = manova(skull$Year, data = craneo)
summary(ajuste, test="Wilks")
