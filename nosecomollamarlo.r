library(readr)
produccion <- read_delim("C:/Users/prestamour/Downloads/produccion.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(produccion)
modelo = aov(produccion$produccion~produccion$Metodo); modelo

ANOVA = anova(modelo); ANOVA

#comparaciones multiples
library(agricolae)
library(car)
#Pruebas de Tukey
interTukey = TukeyHSD(modelo, conf.level = 0.95); plot(interTukey)

#LSD
interLSD = LSD.test(produccion$produccion, produccion$Metodo, DFerror = ANOVA$Df[2], MSerror = ANOVA$`Mean Sq`[2], console = TRUE)
plot(interLSD)


#PRUEBA DE LOS SUPUESTOS
#NORMALIDAD
#Prueba de Shapiro
error = modelo$residuals
shapiro.test(error)

#prueba de BAgan Baguette algo asi. ( Breusch-Pagan)
library(stats)
bartlett.test(error~produccion$Metodo)


library(lmtest)
dwtest(modelo)

#_______________________________________________________________________________
library(readr)
metodoEnse <- read_csv("C:/Users/prestamour/Downloads/metodoEnse.csv", 
                       col_types = cols(metodo = col_character()))
View(metodoEnse)

# anova para matematicas
modelo.m = aov(metodoEnse$matemat~metodoEnse$metodo); modelo.m
ANOVA.m = anova(modelo.m); ANOVA.m

#anova para escritura
modelo.e = aov(metodoEnse$escrit~metodoEnse$metodo); modelo.e
ANOVA.e = anova(modelo.e); ANOVA.e

f = qf(0.05, 2,28, lower.tail = FALSE); f
