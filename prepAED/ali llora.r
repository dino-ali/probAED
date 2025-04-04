# EJERCICIOS PARA ESTUDIAR AED (corte 2)
# 1.a Verifique la hipótesis anterior para cada una de las medias en forma separada. 
# Escriba los supuestos estadísticos requeridos, use α = 0.05
Y = matrix(c(3, 10, 6, 12, 5, 14, 10, 9), ncol = 2, byrow = TRUE); Y
mu.p = c(6, 11) #H0 (hipótesis nula) 
alpha = 0.05

medias = colMeans(Y); medias # vector de medias de Y
n = nrow(Y); n #tamaño muestral en filas
p = ncol(Y); p #cantidad de variables
cov = cov(Y); cov #saca la matriz de covarianza porque no la tenemos xddd.
inv.cov = solve(cov); inv.cov #inversa de la matriz de covarianza de Y

#estadístico de prueba
X0=n*t((medias-mu.p))%*%solve(cov)%*%(medias-mu.p);X0
gl=ncol(Y)
alpha= 0.05 #nivel de significancia
X2=qchisq(1-alpha,gl);X2
  
#pruebas por separado
z1=(medias[1]-mu.p[1])/sqrt(cov[1,1]/n);z1
z2=(medias[2]-mu.p[2])/sqrt(cov[2,2]/n);z2

z=qnorm(alpha/2, lower.tail = FALSE);z

f=qf(alpha,p,n-p,lower.tail = FALSE);f
vc=(p*(n-1)/(n-p))*f;vc#punto crítico

#cuánto se aleja el estadístico (z_i) del valor nulo, sin importar si es positivo o negativo.
#¿La distancia (sin signo) del estadístico Z al cero es mayor al valor crítico (por ejemplo, 1.96)?
z1 > z #rechazo h0
z2 > z #rechazo h0
# si sale verdadero, rechazo h0 para esa media (z_i), de lo contrario, pues no xd.

# 1.b Verifique la hipótesis conjunta, use α = 0.05.
library(MVTests)
OneSampleHT2(Y, mu.p)
# en este caso como el p-valor es mayor al nivel de significancia α=0.05, no se rechaza la hipótesis nula.

# ____________________________________________________________________________________________________________________________________________________
library(readxl)
Words <- read_excel("macc/2025-1/a n á l i s i s  e s t a d í s t i c o  d e  d a t o s/Words.xlsx")
Words = Words[, -1]
View(Words)
# 2.a Pruebe H0 : µ = (30, 25, 40, 25, 30)
mu = c(30, 25, 40, 25, 30); mu
OneSampleHT2(Words, mu, alpha = 0.05)
# aunque sale FALSE debo mirar el p-valor , dio 0.0067, como es menor a 0.05 tenemos evidencia suficiente para rechazar la hipotesis nula

# 2.b Si la hipótesis nula se rechaza, pruebe cada variable separadamente.
n = nrow(Words); n
p = ncol(Words); p
sock = colMeans(Words); sock
s = cov(Words); s

#ya tengo demasiado sueño, adios ali del futuro. llorala
