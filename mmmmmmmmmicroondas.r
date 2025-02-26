calcio <- read.csv("C:/Users/prestamour/Downloads/calcio.csv", sep=";")
View(calcio)

# H0: mu = (15, 6, 2.85)
mu = c(15, 6, 2.85)
alpha = 0.05
n = nrow(calcio) #lohiceantesquelaaprofeeeeeeeeeeeeeeeeeeeeeeeee
vm = colMeans(calcio)
s = cov(calcio)
S = solve(s)
t2= n*t(vm-mu)%*%S%*%(vm-mu); t2

#punto critico
alpha = 0.05
p = ncol(calcio)
f = qf(alpha, p, n-p, lower.tail = FALSE); f

vc = (n-1)/(n-p)*p*f; vc

#si t2 es mas grande que vc: se rechaza
#como en este caso si es mas grande, se rechaza la hipotesis nula, es decir que el vector mu =!(15, 6, 2.85)


library(mvtnorm)
library(MVTests)
MVTests::OneSampleHT2(calcio, mu0 = c(15, 6, 2.85))

#_______________________________________________________________________________
library(readxl)
microondas <- read_excel("C:/Users/prestamour/Downloads/microondas.xlsx", 
                         col_types = c("numeric", "numeric"))
View(microondas)

micro = microondas^(1/4); micro #transformación pq si.
n = nrow(micro); n #cantidad de filas
p = ncol(micro); p #cantidad de variables
vm = colMeans(micro); vm #vector de medias
S = cov(micro); S #matriz de covarianzas
alpha = 0.05; alpha

f = qf(alpha, p, n-p, lower.tail = FALSE); f #se prueba como si fuera una cola derecha
vc = (p*(n-1)/(n-p))*f; vc #punto critico
#como f es menor que vc podemos decir que está dentro de la región de confianza.

mu0 = c(0.562, 0.589); mu0
n*t(vm - mu0)%*%solve(S)%*%(vm - mu0)

#longitudes de los ejes
val = eigen(S); val
val.prop = val$values; val.prop
vec.prop = val$vectors; vec.prop

sqrt(val.prop[1])*sqrt(p*(n-1)*f/(n*(n-p)))
sqrt(val.prop[2])*sqrt(p*(n-1)*f/(n*(n-p)))

#direcciones:
e1 = vec.prop[, 1]; e1
e2 = vec.prop[, 2]; e2
   

#intervalos de confianza simultaneos
alpha = 0.05

#radiacion DC  
liminf = vm[1] - sqrt(p*(n-1)*f/(n*(n-p)))*sqrt(S[1, 1]/n); liminf
limsup = vm[1] + sqrt(p*(n-1)*f/(n*(n-p)))*sqrt(S[1, 1]/n); limsup
