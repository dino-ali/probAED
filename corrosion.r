library(readr)
pruebaspsico <- read_delim("C:/Users/prestamour/Downloads/pruebaspsico.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(pruebaspsico)

library(dplyr)

hombres = pruebaspsico%>%filter(Sexo =="1")
mujeres = pruebaspsico%>%filter(Sexo =="2")

vm1 = colMeans(hombres[, -1]); vm1
vm2 = colMeans(mujeres[, -1]); vm2
S1 = cov(hombres[, -1]); S1
S2 = cov(mujeres[, -1]); S2
n1 = nrow(hombres); n1
n2 = nrow(mujeres); n2
p = ncol(hombres[, -1]); p
gl = n1+n2-2; gl

Sp = (S1*(n1-1) + S2*(n2-1))/gl; Sp

T2 = (n1*n2)/(n1+n2) * t(vm1-vm2)%*%solve(Sp)%*%(vm1-vm2); T2
alpha = 0.05

f = qf(alpha, p, gl-p+1, lower.tail = FALSE); f
vc = (gl*p)/(gl-p+1)*f; vc

#muestras pareadas
A = c(10.6, 9.8, 12.3, 9.7, 8.8)
B = c(10.2, 9.4, 11.8, 9.1, 8.3)
t.test(A, B, paired = TRUE)

library(readr)
corrosion <- read_delim("C:/Users/prestamour/Downloads/corrosion.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(corrosion)

d1 = corrosion$X1-corrosion$Y1; d1
d2 = corrosion$X2-corrosion$Y2; d2

A = cbind(d1, d2); A
n = nrow(A); n
vmd = colMeans(A); vmd
Sd = cov(A); Sd

T2 = n*t(vmd)%*%solve(Sd)%*%(vmd); T2
p = ncol(A); p
alpha = 0.05
f = qf(alpha, p n-1, lower.tail = FALSE); f

estadistico = T2
