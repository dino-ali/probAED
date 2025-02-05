# febrero 5 ____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#distribución normal
mu = 25
sigma = 2
P21 = pnorm(21, mu, sigma) ; P21
P22 = pnorm(22, mu, sigma, lower.tail = FALSE); P22
P23_27 = pnorm(27, mu, sigma) - pnorm(23, mu, sigma); P23_27

alpha = 0.05
z = qnorm(alpha); z
y0 = z*sigma+mu; y0
y0 = qnorm (alpha, mu, sigma); y0

# shift+alt+flecha, lo copia
#alt+flecha, lo mueve
install.packages("mvtnorm")
library(mvtnorm)
Mu = c(150, 165)
Sigma = matrix(c(25, 10, 10, 20), ncol=2, byrow =TRUE); Sigma
n = 50
peso = seq(from = 130, to = 170, length.out = n)
estatura = seq(from = 145, to = 185, length.out = n)
densidad = function(x1, x2) dmvnorm(cbind(x1, x2), Mu, Sigma)

Z = outer(peso, estatura, FUN = "densidad")
persp(peso, estatura, Z, theta = 45, phi = 30, xlab = "", ylab = "", zlab = "", ticktype = "detailed", nticks = 4, col = "lightpink")
title(main = "distribución normal bivariada")

