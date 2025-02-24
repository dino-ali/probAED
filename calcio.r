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

#si t2 es mas grande qie vc. se rechaza
#como en este caso si es mas grande, se rachasa la hipotesis nula, es decir que el vector mu =!(15, 6, 2.85)


library(mvtnorm)
library(MVTests)
MVTests::OneSampleHT2(calcio, mu0 = c(15, 6, 2.85))

