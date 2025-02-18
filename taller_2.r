Y = matrix(c(2, 12, 8, 9, 6, 9, 8, 10), ncol=2, byrow=TRUE)
mu_p = c(7, 11)
Sigma = matrix(c(8, -3.5, -3.5, 2), ncol = 2, byrow=TRUE)
inv.Sigma = solve(Sigma); inv.Sigma
alpha = 0.05
h = colMeans(Y)
n = nrow(Y); n
chi = qchisq(alpha, df=2, lower.tail=FALSE); chi

Z1 = (h[1] - mu_p[1])/(sqrt(Sigma[1, 1])/sqrt(n)); Z1
Z2 = (h[2] - mu_p[2])/(sqrt(Sigma[2, 2])/sqrt(n)); Z2

chi2.0 = n*t(h - mu_p)%*%inv.Sigma%*%(h - mu_p); chi2.0

library(readr)
hemotology <- read_delim("C:/Users/prestamour/Downloads/hemotology.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(hemotology)
# Estimación de máxima verosimilitud para el vector de medias (mu)
mu_hat = colMeans(hemotology); mu_hat
# Estimación de máxima verosimilitud para la matriz de covarianza (Sigma)
Sigma_hat = cov(hemotology) * (nrow(hemotology)-1) / nrow(hemotology)
mu_hat;Sigma_hat

#shapiro wilk (prueba univariada de normalidad):
pn.sw = mvn(data=hemotology, univariateTest = "SW",desc = TRUE); pn.sw

#anderson darling
pn.AD = mvn(data=hemotology, univariateTest = "AD",desc = TRUE); pn.AD

#Kolmogorov-Smirnov Lilliefors (asume que la media y la varianza son desconocidas):
pn.L = mvn(data=hemotology, univariateTest = "Lillie",desc = TRUE); pn.L

#normalidad multivariada (debe pasar ambas pruebas: asimetría y curtosis):
pn.m = mvn (data = hemotology, mvnTest = "mardia"); pn.m
pn.m$multivariateNormality
 
