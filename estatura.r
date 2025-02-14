vm = colMeans(estatura); vm
Sigma = matrix(c(20, 100, 100, 1000), byrow = TRUE, ncol = 2); Sigma
inv.Sigma = solve(Sigma); inv.Sigma
n = nrow(estatura); n #cuantas filas tiene la base
mu_0 = c(70, 170); mu_0
chi2.0 = n*t(vm - mu_0)%*%inv.Sigma%*%(vm - mu_0); chi2.0

p = ncol(estatura); p
alpha = 0.05
chi = qchisq(alpha, p, lower.tail = FALSE); chi

#pruebas por separado
Z1 = (vm[1] - mu_0[1])/(sqrt(Sigma[1, 1])/sqrt(n)); Z1
Z2 = (vm[2] - mu_0[2])/(sqrt(Sigma[2, 2])/sqrt(n)); Z2
 
