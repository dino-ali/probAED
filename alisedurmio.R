#enero 31 ____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#ingreso de vectores
x = c(1, 4)
y = c(5, 1)
z = c(4, -2)

x+y+z
x+y-2*z

#matrices
datos_a = c(2, -3, 1, 0, 1, 5, 4, 5, 3, 0, -1, 6)
datos_b = c(4, 7, -1, 2, 0, 6, 3, 1, -2, 1, 1, 4)
a = matrix(datos_a, ncol=4, byrow=TRUE);a
b = matrix(datos_b, ncol=4, byrow=TRUE);b

a+b
olis<-t(a)%*%b
# va %*% porque es producto matricial y t(a) es la transpuesta de a :).

#determinante
det(olis)
traza =(sum(diag(olis))); traza

c = matrix(c(1,2,3,4,5,8,9,10,9), ncol=3, byrow=TRUE)
round(solve(c), 3) #inversa

#valores y vectores propios
AA = matrix (c(20,18,17,19,25,23,21,34,27), ncol=3, byrow=TRUE);AA
g = eigen(AA)
valores_p = g$values;valores_p
valores_p[1] #primer valor propio
vectores_p = g$vectors; vectores_p

#distanciaS
q = c(1.80, 80); Q = c(180, 80)
w = c(1.70, 72); W = c(170, 72)
e = c(1.65, 81); E = c(1.65, 81)

d_qw = (1.8-1.7)^2 + (80-72)^2; d_qw
d_qe = (1.8-1.65)^2 + (80-81)^2; d_qe
d_we = (1.7-1.65)^2 + (72-81)^2; d_we

d_QW = (180-170)^2 + (80-72)^2; d_QW
d_QE = (180-165)^2 + (80-81)^2; d_QE
d_WE = (170-165)^2 + (72-81)^2; d_WE

SD = 10
S2= 10
CC = 0.7

DM = (1/(1-CC^2))*((180-170)^2/SD^2 + (80-72)^2/S2^2 -2*CC*(180-170)*(80-72)/ (SD*S2));DM

AAA = matrix(c (180,170,80,72), byrow=TRUE, ncol=2); AAA
vm = colMeans(AAA); vm
S = cov(AAA)
DM1 = mahalanobis(AAA, vm, S)
