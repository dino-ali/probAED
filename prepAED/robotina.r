# ✨ Script parcial 2 explicado con amor por Robotina 💕🧠
# Este script es un repaso de todo lo que puede salir en tu parcial de estadística multivariada.
# Está ordenadito por secciones y cada línea te dice para qué sirve, como si te lo contara una amiga 🤓.

# ------------------------
# MATRIZ DE COVARIANZAS CONOCIDA
# ------------------------

Sigma=matrix(c(), byrow = TRUE, ncol = 2); Sigma  # Acá definís la matriz de covarianzas que *ya conocés*. O sea, como si el profe te la hubiera dado.
mu_0=c(); mu_0  # Este es el vector con los valores hipotéticos de las medias. Lo que uno "cree" que deberían ser.
vm=colMeans(base); vm  # Calculamos el promedio de cada variable en tu base de datos.

n=nrow(base); n  # Cantidad de observaciones (individuos/filas) en la base.
X_0=n*t((vm-mu_0))%*%solve(Sigma)%*%(vm-mu_0); X_0  # Estadístico Hotelling T² para contrastar si las medias son iguales a las hipotéticas.
gl=ncol(base)  # Grados de libertad: la cantidad de variables.
alpha=#nivel de significancia
X2=qchisq(1-alpha,gl); X2  # Valor crítico de la chi-cuadrado con gl grados de libertad. Lo usamos para comparar con X_0.

# 💡 TIP: Si X_0 > X2 → rechazás H0. O sea, tus medias no son lo que esperabas.

# ------------------------
# PRUEBAS POR SEPARADO (una por variable)
# ------------------------

z1=(vm[1]-mu_0[1])/sqrt(Sigma[1,1]/n); z1  # Z para la primera variable
z2=(vm[2]-mu_0[2])/sqrt(Sigma[2,2]/n); z2  # Z para la segunda variable

alpha=
z=qnorm(alpha/2); z  # Valor crítico de la normal estándar (para pruebas bilaterales).

# 💡 TIP: Acá es como hacer pruebas Z independientes. Si querés ver cada variable por su cuenta.

# ------------------------
# MATRIZ DE COVARIANZAS DESCONOCIDA (Hotelling con Sigma estimada)
# ------------------------

library(MVTests)
OneSampleHT2(base, mu0 = c())  # Prueba Hotelling T² con matriz de covarianza *desconocida* (estimada a partir de los datos).

n=nrow(base); n  # Número de observaciones
p=ncol(base); p  # Número de variables
alpha=#nivel de significancia
f=qf(alpha,p,n-p,lower.tail = FALSE); f  # Valor crítico de la distribución F
vc=((n-1)*p/(n-p))*f; vc  # Valor crítico transformado para Hotelling T²

vm=colMeans(base); vm  # Vector de medias
S=cov(base); S  # Matriz de covarianza muestral
vecpr=eigen(S); vecpr  # Descomposición espectral de S
valp=vecpr$values; valp  # Valores propios (varianzas de las combinaciones lineales)
vetp=vecpr$vectors; vetp  # Vectores propios (direcciones principales)

mu_0=c()
n=nrow(microondas); n

n*t(vm-mu_0)%*%solve(S)%*%(vm-mu_0)  # Estadístico T²
p=ncol(microondas); p
f=qf(0.05,p,n-p,lower.tail = FALSE); f
vc=f*p*(n-1)/(n-p); vc  # Valor crítico

# 💡 TIP: Si el estadístico T² supera ese valor crítico → ¡sorpresa! Tus medias son sospechosamente distintas.

# Ejes de la elipse de confianza
sqrt(valp[1])*sqrt(((p*(n-1))/(n*(n-p))*f))  # Longitud del primer eje
sqrt(valp[2])*sqrt(((p*(n-1))/(n*(n-p))*f))  # Longitud del segundo eje

vetp  # Direcciones de los ejes

# 💡 TIP: Estos ejes te dicen hacia dónde se estira la elipse de confianza. O sea, la forma de la incertidumbre 👀.

# ------------------------
# INTERVALOS DE CONFIANZA SIMULTÁNEOS
# ------------------------

alpha=0.05
vm[1]-sqrt((p*(n-1)/(n-p))*f)*sqrt(S[1,1]/n)  # Límite inferior para la variable 1
vm[1]+sqrt((p*(n-1)/(n-p))*f)*sqrt(S[1,1]/n)  # Límite superior para la variable 1

vm[2]-sqrt((p*(n-1)/(n-p))*f)*sqrt(S[2,2]/n)  # Límite inferior para la variable 2
vm[2]+sqrt((p*(n-1)/(n-p))*f)*sqrt(S[2,2]/n)  # Límite superior para la variable 2

# 💡 TIP: Con estos intervalos podés decir "con 95% de confianza, la media está entre estos dos valores".

# ------------------------
# INTERVALOS PARA MUESTRAS GRANDES
# ------------------------

vm=c(); vm
S=c(); S  # Desviaciones estándar de cada variable
alpha=0.10
p=length(vm); p

IS=function(vm,S,alpha){
  li=c()
  ls=c()
  n=96
  p=length(vm)
  q=qchisq(alpha,p,lower.tail = FALSE)
  for(i in 1:p){
    li[i]=vm[i]-sqrt(q)*S[i]/sqrt(n)
    ls[i]=vm[i]+sqrt(q)*S[i]/sqrt(n)
  }
  print(li);
  print(ls)
}
IS(vm=vm,S=S,alpha=alpha)  # Función para calcular intervalos de confianza para cada variable

# 💡 TIP: Cuando tenés alta n (n grande), podés usar esta fórmula que es más simple y sirve re bien. Ideal para cerrar con broche de oro.
