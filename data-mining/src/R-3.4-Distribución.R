# ==============================================================================
#   FUNCIONES DE DENSIDAD DE PROBABILIDAD Y DE DISTRIBUCIÓN ACUMULADA
# ==============================================================================


# ==============================================================================
# Distribución Uniforme
# ==============================================================================

# Función de densidad de probabilidad (pdf): dunif
curve(dunif(x, 2, 5), xlim = c(0, 7))
# Función de distribución acumulada (cdf): punif
curve(punif(x, 2, 5), xlim = c(0, 7))

# Usando una función de densidad entre 0 y 1
curve(dunif(x, 0, 1), xlim = c(-0.5, 1.5))
# Sombrear valores entre 0.2 y 0.5
x <- seq(0.2, 0.5, 0.001)
lines (x, dunif(x), type = "h", col = "grey")
text(0.35, 0.8, "P(0.2 < X < 0.5)" )

# Probabilidad P(0.2<X<0.5) = F(X<=0.5) - F(X<=0.2)
# punif: función de distribución acumulada
punif(0.5, 0, 1) - punif(0.2, 0, 1)

# Probabilidad P(X>0.9) = 1 - F(X>=0.9)
1 - punif(0.9, 0, 1)

# Se desea el valor de k tal que: P(X<=k) = 0.95
# qunif: cuantil para la distribución uniforme
k <- qunif(0.95, 0, 1)
k


# ==============================================================================
# Distribución Normal
# ==============================================================================

# Ejemplo: N(media=2, desv=0.5) en el rango 0 a 3
# Función de densidad
curve(dnorm(x, 2.0, 0.5), from=0, to=4, ylab='f(x)')
abline(v=2.0, col='red', lty=2)
# Función de distribución acumulada
curve(pnorm(x, 2.0, 0.5), from=0, to=4, ylab='F(x)')
abline(v=2.0, col='red', lty=2)

# Ejemplo: 2 curvas normales con diferente media
plot(NA, xlab = "Altura [m]", xlim = c(0.5, 3), ylab = "f(x)", ylim = c(0, 1.6))
curve(dnorm(x, 1.65, 0.25), add = TRUE, col="red")
curve(dnorm(x, 1.85, 0.25), add = TRUE, col="blue")
text(1.15, 0.5, expression({mu}~"=1.65"), col="red")
text(2.35, 0.5, expression({mu}~"=1.85"), col="blue")

# Ejemplo: 2 curvas normales con diferente desviación estándar
plot(NA, xlab = "Diametro [cm]", xlim= c(4,16), ylab = "f(x)", ylim = c(0, 0.4))
curve(dnorm(x, 10, 1), add = TRUE, col="red")
curve(dnorm(x, 10, 2), add = TRUE, col="blue")
text(8.5, 0.3, expression({sigma}~"= 1 cm"), col="red")
text(14, 0.07, expression({sigma}~"= 2 cm"), col="blue")

# Ejemplo 3: Probabilidad de 40<=X<=50, con X~N(45,4)
curve(dnorm(x, 45, 4), 30, 60, xlab = "x", ylab = "f(x)")
x <- seq(40, 50, 0.01)
lines (x, dnorm(x, mean = 45, sd = 4), type = "h", col = "grey")

prob <- pnorm(50, 45, 4) - pnorm(40, 45, 4)
prob

# Ejemplo 4: valor de k tal que P(X<=k)=0.95, con X~N(0,1)
qnorm(0.95)
# Verificación
pnorm(1.6446)


# ==============================================================================
# Distribución chi-cuadrado
# ==============================================================================

# Función de densidad de probabilidad
curve(dchisq(x, 2), xlim=c(0,15))
curve(dchisq(x, 3), xlim=c(0,15), add=T, col="red")
curve(dchisq(x, 4), xlim=c(0,15), add=T, col="blue")
curve(dchisq(x, 5), xlim=c(0,15), add=T, col="green")
curve(dchisq(x, 6), xlim=c(0,15), add=T, col="magenta")

# Ejemplos de cálculo de probabilidad usando la función de distribución acumulada
pchisq(6, 10)
1 - pchisq(126, 150)
pchisq(50, 65) - pchisq(40, 65) 
qchisq(0.6, 100)


