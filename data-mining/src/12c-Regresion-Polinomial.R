# ==============================================================================
# Ejemplo: Regresión Polinomial
# ==============================================================================

library(ggplot2)
library(ggpubr)
set.seed(2)

# Simular los datos
# ========================================

x <- seq(-2, 4, 0.1)
# Valores real de "y"
true_y <- x^2
# Valor de "y" con ruido (error)
error <- rnorm(length(x), mean=0, sd=2)     # Valores de la distribución normal
y <- true_y + error

# Conversión a dataframe (x,y serán columnas)
data <- data.frame(x,y)

# Gráfico de los puntos con error
p00 <- ggplot(data=data, aes(x=x, y=y)) + geom_point(size=0.7) + 
  ggtitle("Data") + theme_minimal()
p00

# Gráfico de la verdadera relación
p0 <- ggplot(data=data, aes(x=x, y=y)) + geom_point(size=0.7)+
   geom_line(aes(x=x, y=true_y)) + ggtitle("Verdadera relación") +
   theme_minimal()
p0


# Ajuste de diferentes modelos paramétricos
# ==============================================
p1 <- ggplot(data=data, aes(x=x, y=y)) + geom_point(size=0.7) + 
  geom_line(aes(x=x, y=predict(lm(y~x))), col='red') + ggtitle('poly1') + 
  theme_minimal()

p2 <- ggplot(data=data, aes(x=x, y=y)) + geom_point(size=0.7) + 
  geom_line(aes(x=x, y=predict(lm(y~poly(x,2)))), col='orange') + 
  ggtitle('poly2') + theme_minimal()

p3 <- ggplot(data=data, aes(x=x, y=y)) + geom_point(size=0.7) + 
  geom_line(aes(x=x, y=predict(lm(y~poly(x,10)))), col='pink') + 
  ggtitle('poly10') + theme_minimal()

p4 <- ggplot(data=data, aes(x=x, y=y)) + geom_point(size=0.7) + 
  geom_line(aes(x=x, y=predict(lm(y~poly(x,20)))), col='purple') + 
  ggtitle('poly20') + theme_minimal()

ggarrange(p1, p2, p3, p4)


# ===========================================
# Repitiendo 100 veces la generación de datos (x es fijo, pero con errores diferentes)
# ===========================================
set.seed(2)

M = 100        # Número de repeticiones
nord = 20      # Ajustar polinomios hasta este orden
x = seq(-2, 4, 0.1)   # x entre -2 y 4

# Función que describe el verdadero "y"
func_real <- function(x) return(x^2)
# Verdadero "y" (sin ruido)
y_real <- func_real(x)

# Vector de error normal diferente para cada "muestra" (hasta M)
error <- matrix(rnorm(length(x)*M, mean=0, sd=2), nrow=M, byrow=TRUE)
# Valores de y con error diferente para cada muestra (y_real + error)
y <- matrix(rep(y_real, M), nrow=M, byrow=TRUE) + error

# Alocación de memoria para las predicciones
preds <- array(NA, dim=c(M, length(x), nord))
# Predicciones
for (i in 1:M) {
  for (j in 1:nord) {
    # Predicciones (con 20 polinomios) para cada valor de cada una de las M
    # muestras aleatorias de "y"
    preds[i,,j] <- predict(lm(y[i,]~poly(x, j, raw=TRUE)))
  }
}

# Data frame que contendrá x, rep, poly1 ... poly20, donde cada columna contiene
# las M*61 predicciones (con las M muestras diferentes)
stackmat <- NULL
for (i in 1:M)
  stackmat <- rbind(stackmat, cbind(x,rep(i,length(x)), preds[i,,]))
colnames(stackmat) <- c("x", "rep", paste("poly", 1:20, sep=''))
sdf <- as.data.frame(stackmat)

# Rango de y (útil para graficar)
yrange = range(apply(sdf, 2, range)[,3:22])

# Gráfico de las predicciones considerando los M modelos en polinomios de grado
# 1 al 20
p1 <- ggplot(data=sdf, aes(x=x, y=poly1, group=rep, colour=rep)) +
  scale_y_continuous(limits=yrange) + geom_line() + theme_minimal() + 
  theme(legend.position = 'none') 
p1 <- p1 + stat_function(fun=func_real, lwd=1.3, colour="black") + 
  ggtitle("poly1")

p2 <- ggplot(data=sdf, aes(x=x, y=poly2, group=rep, colour=rep)) +
  scale_y_continuous(limits=yrange) + geom_line() + theme_minimal() + 
  theme(legend.position = 'none')
p2 <- p2 + stat_function(fun=func_real, lwd=1.3, colour="black") + 
  ggtitle("poly2")

p10 <- ggplot(data=sdf, aes(x=x, y=poly10, group=rep, colour=rep)) +
  scale_y_continuous(limits=yrange) + geom_line() + theme_minimal() + 
  theme(legend.position = 'none')
p10 <- p10 + stat_function(fun=func_real, lwd=1.3, colour="black") + 
  ggtitle("poly10")

p20 <- ggplot(data=sdf, aes(x=x, y=poly20, group=rep, colour=rep)) +
  scale_y_continuous(limits=yrange) + geom_line() + theme_minimal() + 
  theme(legend.position = 'none')
p20 <- p20 + stat_function(fun=func_real, lwd=1.3, colour="black") + 
  ggtitle("poly20")

ggarrange(p1, p2, p10, p20)
