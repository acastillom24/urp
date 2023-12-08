# Remuestreo de Datos
# -------------------

#===============================================================================
# CONJUNTO DE VALIDACIÓN
#===============================================================================

# install.packages('ISLR')
library(ISLR)
# Se desea predecir mpg usando horsepower
head(Auto)
n <- dim(Auto)[1]    # Número de filas (datos)

set.seed(123)
MSEval <- matrix(nrow=10, ncol=11)
# Se repetirá 11 veces el muestreo
for(muestra in 1:11)
{
  id = sample(n, n/2) # muestrea n/2 de n (índices) para el "train" set
  # Se usará polinomios de grado 1 hasta 10
  for (grado in 1:10)
  {
    # Entrenar en el conjunto de entrenamiento
    modelo = lm(mpg ~ poly(horsepower,grado), data=Auto, subset=id)
    # Predecir y calcular MSE en conjunto de validación ("test")
    pred = predict(modelo, Auto)[-id]
    MSEval[grado, muestra] = mean((Auto$mpg[-id]-pred)^2)
  }
}


# Resultado para la primera muestra
df <- data.frame("ValMSE"=MSEval[,1], "grado"=1:10)

library(ggplot2)
g0 <- ggplot(df, aes(x=grado)) + geom_line(y=MSEval[,1]) + 
     scale_y_continuous(limits=c(15,28)) + scale_x_continuous(breaks=1:10) + 
     labs(y="MSE del conjunto de Validación")
g0 + theme_minimal()


# Comparando para las demás muestras
cols <- rainbow(10)
g1 <- g0 + geom_line(aes(x=1:10, y=MSEval[,2]), colour=cols[1])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,3]), colour=cols[2])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,4]), colour=cols[3])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,5]), colour=cols[4])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,6]), colour=cols[5])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,7]), colour=cols[6])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,8]), colour=cols[7])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,9]), colour=cols[8])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,10]), colour=cols[9])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEval[,11]), colour=cols[10])
g1 <- g1 + theme_minimal()
g1


#===============================================================================
# VALIDACIÓN CRUZADA DEJANDO UNO AFUERA (LOOCV)
#===============================================================================

library(boot)   # Para cv.glm
set.seed(123)

MSEloocv <- NULL
for(grado in 1:10)
{
  modelo <- glm(mpg ~ poly(horsepower,grado), data=Auto)
  # k es el número de grupos que se generará (n grupos, en este caso)
  loocv <- cv.glm(Auto, modelo, K=n)
  MSEloocv <- c(MSEloocv, loocv$delta[1])
}

df <- data.frame("loocvMSE"=MSEloocv, "grado"=1:10)
g0 <- ggplot(df, aes(x=grado, y=MSEloocv)) + geom_line() + geom_point() + 
     scale_y_continuous(limits=c(15,28)) + scale_x_continuous(breaks=1:10) + 
     labs(y="MSE usando LOOCV")
g0 + theme_minimal()


#===============================================================================
#   VALIDACIÓN CRUZADA (5-CV y 10-CV)
#===============================================================================

set.seed(123)

MSE_5cv <- NULL
MSE_10cv <- NULL

# Usar polinomios de grado 1 a 10
for(grado in 1:10)
{
  modelo <- glm(mpg~poly(horsepower, grado), data=Auto)
  cv5  <- cv.glm(Auto, modelo, K=5)
  cv10 <- cv.glm(Auto, modelo, K=10)
  MSE_5cv  <- c(MSE_5cv,  cv5$delta[1])
  MSE_10cv <- c(MSE_10cv, cv10$delta[1])
}

df <- data.frame("MSEcv5"=MSE_5cv, "grado"=1:10)

g0 <- ggplot(df, aes(x=grado, y=MSE_5cv)) + geom_line() + geom_point() + 
      scale_y_continuous(limits=c(15,28)) + scale_x_continuous(breaks=1:10) +
      labs(y="MSE usando CV") + ggtitle("CV con K=5 y K=10")
g0 + geom_line(aes(y=MSE_10cv), colour="red") + 
     geom_point(aes(y=MSE_10cv), colour="red") + 
     ggtitle("K=5 (negro), K=10 (rojo)") + theme_minimal()


# ----------------
# Repetir 11 veces la validación cruzada con k=10 (10-CV)
# ----------------
# (para comparar la variabilidad con LOOCV)

set.seed(123)
MSEcv <- matrix(nrow=10, ncol=11)

for(grado in 1:10)
{
  modelo <- glm(mpg~poly(horsepower, grado), data=Auto)
  for(muestra in 1:11)
  {
    cv10 <- cv.glm(Auto, modelo, K=10)
    MSEcv[grado, muestra] = cv10$delta[1]
  }
}

df <- data.frame("MSE_cv"=MSEcv[,1], "grado"=1:10)
g0 <- ggplot(df, aes(x=grado)) + geom_line(y=MSEcv[,1]) + 
     scale_y_continuous(limits=c(15,28)) + scale_x_continuous(breaks=1:10) +
     labs(y="MSE en Validación Cruzada con K=10")
g0

cols <- rainbow(10)
g1 <- g0 + geom_line(aes(x=1:10, y=MSEcv[,2]), colour=cols[1])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,3]), colour=cols[2])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,4]), colour=cols[3])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,5]), colour=cols[4])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,6]), colour=cols[5])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,7]), colour=cols[6])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,8]), colour=cols[7])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,9]), colour=cols[8])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,10]), colour=cols[9])
g1 <- g1 + geom_line(aes(x=1:10, y=MSEcv[,11]), colour=cols[10])
g1 + theme_minimal() + 
  ggtitle("11 particiones distintas de 10-CV (evaluar variabilidad)")

