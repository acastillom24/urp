# ==============================================================================
#     MEDIDAS DE TENDENCIA CENTRAL
# ==============================================================================

datos <- c(0, 1, 2, 12, 12, 14, 18, 21, 21, 23, 24, 25,
           28, 29, 30, 30, 30, 33, 36, 44, 45, 47, 51)

# Media
# """""
mean(datos)

# Forma 1: creando una función
media <- function(datos){
  suma = 0
  for(dato in datos){
    suma = suma+dato
  }
  return(suma/length(datos))
}
media(datos)

# Forma 2
sum(datos)/length(datos)

# Ejemplo con data frame
df <- read.csv('resultados.txt', header=TRUE, sep="\t")
mean(df$algeb1)
mean(df$algeb1, na.rm=TRUE)   # Ignora valores NA

sapply(df, mean, na.rm=TRUE)
sapply(df[2:5], mean, na.rm=TRUE)

# Mediana
# """""""
median(datos)
sapply(df[2:5], median, na.rm=TRUE)

# Moda
# """"
# install.packages('modeest')
library(modeest) 
mfv(datos)

# Otras medidas
# """""""""""""
# install.packages("e1071")
library(e1071)

# Asimetría
skewness(cars$dist)     # Ver hist(cars$dist)
sk <- 1/length(cars$dist)*sum((cars$dist-mean(cars$dist))^3)/sd(cars$dist)^3
sk

# Curtosis
kurtosis(cars$dist)
kr <- 1/length(cars$dist)*sum((cars$dist-mean(cars$dist))^4)/sd(cars$dist)^4 - 3
kr


# ==============================================================================
#    MEDIDAS DE DISPERSIÓN
# ==============================================================================

# Rango
# """""
rango <- max(datos) - min(datos)
rango

rango <- max(df$algeb1, na.rm=T) - min(df$algeb1, na.rm=T)
rango

range(df$algeb1, na.rm=T)    # Brinda el mínimo y el máximo

sapply(df[2:5], range, na.rm=T)

# Varianza
# """"""""
var(datos)

# Desviación estándar (para la muestra)
# """""""""""""""""""
sd(datos)
# Usando la fórmula
sqrt(1/(length(datos)-1)*sum((datos-mean(datos))^2))
# Para varias columnas
sapply(df[2:5], sd, na.rm=T)

# Covarianza
# """"""""""
cov(df$algeb1, df$algeb2, use="complete.obs")
# Matriz de covarianza 
cov(df[2:5], use="complete.obs")    # Podría haber diferencias por NA

# Alternativa: primero eliminar todos los NA
df2 <- na.omit(df)
cov(df2$algeb1, df2$algeb2)
cov(df2[2:5])


# Cuantiles
# """""""""
quantile(datos)            # Muestra los cuartiles

# Deciles 
# """""""
deciles <- seq(0, 1, 0.1)
quantile(datos, deciles)

# Percentiles
# """""""""""
percentiles <- seq(0, 1, 0.01)
quantile(datos, percentiles)

# Rango intercuartílico
# """""""""""""""""""""
IQR(datos)
quantile(datos, 0.75) - quantile(datos, 0.25)


# ==============================================================================
# Resumen estadístico
# ==============================================================================

summary(datos)

summary(df)

# Convertir genero a "factor"
df$genero <- factor(df$genero)
summary(df)
