# PCA
# ====

df <- USArrests
head(df)

# Matriz de covarianza
S <- cov(df)
S

# Autovalores de la matriz de covarianza
s_auto <- eigen(S)
s_auto

# Proporción de la varianza total explicada por cada componente (autovalores)
for (s in s_auto$values) {
  print(s / sum(s_auto$values))
}

# Visualización de los autovalores
plot(s_auto$values, xlab = 'Número de autovalor', ylab = 'Tamaño de autovalor',
     main = 'Scree Plot')
lines(s_auto$values)


# ---------------------------------------------------
# Usando funciones de R
# ---------------------------------------------------

#library(tidyverse)

#results <- prcomp(df, scale = TRUE)
res <- prcomp(df)
res

# R usa direcciones negativas de autovectores: se debe multipicar por -1
res$rotation <- -1*res$rotation
res$rotation

# Resumen
summary(res)

# Componentes para cada fila
res$x <- -1*res$x
head(res$x)

biplot(res, scale = 0)

v <- res$rotation
vred = v[,1:2]
vred
t(t(vred) %*% t(df))
