#===============================================================================
#  ANÁLISIS MULTIVARIADO DE VALORES ATÍPICOS (OUTLIERS)
#===============================================================================


#===============================================================================
#  EJEMPLO 1: OUTLIERS EN 2 DIMENSIONES
#===============================================================================

# Cargar "df2"
load(file="EjOutliers.rdata")

# Inspección de "df2"
head(df2)
tail(df2)
summary(df2)

par(mfrow=c(1,3))
# Diagrama de dispersión (bidimensional)
plot(df2, xlim=c(-5,5), ylim=c(-5,5))
# Diagramas de caja (unidimensionales)
boxplot(df2[,1], main="Valores de x")
boxplot(df2[,2], main="Valores de y")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
plot(df2, xlim=c(-5,5), ylim=c(-5,5))
# Gráfico de barras (unidimensional)
barplot(df2[,1], main="Valores de x")
barplot(df2[,2], main="Valores de y")
par(mfrow=c(1,1))

# Distancia de Mahalanobis
mu <- colMeans(df2)    # Medias de cada columna
S <- cov(df2)          # Matriz de covarianza
dm <- sqrt(apply(df2, 1, function(x) t(x-mu) %*% solve(S) %*% (x-mu)))

# Distancia de Mahalanobis cuadrada
d2 <- dm^2                         # Calculando manualmente
dm2 <- mahalanobis(df2, mu, S)    # Cálculo directo
barplot(dm2, main="Mahalanobis")  # Gráfico de barras de dist Mahalanobis^2

# Valores máximos
idx_max <- which.max(dm2)         # Índice del valor máximo 
order(dm2, decreasing=TRUE)       # Índices ordenados
dm2[order(dm2, decreasing=TRUE)]  # Valores ordenados

plot(df2, xlim=c(-5,5), ylim=c(-5,5))
identify(df2[,1], df2[,2], round(dm2,1))    # Identifica puntos en scatter plot

# Distribución Chi-Cuadrado: Punto de Corte 
p <- 1-0.025
dof <- ncol(df2)                # Num variables = num grados de libertad
k <- qchisq(p, dof)             # k: valor umbral (de corte)
idx_outliers <- which(dm2>k)    # Índices de los valores atípicos
idx_outliers

# Gráfica de ojiva (probabilidad acumulada)
# """"""""""""""""
plot(sort(dm2), ppoints(nrow(df2)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v=qchisq(p,dof), col="red")              # Recta que indica el cuantil
abline(v=qchisq(1-0.01,dof), col="green")

# Gráfica cuantil-cuantil (QQ-plot)
# """""""""""""""""""""""
x <- qchisq(ppoints(nrow(df2)), dof)   # valores de chi cuadrado teóricos
y <- dm2                               # distancia de mahalanobis cuadrada
qqplot(x, y, main=expression("Q-Q plot para" ~ {chi^2}), 
       xlab=expression("Cuantiles"~{chi^2}), ylab="Dist Mahalanobis cuadrada")
abline(a=0, b=1, col="red")

# Gráfica de dispersión resaltando el outlier (en rojo)
# """""""""""""""""""""""""""""""""""""""""""
par(mfrow = c(1,1))
plot(df2, xlim=c(-5,5), ylim=c(-5,5))
points(df2[idx_max,1], df2[idx_max,2], col="red")

# Exclusión de outliers
# """""""""""""""""""""
df2[idx_outliers, ]
df2_clean <- df2[-idx_outliers,]

# Ojiva luego de excluir los outliers
dm2 <- mahalanobis(df2_clean, colMeans(df2_clean), cov(df2_clean))
plot(sort(dm2), ppoints(nrow(df2_clean)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v=qchisq(p,dof), col="red")


#===============================================================================
#  EJEMPLO 2: OUTLIERS EN 3 DIMENSIONES
#===============================================================================

# Datos que provienen de distribución normal multivariada de dimensión 3
load(file = "EjOutliers3D.rdata")
head(df3)

# Matriz de gráficas de dispersión
pairs(df3)

# Introducción de outliers
outFactor <- 1.5
df3 <- rbind(df3, outFactor*c(-1, -1.2, 0.7))

# Gráficas de dispersión incluyendo el outlier
pairs(df3)
pairs(df3, col=c(rep(1,300), 2), pch=c(rep(1,300), 3), cex=c(rep(1,300), 2))

# Mahalanobis al cuadrado
dm2 <- mahalanobis(df3, colMeans(df3), cov(df3))
barplot(dm2, main="Mahalanobis")
which.max(dm2)

# Distribución Chi-Cuadrado: Punto de Corte 
p <- 1-0.025
dof <- ncol(df3)
k <- (qchisq(p, dof))
idx_outliers <- which(dm2 > k)
idx_outliers

# Gráfica de ojiva
plot(sort(dm2), ppoints(nrow(df3)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")
abline(v = qchisq(1-0.01,dof), col = "green")

# Gráfica cuantil-cuantil (QQ-plot)
x <- qchisq(ppoints(nrow(df3)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot para" ~ {chi^2}[nu==3]), 
       xlab=expression("Cuantiles"~{chi^2}), ylab="Dist Mahalanobis cuadrada")
abline(a=0, b=1, col="red")

# Gráfico 3D de los datos
# install.packages('rgl')
library(rgl)
plot3d(df3, col=c(rep(1,300), 2))

# Outliers
df3[idx_outliers, ]
# Exclusión de los outliers
df3_clean <- df3[-idx_outliers, ]

# Verificación excluyendo los outliers
dm2 <- mahalanobis(df3_clean, colMeans(df3_clean), cov(df3_clean))
plot(sort(dm2), ppoints(nrow(df3_clean)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p, dof), col = "red")


#===============================================================================
#  EJEMPLO 3: OUTLIERS EN "EMPLEADOS DE UNA EMPRESA" (Banco en 1965)
#===============================================================================

dfe <- read.delim("DatosEmpresa.dat" )
# Asignación de etiquetas
dfe$GENERO <- as.factor(dfe$GENERO);
levels(dfe$GENERO) <- c("Hombre", "Mujer", NA)
dfe$MINORIA <- as.factor(dfe$MINORIA);
levels(dfe$MINORIA) <- c("Blanco", "No blanco", NA)
dfe$CATTRAB <- as.factor(dfe$CATTRAB)
levels(dfe$CATTRAB)<- c(NA, "Oficinista", "Asistente", "Seguridad", 
                       "Académico", "Empleado", "Ejecutivo", "Técnico")
# Conversión de valores perdidos a NA (por defecto están como 0)
dfe$SALINIT[dfe$SALINIT == 0] <- NA
dfe$TTRABAJO[dfe$TTRABAJO == 0] <- NA
dfe$EDAD[dfe$EDAD == 0] <- NA
dfe$SALACTUAL[dfe$SALACTUAL == 0] <- NA
dfe$EXPACAD[dfe$EXPACAD == 0] <- NA
dfe$EXPTRAB[dfe$EXPTRAB == 0] <- NA
# Por "simplicidad" se omitirá los valores perdidos
dfe <- na.omit(dfe)
summary(dfe)


# Selección de algunas columnas de interés
df <- dfe[c(2,4,5,6,7,8)]

# Distancia de Mahalanobis
dm2 <- mahalanobis(df, colMeans(df), cov(df))
barplot(dm2, main="Mahalanobis")
which.max(dm2)

# Distribución Chi-Cuadrado: Punto de Corte 
p <- 1-0.001
dof = ncol(df)
k <- (qchisq(p, dof))
idx_outliers <- which(dm2 > k)
idx_outliers
df[idx_outliers,]         # Registros con valores atípicos

# Gráfico de Ojiva
plot(sort(dm2), ppoints(nrow(df)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")

# QQ-plot:
x <- qchisq(ppoints(nrow(df)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot para"~~{chi^2}[nu==6]))
abline(0, 1, col="red")


# Exclusión de valores atípicos (un valor atípico)
idx_excluido <- which.max(dm2)    
df[idx_excluido, ]
df_clean <- df[-idx_excluido, ]

# Distancia de Mahalanobis
dm2 <- mahalanobis(df_clean, colMeans(df_clean), cov(df_clean))
plot(sort(dm2), ppoints(nrow(df_clean)), xlab="DM al cuadrado ordenada",
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")

idx_outliers <- which(dm2 > k)
idx_outliers


# Uso de estimador robusto: MCD (determinante de covarianza mínima)
# """""""""""""""""""""""""""""

library(MASS)
# Porcentaje de valores que utilizará MCD
q = 0.85
# Calcula media y covarianza robusta usando MCD
mcd <- cov.mcd(df, quantile.used=nrow(df)*q)
dm2 <- mahalanobis(df, mcd$center, mcd$cov)

# Alternativa: librería robustbase (más rápido que cov.mcd)
# library(robustbase)
# mcd2 <- covMcd(df, alpha=q)

# Distribución Chi-Cuadrado: Punto de Corte 
p <- 1-0.001
k <- (qchisq(p, dof))
idx_outliers <- which(dm2 > k)
idx_outliers

# Ojiva
plot(sort(dm2), ppoints(nrow(df)), xlab="DM al cuadrado ordenada",
     ylab="Probabilidad Acumulada")
abline(v = qchisq(p,dof), col = "red")

# QQ-plot
x <- qchisq(ppoints(nrow(df)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot for"~~{chi^2}[nu==6]))
abline(0, 1, col="red")

# Excluyendo los outliers
df[idx_outliers, ]
df_clean_mcd <- df[-idx_outliers, ]


#-----------------------------------------------------------------------------
# Ejemplo: 
# Filzmoser (2005), Multivariate outlier detection in exploration geochemistry
#-----------------------------------------------------------------------------

# install.packages("mvoutlier")
library(mvoutlier)
aq.plot(x=df, delta=qchisq(p,dof), quan=0.85)

# Cambios al eliminar datos
chisq.plot(df)

# Gráfica univariada
uni.plot(df, symb=TRUE)



# =====================================================
#  OTROS MÉTODOS
# =====================================================

# Densidad Local
# """"""""""""""
library(DMwR2)
lof <- lofactor(df, 10)     # 10: valor de la vecindad a considerar
lof
# Ordenamiento decreciente y tomar los 10 primeros
df[order(lof, decreasing=TRUE)[1:10], ]


# Clusters (PAM)
# """"""""""""""
library(cluster)
pamDf = pam(df, 25, stand=TRUE)     # 25: número de clústers
pamDf$clusinfo                      # Ver qué clúster tiene menos elementos
df[pamDf$clustering==19,]



# Método PCOut (robust principal components)
# """"""""""""
# De Filzmoser et al. (2008), Outlier identification in high dimensions
outlier = pcout(df, makeplot = TRUE)
outlier
df[order(outlier$wfinal,decreasing=F)[1:10],]

# Método de signo
outlier1=sign2(df)
outlier1
df[order(outlier1$x.dist,decreasing=T)[1:10],]

