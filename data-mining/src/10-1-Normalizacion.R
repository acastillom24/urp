#===============================================================================
#  NORMALIZACIÓN
#===============================================================================

# Ejemplo: Base de datos sobre créditos de un banco alemán                          
#---------------------------------------------------------

# install.packages('Fahrmeir')
library(Fahrmeir)
# Datos
help(credit)
df <- credit
head(df)
# Histograma del valor prestado
hist(df$DM)


# Normalización usando puntaje z
# """"""""""""""""""""""""""""""
# Alternativa 1:
df$DMz <- scale(x=df$DM)
# Alternativa 2:
# install.packages('reshape')
# library(reshape)
# df$DMz <- rescaler(x=df$DM, type="sd")


# Normalización usando criterio Min-Max
# """""""""""""""""""""""""""""""""""""
rescale <- function(x, vmin, vmax){
  xmin <- min(x)
  xmax <- max(x)
  y <- ((x-xmin)*(vmax-vmin))/(xmax-xmin) + vmin
  return(y)
}

min_val <- 0
max_val <- 1
df$DMmm <- rescale(df$DM, min_val, max_val)


# Transformación por escalamiento decimal
# """""""""""""""""""""""""""""""""""""""
maxval <- max(abs(df$DM))
k <- ceiling(log10(maxval))
df$DMed <- credit$DM/10^k


# Transformación sigmoidal
# """"""""""""""""""""""""
curve((1-exp(-x))/(1+exp(-x)), from=-6, to=6, xlab='z')
curve(1/(1+exp(-x)), from=-6, to=6, xlab='z')

# Ejemplo:
x <- seq(5, 15, 0.01)
mx <- mean(x); sx <- sd(x)
z  <- (x-mx)/sx
y  <- (1 - exp(-z))/(1 + exp(-z))
plot(x, y)
# Líneas a +- 1 desviación estándar
abline(v=mx-sx, col='red'); abline(v=mx+sx, col='red')
# Media
abline(v=mx, col='green', lty=2)

# Con los datos del "data frame"
z <- scale(x=df$DM)
df$DMsig <- (1-exp(-z))/(1+exp(-z))

# Efecto de la normalización usando plots
par(mfrow=c(1,2))
plot(sort(df$DM))
plot(sort(df$DMsig))
par(mfrow=c(1,1))

# Usando función logística
#credit$softDM<-DMwR::SoftMax(credit$DM,lambda=2*pi)
z = scale(x=df$DM)
df$DMsigl = 1/(1+exp(-z))

summary(df)

#Gráfico de comparación
boxplot(df[,c(9,10,11,12,13)])
