
# Lectura de los datos
library(foreign)
golf <- read.dta(file="golffull.dta")
head(golf)


#===============================================================================
# Selección de Variables                                                 
#===============================================================================

# Analizando el p-valor
form <- price ~ kilometerop1 + kilometerop2 + kilometerop3 + ageop1 + ageop2 +
                ageop3 + extras1 + extras2
modelo1 <- lm(formula=form, data=golf)
# Los valores *** (menor Pr(>|t|)) indican las variables más significativas
summary(modelo1)


# M1: Algoritmo "leaps and bounds" (cálculo de todos los subconjuntos de modelos)
# ---------------------------------

# install.packages("bestglm")
library(bestglm)

# Se incluye las variables independientes (4 a 12) y al final la dependiente (1)
# Se utiliza el indicador AIC y la familia gaussiana (para el modelo RLN)
mselec <- bestglm(Xy = golf[,c(4:12, 1)], IC="AIC", family = gaussian)
modelo2 <- mselec$BestModel
summary(modelo2)


# M2: Selección Paso a Paso (Stepwise)
# -------------------------
library(MASS)
# Se pasa el modelo completo (con todas las variables) al algoritmo
form = price ~ kilometerop1 + kilometerop2 + kilometerop3 +
              ageop1 + ageop2 + ageop3 + extras1 + extras2 + TIA
# Se calcula el modelo lineal
modelofull <- lm(formula = form, data=golf)
# Se selecciona variables
mselec <- stepAIC(modelofull, trace=TRUE)

# Modelo obtenido
form <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2
modelo <- lm(formula=form, data=golf)
summary(modelo)

# Estimación de la varianza
sum(modelo$residuals^2)/(172-5)



#===============================================================================
# PREDICCIÓN
#===============================================================================

# Nuevo dato
x0 <- data.frame(kilometer = 30, age=100)
# Creación de atributos derivados
x0$kilometerop1 <- (x0$kilometer - mean(golf$kilometer))/sd(golf$kilometer)
x0$kilometerop2 <- ((x0$kilometer)^2 - mean((golf$kilometer)^2))/sd((golf$kilometer)^2)
x0$ageop1 <- (x0$age - mean(golf$age))/sd(golf$age)
x0$ageop2 <- ((x0$age)^2 - mean((golf$age)^2))/sd((golf$age)^2)

# Predicción puntual
predict(modelo, newdata=x0, type="response") 

# Intervalo de Confianza para la media 
predict(modelo, newdata=x0, interval="confidence")

# Intervalo de Predicción
predict(modelo, newdata = x0, interval="prediction")

library(ggplot2)
golf$predictions <- predict(modelo)
# Gráfico que compara las predicciones con los valores actuales (prediction en el eje X). 
ggplot(golf, aes(x = predictions, y = price)) + 
  geom_point()  +
  geom_abline(color = "blue")


# ================================
# Evaluación de la Predicción
# ================================

golf$predictions <- predict(modelo)
residual <- golf$price - golf$predictions
residuals(modelo)

#Estimación de la Raíz del Error Cuadrático Medio (RMSE)
RMSE <- sqrt(mean(residual^2))
RMSE

# install.packages('forecast')
library(forecast)
forecast::accuracy(object = golf$predictions, x= golf$price)

# install.packages('Metrics')
library(Metrics)
mae(golf$price, golf$predictions)
mse(golf$price, golf$predictions)
mape(golf$price, golf$predictions)
# RMSE vs desviación estándar
rmse(golf$price, golf$predictions)
sd(golf$price)  #El modelo tiende a estimar mejor los precios en comparación a simplemente tomar la media
