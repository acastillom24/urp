#==========================================================================
# Ejemplo: Precios de Autos Usados                                         
#==========================================================================
#  Los datos de este ejemplo están relacionados con las ventas de autos
#  usados de la marca VW Golf. El objetivo es modelar la relación existente
#  entre los precios de venta en miles de Euros (variable price) y las cinco
#  variables explicativas:
#    * age: edad del auto en meses
#    * kilometer: kilometraje en miles de km
#    * TIA: número de meses hasta la próxima revisión técnica
#    * extras1: Frenos ABS sí/no
#    * extras2: Techo solar sí/no
#    * kilometerop1: puntajes Z de la variable kilometer
#    * ageop1: puntajes Z de la variable age
#    * kilometerop2: puntajes Z de la variable kilometer al cuadrado
#==========================================================================

# Lectura de los datos
library(foreign)
golf <- read.dta(file="golffull.dta")

head(golf)
str(golf)

#-------------------------------------------------------------------------
# Análisis descriptivo
#-------------------------------------------------------------------------

# Distribución de los precios de autos usados (aparentemente no normal)
hist(golf$price, probability = TRUE)
lines(density(golf$price), col=2, lwd=2)

# Relación de los precios de autos usados con los posibles predictores
par(mfrow=c(3,2))
# Diagramas de Dispersión
plot(golf$age,golf$price,ylab="Precio de Venta (Miles Euros)",
     xlab="edad en meses",
     main="Dispersión: precio de venta vs edad en meses") 

plot(golf$kilometer,golf$price,ylab="Precio de Venta (Miles Euros)",
     xlab="kilometraje en miles de km",
     main="Dispersión: precio de venta vs kilometraje") 

plot(golf$TIA,golf$price,ylab="Precio de Venta (Miles Euros)",
     xlab="meses antes de la próxima revisión técnica",
     main="Dispersión: precio de venta vs meses antes de la revisión") 

# Diagramas de caja
boxplot(price ~ extras1, data = golf,
        main="Box plot: precio de venta con o sin ABS", 
        ylab="Ventas (Miles Euros)")

boxplot(price ~ extras2, data = golf,
        main="Box plot: precios de venta con o sin techo solar", 
        ylab="Ventas (Miles Euros)")
par(mfrow=c(1,1))


#==========================================================================
# Estimación 
#==========================================================================

# Modelo completo
form <- price ~ kilometerop1 + kilometerop2 + kilometerop3 + ageop1 + ageop2 +
                ageop3 + extras1 + extras2
modelo <- lm(formula=form, data=golf)
summary(modelo)

# Estimación de los coeficientes
coef(modelo)

# Varianza del estimador: residuos^2/(n-p)
n <- nrow(golf)   # Número de datos
p <- 9            # Número de atributos utilizados
sum(modelo$residuals^2)/(n-p)


# Residuos Ordinarios: valor real - valor predicho
residuos_1 <- modelo$residuals
residuos_2 <- residuals(modelo)
residuos_3 <- golf$price - modelo$fitted.values
summary(residuos_1)
# Verificación de los residuos
sum(residuos_1 - residuos_2)
sum(residuos_1 - residuos_3)


# Modelo de regresión lineal final (descartando algunas variables)
form <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2
modelo <- lm(formula=form, data=golf)
summary(modelo)
