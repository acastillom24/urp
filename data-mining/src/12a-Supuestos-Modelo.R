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

#==========================================================================
# Estimación 
#==========================================================================

# Modelo de regresión
form <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2
modelo <- lm(formula=form, data=golf)
summary(modelo)


# Prueba de Significancia de la Regresión
# install.packages("sigr")
# library(sigr)
# wrapFTest(modelo)

# Residuos Ordinarios: valor real - valor predicho
residuos_1 <- modelo$residuals
residuos_2 <- residuals(modelo)
residuos_3 <- golf$price - modelo$fitted.values
summary(residuos_1)
# Verificación de los residuos
sum(residuos_1 - residuos_2)
sum(residuos_1 - residuos_3)

# Residuos Estandarizados
residuos_est <- rstandard(modelo)

# Residuos Studentizados
residuos_st <- rstudent(modelo)



#===============================================================================
#            Evaluación de Supuestos del modelo
#===============================================================================

# (1) Normalidad              
# """"""""""""""""

# Gráfico Q-Q
qqnorm(rstudent(modelo), 
       ylab="Cuantiles de los Residuos Estudentizados", 
       xlab="Cuantiles teóricos")
qqline(rstudent(modelo))


#--------------------------------------------------
# Datos Simulados
par(mfrow=c(3,3))
for(i in 1:9) qqnorm(rnorm(50))
# Distribución Asimétrica (asimetría positiva)
# for(i in 1:9) qqnorm(exp(rnorm(50))) 
# Distribución con colas largas (leptocúrtica)
# for(i in 1:9) qqnorm(rcauchy(50))
# Distribución con colas cortas (platicúrtica)
# for(i in 1:9) qqnorm(runif(50))
par(mfrow=c(1,1))
#---------------------------------------------------

# "Envelopes"
#source("http://www.poleto.com/funcoes/envel.norm.txt")
source("envel.norm.r")
envel.norm(modelo)

# Test de normalidad
shapiro.test(rstudent(modelo))


# (2) Homocedasticidad              
# """"""""""""""""""""""

# Gráfico de residuos estudentizados 
rs <- rstudent(modelo)    # Residuos studentizados
ypred <- predict(modelo)  # Valores ajustados (predicción de la media)

# Límites para detectar outliers: 
# Cuantiles de la distribución t-student (corrección de Bonferroni)
n <- dim(golf)[1]
p <- length(modelo$coefficients)
alfa <- 0.01
t <- qt(alfa/(n*2), df=n-p-1)    # distribución t

par(mfrow=c(2,2))
plot(ypred, rs, ylab="Residuos Studentizados", xlab="precio de venta estimado",
     main="residuos Studentizados vs precio de venta estimado",ylim=c(-5,5)) 
abline(-t, 0)
abline( t, 0)
abline( 0, 0)

plot(golf$kilometer, rs, ylab="Residuos Studentizados",
     xlab="kilometraje (miles de km)", 
     main="Residuos Studentizados vs kilometraje", ylim=c(-5,5)) 
abline(-t, 0)
abline( t, 0)
abline( 0, 0)

plot(golf$age, rs, ylab="Residuos Studentizados", xlab="edad en meses",
     main="Residuos Studentizados vs edad en meses", ylim=c(-5,5)) 
abline(-t, 0)
abline( t, 0)
abline( 0, 0)

plot(golf$TIA,rs, ylab="Residuos Studentizados",
     xlab="meses antes de la revisión",
     main="Residuos Studentizados vs meses antes de revisión", ylim=c(-5,5)) 
abline(-t, 0)
abline( t, 0)
abline( 0, 0)
par(mfrow=c(1,1))


#--------------------------------------------------
# Algunos Gráficos Simulados
par(mfrow=c(3,3))
# Varianza Constante (homocedasticidad)
for(i in 1:9) plot(1:50, rnorm(50))
# Varianza no constante fuerte (fuerte heterocedasticidad)
for(i in 1:9) plot(1:50, (1:50)*rnorm(50))
# Varianza no constante moderada (moderada heterocedasticidad)
for(i in 1:9) plot(1:50, sqrt((1:50))*rnorm(50)) 
# No Linealidad
for(i in 1:9) plot(1:50, cos((1:50)*pi/25)+rnorm(50))
par(mfrow=c(1,1))
#--------------------------------------------------

# Test de Heterocedasticidad (Test de Breusch-Pagan)
library(lmtest)
bptest(modelo, varformula = ~fitted.values(modelo), studentize=FALSE)
# Test chi-cuadrado (similar al anterior)
library(car)
ncvTest(modelo)   # Test para varianza no constante de error


# (3) Multicolinealidad    
# """"""""""""""""""""""""
car::vif(modelo)         # Factor de Inflación de Varianza


# (4) Autocorrelación    
# """"""""""""""""""""""""
# Gráfico de Residuos a través del tiempo
plot(residuals(modelo), ylab="Residuos")
abline(h=0)

# Prueba de Durbin-Watson
library(lmtest)
dwtest(modelo)



#==========================================================================
# Selección de Variables                                                 
#==========================================================================

# M1: Cálculo de todos los subconjuntos de modelos: algoritmo "leaps and bounds"
# ------------------------------------------------

form <- price ~ kilometerop1 + kilometerop2 + kilometerop3 + ageop1 + ageop2 +
                ageop3 + extras1 + extras2
modelo1 <- lm(formula=form, data=golf)

# install.packages("bestglm")
library(bestglm)
mselec <- bestglm(Xy = golf[,c(4:12, 1)], IC="AIC", family = gaussian)
modelo1 <- mselec$BestModel
summary(modelo1)

# M2: Selección Paso a Paso (Stepwise)
# -------------------------
library(MASS)
# Se pasa el modelo completo (con todas las variables) al algoritmo
form = price ~ kilometerop1 + kilometerop2 + kilometerop3 +
               ageop1 + ageop2 + ageop3 + extras1 + extras2 + TIA
modelofull <- lm(formula = form, data=golf)
mselec <- stepAIC(modelofull, trace=TRUE)

# Modelo obtenido
form <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2
modelo2 <- lm(formula=form, data=golf)
summary(modelo2)

# Estimación de la varianza
sum(modelo2$residuals^2)/(172-5)

