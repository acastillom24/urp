#-------------------------------------------------------------------------
# MODELOS PARA DATOS POSITIVOS ASIMÉTRICOS                                
# ------------------------------------------------------------------------
# Ejemplo: Precios de Autos Usados                                        
#-------------------------------------------------------------------------

golf <- foreign::read.dta(file = "data/raw/golffull.dta")

# ======================================================================
# Explicación de variable de salida en términos de variables de entrada
# usando diferentes modelos
# ======================================================================

## Modelo Lineal Normal
# ___________________________________

modeloLN <- stats::lm(price ~ kilometerop1+kilometerop2+ageop1+ageop2, data=golf)
summary(modeloLN)
# Evaluación de la estimación de la calidad de ajuste (ejemplo: AIC)
stats::AIC(modeloLN)



# Envelope (verificar supuesto de normalidad)
# source('http://www.poleto.com/funcoes/envel.norm.txt')
source('src/envel.norm.R')
envel.norm(modeloLN)
# Histograma
hist(golf$price)

golf$pred_LN <- predict(modeloLN)
forecast::accuracy(object=golf$pred_LN, x=golf$price)


## Modelo Gamma con enlace identidad
#____________________________________

modeloGammaI = glm(price ~ kilometerop1+kilometerop2+ageop1+ageop2, data=golf,
                   family = Gamma(link=identity))
summary(modeloGammaI)
AIC(modeloGammaI)
#source('http://www.poleto.com/funcoes/envel.gama.txt')
source('src/envel.gama.R')
envel.gama(modeloGammaI)

golf$pred_gamma1 <- predict(modeloGammaI)
forecast::accuracy(object=golf$pred_gamma1, x= golf$price)


## Modelo Gamma con enlace logaritmico
# _______________________________________

modeloGammaL = glm(price~kilometerop1+kilometerop2+ageop1+ageop2, data=golf,
                   family=Gamma(link=log))
summary(modeloGammaL)
AIC(modeloGammaL)
envel.gama(modeloGammaL)

golf$pred_gammaL <- predict(modeloGammaL, type="response")
forecast::accuracy(object=golf$pred_gammaL, x= golf$price)


## Modelo Gamma con enlace recíproco
# _______________________________________

modeloGammaR = glm(price~kilometerop1+kilometerop2+ageop1+ageop2, data=golf,
                   family=Gamma(link=inverse))
summary(modeloGammaR)
AIC(modeloGammaR)
envel.gama(modeloGammaR)

golf$pred_gammaR <- predict(modeloGammaR, type="response")
forecast::accuracy(object=golf$pred_gammaR, x=golf$price)


## Modelo Normal Inversa con enlace identidad
# ______________________________________________

modeloNII = glm(price~kilometerop1+kilometerop2+ageop1+ageop2, data=golf,
                family = inverse.gaussian(link=identity))
summary(modeloNII)
AIC(modeloNII)

# source('http://www.poleto.com/funcoes/invgauss.txt')
# source('http://www.poleto.com/funcoes/envel.ig.txt')
source('src/envel.ig.R')
source('src/invgauss.R')
envel.ig(modeloNII)

golf$pred_NII <- predict(modeloNII, type="response")
forecast::accuracy(object=golf$pred_NII, x= golf$price)


## Modelo Normal Inversa con enlace logarítmico
# _______________________________________________

modeloNIL = glm(price~kilometerop1+kilometerop2+ageop1+ageop2, data=golf,
                family=inverse.gaussian(link=log))
summary(modeloNIL)
AIC(modeloNIL)
envel.ig(modeloNIL, link="log")

golf$pred_NIL <- predict(modeloNIL, type="response")
forecast::accuracy(object=golf$pred_NIL, x= golf$price)


## Modelo Normal Inversa con enlace recíproco cuadrático
# ________________________________________________________

modeloNIR = glm(price~kilometerop1+kilometerop2+ageop1+ageop2, data=golf,
                family=inverse.gaussian(link=1/mu^2))
summary(modeloNIR)
AIC(modeloNIR)
envel.ig(modeloNIR, link="1/mu^2")

golf$pred_NIR <- predict(modeloNIR, type="response")
forecast::accuracy(object=golf$pred_NIR, x=golf$price)



