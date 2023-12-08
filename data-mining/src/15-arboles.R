# ========================================================
#  Ejemplo: Precios de Autos Usados
# ========================================================

# Leer datos
library(foreign)
golf <- read.dta(file="golffull.dta")

# formul <- price ~ kilometerop1+kilometerop2+ageop1+ageop2
formul <- price ~ kilometer + age + TIA + extras1 + extras2


#------------------------------------------------------------------
#   Árboles de Regresión: CART (RPART)                             
#------------------------------------------------------------------

# https://cran.r-project.org/web/packages/rpart/
# https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
library(rpart)

# Valores de la raíz
mu = mean(golf$price)
mu
sum((golf$price-mu)^2)    # Devianza

# Ejemplo 1: considerando minbucket=50
# Estimar el árbol
arbol1 = rpart(formul, data=golf, minbucket=50)
arbol1
predict(arbol1)

# Gráfico básico del árbol (alternativa 1)
plot(arbol1, margin = 0.25)
text(arbol1, use.n = TRUE)

# Gráfico del árbol (alternativa 2)
# install.packages("partykit")
library(partykit)
plot(as.party(arbol1), tp_args = list(id=FALSE))

# Gráfico del árbol (alternativa 3)
# install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(arbol1)

# Ejemplo 2: minbucket=20 (minsplit=60) para obtener un árbol con más ramas
arbol2 = rpart(formul, data=golf, minbucket=20)
plot(as.party(arbol2), tp_args = list(id=FALSE))

# Ejemplo 3: usando parámetro de complejidad (cp=0.05)
arbol3 = rpart(formul, data=golf, cp=0.05)
plot(as.party(arbol3), tp_args = list(id=FALSE))

# Ejemplo 4: cp=0.001 (para obtener un árbol con más ramas)
arbol4 = rpart(formul, data=golf, cp=0.001)
plot(as.party(arbol4), tp_args = list(id=FALSE))

# Ejemplo 5: usando el número máximo de niveles (maxdepth=3)
arbol5 = rpart(formul, data=golf, maxdepth=3)
plot(as.party(arbol5), tp_args = list(id=FALSE))

# Ejemplo 6: Podar el árbol (post-poda)
set.seed(666)
arbol = rpart(formul, data=golf, cp=0.001)              # Inicial
arbol6 = prune(arbol, cp=0.1)                           # Poda del árbol    
plot(as.party(arbol), tp_args = list(id=FALSE))
plot(as.party(arbol6), tp_args = list(id=FALSE))

# Elegir un valor de CP (parámetro de complejidad: cuánto disminuye la devianza)
printcp(arbol)
plotcp(arbol)

# Usando el criterio del Min xerror
arbol7a = prune(arbol, cp=0.0033588)   # cp=0.0036
plot(as.party(arbol7a), tp_args = list(id=FALSE))
library(Metrics)
Metrics::rmse(actual=golf$price, predicted=predict(arbol7a))

# Usando el criterio de +-xstd
arbol7b = prune(arbol, cp=0.0212)
plot(as.party(arbol7b), tp_args = list(id=FALSE))
Metrics::rmse(actual = golf$price, predicted = predict(arbol7b))

# Automatizando la selección del Valor óptimo de CP (criterio Min xerror)
arbol_completo <- rpart(formul, data=golf, cp=0, minbucket=3)
arbol_completo
xerr <- arbol_completo$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <- arbol_completo$cptable[minxerr, "CP"]
arbol_prune <- prune(arbol_completo, cp=mincp)
plot(as.party(arbol_prune), tp_args = list(id=FALSE))


# =====================================================================
#  Arboles de Clasificacion (Algoritmo Party - Inferencia condicional)
# =====================================================================

# install.packages("party")
library(party)
arbolc1 <- ctree(formul, data=golf)
arbolc1
plot(arbolc1)

ct = ctree_control(mincriterion=0.70)  # mincriterion = 1 - p_Valor
arbolc2 <- party::ctree(formul, data=golf, controls = ct)
arbolc2
plot(arbolc2)

pred <- Predict(arbolc2, golf)
Metrics::rmse(actual=golf$price, predicted=pred)

