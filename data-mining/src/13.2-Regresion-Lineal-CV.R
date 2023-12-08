#==========================================================================
# Ejemplo: Precios de Autos Usados                                         
#==========================================================================

# Lectura de los datos
library(foreign)
golf <- read.dta(file="golffull.dta")
head(golf)


#==========================================================================
# Esquemas de Evaluación de la calidad predictiva del Modelo     
#==========================================================================

## Método de Retención (Entrenamiento: 75%, Evaluación: 25%)
# --------------------
set.seed(1243)
# Filas para el entrenamiento 
trows <- sample(rownames(golf), dim(golf)[1]*0.75)
data_train <- golf[trows, ]
# Filas restantes para la evaluación 
vrows <- setdiff(rownames(golf), trows) 
data_test<- golf[vrows, ]

# Entrenamiento del modelo
formul <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2
modelo <- lm(formul, data=data_train)
summary(modelo)

# Predicción del modelo (en el conjunto de entrenamiento)
data_train$pred <- predict(modelo)
library(Metrics)
rmse_train <- rmse(data_train$price, data_train$pred)
rmse_train

# Evaluación del modelo
data_test$pred <- predict(object=modelo, newdata=data_test)
rmse_test <- rmse(data_test$price, data_test$pred)
rmse_test

library(ggplot2)
ggplot(data_test, aes(x=pred, y=price)) + geom_point() + geom_abline()



# Validación Cruzada (CV K-Fold)
# ==============================
# install.packages('vtreat')
library(vtreat)
set.seed(1243)

k <- 5  # Número de grupos ("folds")
# División de los datos en 5 grupos
idx_split <- kWayCrossValidation(nRows=dim(golf)[1], nSplits=k)
# Muestra los k=5 grupos divididos
idx_split
# Muestra el primer grupo (train, app)
idx_split[[1]]

golf$cv5 <- 0 
# Para cada una de las 5 particiones
for(i in 1:k) {
  split <- idx_split[[i]]
  # Calcula el modelo con los datos de entrenamiento
  model <- lm(formul, data=golf[split$train,])
  # Predice usando los datos de evaluación (prueba)
  golf$cv5[split$app] <- predict(model, newdata=golf[split$app,])
}

rmse_CV <- rmse(golf$price, golf$cv5)
rmse_CV

# Usando la librería caret
#https://topepo.github.io/caret/measuring-performance.html
# install.packages('caret')
library(caret)
set.seed(1243)
cv2 <- train(formul, data=golf, method ="lm", 
       trControl=trainControl(method="repeatedcv", number=k, repeats=1))
cv2
cv2$results

summary(cv2$finalModel)


