# =================================================================
#  Ejemplo: Precios de Autos Usados
# =================================================================

library(foreign)
golf <- read.dta(file="golffull.dta")

formul <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2
# formul <- price ~ kilometer + age + TIA + extras1 + extras2


# =================================================================
#  K-NN (K-nearest neighbor) para construir el modelo
# =================================================================

library(Metrics)
# install.packages('kknn')
library(kknn)

# Con k = 1 (solo como ejemplo se usa test=golf)
modeloK1 <- kknn(formula=formul, train=golf, test=golf, k=1)
modeloK1
Metrics::rmse(actual=golf$price, predicted=modeloK1$fitted.values)

# Con k = 7 
modeloK7 <- kknn(formula=formul, train=golf, test=golf, k=7)
modeloK7
Metrics::rmse(actual=golf$price, predicted=predict(modeloK7))

# Con k = 10
modeloK10 <- kknn(formula=formul, train=golf, test=golf, k=10)
modeloK10
Metrics::rmse(actual=golf$price, predicted=modeloK10$fitted.values)

# Usando un kernel Gaussiano
modeloKG <- kknn(formula=formul, train=golf, test=golf, kernel="gaussian")
modeloKG
Metrics::rmse(actual = golf$price, predicted = predict(modeloKG))


# ===============================================================================
# Selección de hiperparámetros ("modelos")
# ===============================================================================

# Determinación del mejor valor de K (usando Caret y 10 CV)
particiones  <- 10 # valor de k (folds)
repeticiones <- 5

# Métrica para seleccionar los hiperparametros óptimos
metrica <- "RMSE"

# Definición de hiperparámetros
hiperparametros <- expand.grid(kmax = seq(from=1, to=50, by=2),
                               distance = 2,
                               kernel = c("optimal", "epanechnikov","gaussian")
)
hiperparametros

library(caret)
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, returnResamp = "final",
                              verboseIter = FALSE)


set.seed(555)
modeloKNN <- train(formul, data = golf, method = "kknn", tuneGrid = hiperparametros,
                   metric = metrica, trControl = control_train)
modeloKNN

ggplot(modeloKNN, highlight=TRUE) + scale_x_discrete(breaks=hiperparametros$kmax) +
       labs(title="Evolución de exactitud del modelo KNN", x="K") + theme_bw()

