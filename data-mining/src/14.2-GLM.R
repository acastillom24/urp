#-------------------------------------------------------------------------
# MODELOS PARA DATOS POSITIVOS ASIMÉTRICOS                                
# ------------------------------------------------------------------------
# Ejemplo: Precios de Autos Usados                                        
#-------------------------------------------------------------------------

library(foreign)
golf <- read.dta(file="golffull.dta")


# ======================================================================
# Evaluación de la calidad predictiva del modelo
# ======================================================================

library(caret)
set.seed(325)

# División de datos: conjunto de entrenamiento/validación (75%) y evaluación (25%)
idx_train <- createDataPartition(y=golf$price, p=0.75, list=FALSE, times=1)
datos_train <- golf[idx_train,]
datos_test  <- golf[-idx_train,]

mean(datos_train$price)
mean(datos_test$price)
mean(golf$price)

# ----------------------------------------------------------
# Validación Cruzada (K=10) con 5 repeticiones
# ----------------------------------------------------------
#   Revisar:
#     https://rdrr.io/cran/caret/man/models.html
#     names(getModelInfo())
#     http://topepo.github.io/caret/available-models.html
# ----------------------------------------------------------

formul <- price ~ kilometerop1 + kilometerop2 + ageop1 + ageop2

# Parámetros
k <- 10
repeticiones <- 5

trc <- trainControl(method="repeatedcv", number=k, repeats=repeticiones, 
                    returnResamp="all")

## Modelo Lineal Normal
# ______________________

set.seed(835)
modeloLN <- train(formul, data=datos_train, method ="glm", trControl=trc)

modeloLN
summary(modeloLN)

modeloLN$results    # RMSE, R2, MAE (y sus desviaciones estándar)
modeloLN$resample   # Valores para cada muestra (50 = K*repeticiones)

summary(modeloLN$resample$RMSE)  # Media del RMSE
sd(modeloLN$resample$RMSE)       # Desviación estándar del RMSE

se <- sd(modeloLN$resample$RMSE)/sqrt(k*repeticiones)
se

library(ggplot2)
library(ggpubr)
# Gráficas de evaluación de la precisión (evolución de RMSE con cada muestra)
p1 <- ggplot(data=modeloLN$resample, aes(x=RMSE)) + 
      geom_density(alpha=0.5, fill="gray50") +
      geom_vline(xintercept=mean(modeloLN$resample$RMSE), linetype="dashed") +
      theme_bw() 
p2 <- ggplot(data=modeloLN$resample, aes(x=1, y=RMSE)) +
      geom_boxplot(outlier.shape=NA, alpha=0.5, fill="gray50") +
      geom_jitter(width=0.05) + labs(x="") + theme_bw() +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
p3 <- ggplot(data=modeloLN$resample, aes(x=Rsquared)) + 
      geom_density(alpha=0.5, fill="gray50") +
      geom_vline(xintercept=mean(modeloLN$resample$Rsquared), linetype="dashed") + 
      theme_bw() 
p4 <- ggplot(data=modeloLN$resample, aes(x=1, y=Rsquared)) +
      geom_boxplot(outlier.shape=NA, alpha=0.5, fill="gray50") +
      geom_jitter(width=0.05) + labs(x="") + theme_bw() +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
p5 <- ggplot(data=modeloLN$resample, aes(x=MAE)) +
      geom_density(alpha=0.5, fill="gray50") +
      geom_vline(xintercept=mean(modeloLN$resample$MAE), linetype="dashed") +
      theme_bw() 
p6 <- ggplot(data=modeloLN$resample, aes(x=1, y=MAE)) +
      geom_boxplot(outlier.shape=NA, alpha=0.5, fill="gray50") +
      geom_jitter(width=0.05) + labs(x="") + theme_bw() +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)
final_plot <- annotate_figure(final_plot,
                              top=text_grob("Evaluación: Modelo Lineal Normal", size=15))
final_plot


## Modelo Gamma con enlace identidad
# ____________________________________

set.seed(835)
modeloGI <- train(formul, data=datos_train, method ="glm", 
                  family=Gamma(link=identity), trControl=trc)
modeloGI

summary(modeloGI)
modeloGI$results

# Gráficas de evaluación de la precisión (evolución de RMSE con cada muestra)
p1 <- ggplot(data=modeloGI$resample, aes(x=RMSE)) +
      geom_density(alpha=0.5, fill="gray50") +
      geom_vline(xintercept=mean(modeloGI$resample$RMSE), linetype="dashed") +
      theme_bw() 
p2 <- ggplot(data=modeloGI$resample, aes(x=1, y=RMSE)) +
      geom_boxplot(outlier.shape=NA, alpha=0.5, fill="gray50") +
      geom_jitter(width=0.05) + labs(x="") + theme_bw() +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
p3 <- ggplot(data=modeloGI$resample, aes(x=Rsquared)) + 
      geom_density(alpha=0.5, fill="gray50") +
      geom_vline(xintercept=mean(modeloGI$resample$Rsquared), linetype="dashed") +
      theme_bw() 
p4 <- ggplot(data=modeloGI$resample, aes(x=1, y=Rsquared)) +
      geom_boxplot(outlier.shape=NA, alpha=0.5, fill="gray50") +
      geom_jitter(width=0.05) + labs(x="") + theme_bw() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data=modeloGI$resample, aes(x=MAE)) +
      geom_density(alpha=0.5, fill="gray50") +
      geom_vline(xintercept=mean(modeloGI$resample$MAE), linetype="dashed") +
      theme_bw() 
p6 <- ggplot(data=modeloGI$resample, aes(x=1, y=MAE)) +
      geom_boxplot(outlier.shape=NA, alpha=0.5, fill="gray50") +
      geom_jitter(width=0.05) + labs(x="") + theme_bw() +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)
final_plot <- annotate_figure(final_plot,
                              top=text_grob("Evaluación: Gamma (enlace identidad)", size = 15))
final_plot


## Modelo Gamma con enlace logaritmico
# ____________________________________

set.seed(835)
modeloGL <- train(formul, data=datos_train, method="glm", 
                  family=Gamma(link=log), trControl=trc)
modeloGL
summary(modeloGL)
modeloGL$results


# Gráficas de evaluación de la precisión
p1 <- ggplot(data = modeloGL$resample, aes(x = RMSE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloGL$resample$RMSE),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modeloGL$resample, aes(x = 1, y = RMSE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = modeloGL$resample, aes(x = Rsquared)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloGL$resample$Rsquared),
             linetype = "dashed") +
  theme_bw() 
p4 <- ggplot(data = modeloGL$resample, aes(x = 1, y = Rsquared)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data = modeloGL$resample, aes(x = MAE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloGL$resample$MAE),
             linetype = "dashed") +
  theme_bw() 
p6 <- ggplot(data = modeloGL$resample, aes(x = 1, y = MAE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluación: Modelo Gamma (enlace logarítmico)", size = 15))
final_plot


## Modelo Gamma con enlace recíproco
# ____________________________________

set.seed(835)
modeloGR <- train(formul, data=datos_train, method ="glm", 
                  family=Gamma(link=inverse), trControl=trc)
modeloGR
summary(modeloGR)
modeloGR$results

## Gráficas de evaluación de la precisión
p1 <- ggplot(data = modeloGR$resample, aes(x = RMSE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloGR$resample$RMSE),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modeloGR$resample, aes(x = 1, y = RMSE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = modeloGR$resample, aes(x = Rsquared)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloGR$resample$Rsquared),
             linetype = "dashed") +
  theme_bw() 
p4 <- ggplot(data = modeloGR$resample, aes(x = 1, y = Rsquared)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data = modeloGR$resample, aes(x = MAE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloGR$resample$MAE),
             linetype = "dashed") +
  theme_bw() 
p6 <- ggplot(data = modeloGR$resample, aes(x = 1, y = MAE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluación: Modelo Gamma (enlace recíproco)", size = 15))
final_plot


## Modelo Normal Inversa con enlace identidad
# ___________________________________________

set.seed(835)
modeloNII <- train(formul, data=datos_train, method="glm",
                   family=inverse.gaussian(link=identity), trControl=trc)

modeloNII
summary(modeloNII)
modeloNII$results

## Gráficas de evaluación de la precisión
p1 <- ggplot(data = modeloNII$resample, aes(x = RMSE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNII$resample$RMSE),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modeloNII$resample, aes(x = 1, y = RMSE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = modeloNII$resample, aes(x = Rsquared)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNII$resample$Rsquared),
             linetype = "dashed") +
  theme_bw() 
p4 <- ggplot(data = modeloNII$resample, aes(x = 1, y = Rsquared)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data = modeloNII$resample, aes(x = MAE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNII$resample$MAE),
             linetype = "dashed") +
  theme_bw() 
p6 <- ggplot(data = modeloNII$resample, aes(x = 1, y = MAE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluación: Modelo Normal Inversa (enlace identidad)", size = 15))
final_plot


## Modelo Normal Inversa con enlace logaritmico
# ___________________________________________

set.seed(835)
modeloNIL <- train(formul, data=datos_train, method ="glm",
                   family=inverse.gaussian(link=log), trControl=trc)
modeloNIL
summary(modeloNIL)
modeloNIL$results

## Gráficas de evaluación de la precisión
p1 <- ggplot(data = modeloNIL$resample, aes(x = RMSE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNIL$resample$RMSE),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modeloNIL$resample, aes(x = 1, y = RMSE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = modeloNIL$resample, aes(x = Rsquared)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNIL$resample$Rsquared),
             linetype = "dashed") +
  theme_bw() 
p4 <- ggplot(data = modeloNIL$resample, aes(x = 1, y = Rsquared)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data = modeloNIL$resample, aes(x = MAE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNIL$resample$MAE),
             linetype = "dashed") +
  theme_bw() 
p6 <- ggplot(data = modeloNIL$resample, aes(x = 1, y = MAE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluación: Modelo Normal Inversa (enlace logarítmico", size = 15))
final_plot


## Modelo Normal Inversa con enlace recíproco cuadrático
# ___________________________________________

set.seed(835)
modeloNIRC <- train(formul, data=datos_train, method="glm",
                    family=inverse.gaussian(link=1/mu^2), trControl=trc)
modeloNIRC
summary(modeloNIRC)
modeloNIRC$results

## Gráficas de evaluación de la precisión
p1 <- ggplot(data = modeloNIRC$resample, aes(x = RMSE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNIRC$resample$RMSE),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modeloNIRC$resample, aes(x = 1, y = RMSE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = modeloNIRC$resample, aes(x = Rsquared)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNIRC$resample$Rsquared),
             linetype = "dashed") +
  theme_bw() 
p4 <- ggplot(data = modeloNIRC$resample, aes(x = 1, y = Rsquared)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data = modeloNIRC$resample, aes(x = MAE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modeloNIRC$resample$MAE),
             linetype = "dashed") +
  theme_bw() 
p6 <- ggplot(data = modeloNIRC$resample, aes(x = 1, y = MAE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluación: Modelo Normal Inversa (enlace recíproco cuadrático)", size = 15))
final_plot




# Comparación de los Modelos Lineales Generalizados
modelos <- list(Normal=modeloLN, Gamma_Ident=modeloGI, Gamma_Log=modeloGL,
                Gamma_Recip=modeloGR, NI_Ident=modeloNII, NI_Log=modeloNIL,
                NI_RecipC=modeloNIRC)

resultados_resamples <- resamples(modelos)
resultados_resamples

# Extraer información relevante
library(tidyverse)
resultados_resamples$values %>% head(10)


# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key="modelo", value="valor", -Resample) %>%
  separate(col="modelo", into=c("modelo", "metrica"), sep="~", remove=TRUE)

metricas_resamples %>% head()


# Comparación de los RMSE, MAE y R2 promedio 
metricas_resamples %>% group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>% spread(key = metrica, value = media) %>%
  arrange(RMSE)
#arrange(desc(Rsquared))


# Gráfica para comparar los modelos según RMSE
metricas_resamples %>%
  filter(metrica == "RMSE") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0.5,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 15, color = "firebrick") +
  geom_text(color = "white", size = 5) +
  scale_y_continuous(limits = c(0.5, 1)) +
  # RMSE de referencia
  geom_hline(yintercept = 0.73, linetype = "dashed") +
  annotate(geom = "text", y = 0.72, x = 7.5, label = "RMSE referencia") +
  labs(title = "Validación: RMSE medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()

# Gráfica para comparación considerando la variabilidad
metricas_resamples %>% filter(metrica == "RMSE") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 2.5)) +
  # RMSE de referencia
  geom_hline(yintercept = 0.72, linetype = "dashed") +
  annotate(geom = "text", y = 0.72, x = 7.5, label = "RMSE de referencia") +
  theme_bw() +
  labs(title = "Validación: RMSE medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")


# Evaluar diferencias
# -------------------
# Test de Friedman
#   H0: todas las medianas son iguales
#   H1: alguna de las medianas (de los RMSE) es distinta a las demás

matriz_metricas <- metricas_resamples %>% filter(metrica == "RMSE") %>%
  spread(key = modelo, value = valor) %>%
  dplyr::select(-Resample, -metrica) %>% as.matrix()

# alfa = 0.05
friedman.test(y=matriz_metricas) # Las medianas son muy similares


# Error de Test
# -------------
predicciones <- extractPrediction(models = modelos,
                                  testX = datos_test[, -1],
                                  testY = datos_test$price)
predicciones %>% head()

metricas_predicciones <- predicciones %>% mutate(acierto = (obs-pred)^2) %>%
  group_by(object, dataType) %>% summarise(RMSE = sqrt(mean(acierto)))

# RMSE en conjunto de evaluación (y de prueba)
metricas_predicciones %>% spread(key = dataType, value = RMSE) %>%
  arrange(desc(Test))

ggplot(data = metricas_predicciones, aes(x = reorder(object, RMSE), y = RMSE,
                                         color = dataType, label = round(RMSE, 3))) +
      geom_point(size=14) + scale_color_manual(values = c("orangered2", "gray50")) +
      geom_text(color="white", size=3) + scale_y_continuous(limits=c(0.7, 0.84)) +
      # RMSE de referencia 
      geom_hline(yintercept = 0.73, linetype = "dashed") +
      annotate(geom = "text", y = 0.73, x = 8.5, label = "RMSE referencia") +
      coord_flip() + labs(title="RMSE de entrenamiento y test", x="Modelo") +
      theme_bw() + theme(legend.position = "bottom")

# Modelo Final
modeloLN



# ========================================================================
# MODELOS PARA DATOS DE CONTEO
# ========================================================================
# Conjunto de datos sobre el perfil de los clientes de uma determinada
# tienda que pertenecen a una de las 110 áreas de una ciudad
#
# Objetivo: relacionar el número esperado de clientes (nclientes) en cada
# área con
#  * número de domicilios (domic)
#  * renta media anual (renda) en USD
#  * edad media de los domicilios (idade) en años
#  * distancia entre el área y el competidor más próximo (dist1) en millas
#  * distancia entre el área y la tienda (dist2) en millas
# ========================================================================

store = as.data.frame(scan("store.dat", list(nclientes=0, domic=0, renda=0, 
                                             idade=0, dist1=0, dist2=0)))
attach(store)
head(store)


formul <- nclientes ~ domic + renda + idade + dist1 + dist2

# Modelo Lineal Normal
modeloLN <- lm(formul, data=store)
summary(modeloLN)
AIC(modeloLN)
#source('http://www.poleto.com/funcoes/envel.norm.txt')
source('envel.norm.r')
envel.norm(modeloLN)
# envel.norm(modeloLN, iden = 1)


# Modelo de Poisson con enlace logarítmico
modeloPL <- glm(formul, family=poisson(link=log), data=store)
summary(modeloPL)
AIC(modeloPL)
#source('http://www.poleto.com/funcoes/envel.pois.txt')
source('envel.pois.R')
envel.pois(modeloPL)
#envel.pois(modeloPL, iden=1)


# Modelo de Poisson con enlace raíz cuadrada
modeloPR <- glm(formul, family=poisson(link=sqrt), data=store)
summary(modeloPR)
AIC(modeloPR)
source('http://www.poleto.com/funcoes/envel.pois.txt')
envel.pois(modeloPR)
#envel.pois(modeloPR, iden=1)


# Modelo Binomial Negativo con enlace logarítmico
library(MASS)
modeloBNL <- glm.nb(formul, link=log, data=store)
summary(modeloBNL)
AIC(modeloBNL)
source('http://www.poleto.com/funcoes/envel.nb.txt')
envel.nb(modeloBNL)


# Modelo Binomial Negativo con enlace raíz cuadrada
modeloBNR <- glm.nb(formul, link=sqrt, data=store)
summary(modeloBNR)
AIC(modeloBNR)
source('http://www.poleto.com/funcoes/envel.nb.txt')
envel.nb(modeloBNR)


# Método de Validación Cruzada (K=10)
# -----------------------------------

# Número de repeticiones y semilla
k <- 10
repeticiones <- 7

hiperparametros <- data.frame(C = c("identity","log","sqrt")) # Para el modelo BN
trc <- trainControl(method = "repeatedcv", number = k, seeds = seeds,
                    repeats = repeticiones, returnResamp = "all")

set.seed(666)
seeds <- vector(mode="list", length=(k*repeticiones)+1)
for (i in 1:(k*repeticiones)) 
{
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(k*repeticiones) + 1]] <- sample.int(1000, 1)

## Modelo Lineal Normal 
modeloLN <- train(formul, data=store, method ="glm", family = gaussian,
                  trControl=trc)
modeloLN
summary(modeloLN)
modeloLN$results

## Modelo de Poisson con enlace logarítmico
modeloPL <-train(formul, data=store, method="glm", family=poisson(link=log),
                 trControl=trc)
modeloPL
summary(modeloPL)
modeloPL$results

## Modelo de Poisson con enlace raíz cuadrada
modeloPR <- train(formul, data=store, method="glm", family=poisson(link=sqrt),
                  trControl=trc)
modeloPR
summary(modeloPR)
modeloPR$results

## Modelo Binomial Negativo 
modeloBN <- train(formul, data=store, method='glm.nb', trControl=trc)
summary(modeloBN)
modeloBN$results
modeloBN$finalModel
