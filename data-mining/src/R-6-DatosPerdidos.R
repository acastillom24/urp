#===============================================================================
#  DATOS PERDIDOS
#===============================================================================

# install.packages("VIM")
library(VIM)

# Datos a ser usados: tao 
help(tao)     # https://en.wikipedia.org/wiki/Tropical_Atmosphere_Ocean_project
df <- tao
names(df) <- c("Anho", "Latitud", "Longitud", "TempSup", "TempAire", 
               "Humedad", "Viento1", "Viento2")

head(df)
summary(df)
ndatos <- nrow(df)

# Mostrar qué columnas tienen valores perdidos
cidx_perd <- which(colSums(is.na(df))!=0)
cidx_perd

# Cantidad de valores perdidos en las columnas
nperdidos <- colSums(is.na(df[,cidx_perd]))
nperdidos

# Porcentaje de valores perdidos en las columnas
pperdidos <- 100*nperdidos/ndatos
pperdidos

# Gráfico de agregación: "aggregation plot"
agreg <- aggr(df, numbers=TRUE)
agreg
summary(agreg)
# Ejemplo de visualización diferente: ordenado según valores faltantes
aggr(df, numbers=TRUE, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)


#===============================================================================
#  MECANISMO DE DATOS PERDIDOS
#===============================================================================

# Gráfico de Matriz ("Matrix plot") ... En RStudio previamente USARr: x11()
# x11()
matrixplot(df)

# Boxplots paralelos ("Parallel boxplots")
VIM::pbox(df[4:6], pos=1)    # pos=1 indica que se desea mostrar la variable 1

# Prueba t de medias
t.test(TempSup ~ is.na(Humedad), data=df)


#===============================================================================
#  Eliminación de datos perdidos
#===============================================================================

# Solo si se está en un caso trivial (pocos datos perdidos)
df2 <- na.omit(df)
summary(df2)


#===============================================================================
#  Imputación                                           
#===============================================================================

# Usando una medida de tendencia central
# """"""""""""""""""""""""""""""""""""""
# install.packages('DMwR2')
library(DMwR2)
df3 <- centralImputation(df)
df3

df4 <- initialise(df, method="median")
df4


# Usando k-Vecinos más cercanos                 
# """""""""""""""""""""""""""""

# Con libreria DMwR
df_iknn <- knnImputation(df, k=5)

df[c(108:110, 463,551:552), ]
df_iknn[c(108:110, 463,551:552), ]

# Con libreria VIM
df_iknn2 <- VIM::kNN(data=df, variable=c("TempAire","Humedad"), k=5)
df_iknn2[c(108:110, 463,551:552),]

aggr(df_iknn2, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))
aggr(df_iknn2, delimiter="_imp",numbers=TRUE, prop=c(TRUE,FALSE), combined = TRUE)


# Usando Modelos de Regresión            
# """""""""""""""""""""""""""
# install.packages("simputation")
library(simputation)

# Usando la media (imputación para la variable TempAire y Humedad)
# Notas:
# El operador + no es suma, solo implica que se predice para 2 variables
# ~ 1 significa que se obtiene la media (no se incluye variables independientes)
dfreg1 <- impute_lm(df, TempAire + Humedad ~ 1)
df[c(108:110, 463,551:552),]
mean(df$TempAire, na.rm = TRUE)
mean(df$Humedad, na.rm = TRUE)
dfreg1[c(108:110, 463,551:552),]

# Usando la media de cada año (condicionada al año): valores con igual año
# tendrán la misma imputación, correspondiente a la media de dicho año
dfreg2 <- impute_lm(df, TempAire + Humedad ~ 1 | Anho)
df[c(108:110, 463,551:552),]
dfreg2[c(108:110, 463,551:552),]

# Usando más variables predictoras por año
# Las variables independientes son TempSup, Viento1, Viento2
dfreg3 <- impute_lm(df, TempAire + Humedad ~ TempSup + Viento1 + Viento2 | Anho)
df[c(108:110, 463,551:552),]
dfreg3[c(108:110, 463,551:552),]


# Iterative robust model-based imputation (IRMI)
# """"""""""""""""""""""""""""""""""""""""""""""
df_irmi <- irmi(df)
summary(df_irmi)
df_irmi[c(108:110, 463,551:552),]
