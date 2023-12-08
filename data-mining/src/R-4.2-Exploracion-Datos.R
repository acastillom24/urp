# ==============================================================================
#         Caso de Estudio: Marketing Directo                      
# ==============================================================================

df <- read.csv("MarketingDirecto.csv")
head(df, 10)    # Primeros 10 datos
str(df)         # Estructura de los datos

# Ordenamiento de niveles de variables categoricas ordinales
df$Edad <- factor(df$Edad, levels = c("Joven", "Media", "Adulta"), ordered=T)
df$Historial <- factor(df$Historial, levels = c("Bajo", "Medio", "Alto"),ordered=T)
str(df)


# ------------------------------------------------------------------------------
# 1. Tabla de distribución de frecuencia para la edad del cliente
# ------------------------------------------------------------------------------

# Tabla de Frecuencia para la edad
frec    <- table(df$Edad)                      # Frecuencia
porcent <- prop.table(table(df$Edad))*100      # Porcentaje
edad    <- cbind(frec, porcent)
edad

# install.packages("tidyverse")
library(tidyverse)      # Para poder usar %>%
table(df$Edad)
prop.table(table(df$Edad)) %>% round(digits = 2)

# Nota: para obtener un formato similar a SAS o SPSS se puede usar gmodels
# install.packages('gmodels')
library(gmodels)
CrossTable(df$Edad, format="SAS")
CrossTable(df$Edad, format="SPSS")


# ------------------------------------------------------------------------------
#  2. Gráficos para distribución de frecuencia de la edad (datos cualitativos)
# ------------------------------------------------------------------------------

# a) Gráfico de Barras
# """"""""""""""""""""
barplot(porcent, main="Distribución de las edades de los clientes", 
        xlab="Grupo Etario", col = 2:4,
        ylab="Porcentaje de Clientes")

# Usando ggplot2 (se carga al cargar tidyverse)
ggplot(data = df, aes(x = Edad, y = ..count.., fill = Edad)) +
  geom_bar() +
  scale_fill_manual(values = c("gray70", "orangered3","darkblue")) +
  labs(title = "Distribución de las edades de los clientes") +
  theme_bw() +
  theme(legend.position = "bottom")


# b) Gráfico de Sectores Circulares
# """""""""""""""""""""""""""""""""
pie(porcent, main="Distribución de la Edad de los Clientes")

# Colocar porcentajes
nombres <- paste(names(table(df$Edad)), "\n", round(porcent,2), "%", sep="")
pie(porcent, labels = nombres, main="Distribución de la Edad de los Clientes")

# Usando ggplot2
fi <- prop.table(table(df$Edad))
df2 <- as.data.frame(fi)
# Barras simples
pie <- ggplot(df2, aes(x="", y=Freq, fill=Var1)) + 
       geom_bar(stat="identity", width=1)
# Conversión a "pie" (coordenadas polares)
pie <- pie + coord_polar("y", start=0) + 
      geom_text(aes(label = paste0(round(Freq*100), "%")),
                    position = position_stack(vjust = 0.5))
# Cambio de escala de colores
pie <- pie + scale_fill_manual(values=c("#55DDE0", "#F6AE2D", "#F26419", "#999999"))
# Remover y añadir etiquetas/título
pie <- pie + labs(x = NULL, y = "% de clientes", fill = "Grupo Etario",
                  title = "Distribución del Grupo Etario")
# Limpiar el formato
pie <- pie + theme_classic() + theme(axis.line = element_blank(),
                                     axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     plot.title = element_text(hjust = 0.5, 
                                                               color = "#666666")) 
pie


# Gráfico de puntos
# """""""""""""""""
dotchart(as.numeric(porcent), labels = names(porcent), 
         main = "Distribución de la Edad de los Clientes", 
         xlab="% de Clientes")

 
# ------------------------------------------------------------------------------
# 3. Tabla de contingencia: edad vs historial del volumen de compra anterior
# ------------------------------------------------------------------------------
tabla1 <- table(df$Edad, df$Historial)
tabla1

# Otra alternativa
CrossTable(x=df$Edad, y=df$Historial)


# ------------------------------------------------------------------------------
# 4. Distribución condicional del historial de volumen por grupo de edades
# ------------------------------------------------------------------------------
tabla2 <- prop.table(tabla1, margin=1)
tabla2

# Barras agrupadas
# """"""""""""""""
barplot(t(tabla2), col=2:4, beside = TRUE,
        xlab="Grupo Etario",
        ylab="Proporción de Clientes",
        main="Distribución del historial de compra según grupo etario")
legend("topright",legend=levels(df$Historial),col=2:4,
       pch=15,title="Historial de Compra")

# Barras Componentes
# """"""""""""""""""
barplot(t(tabla2),col=2:4,
        xlab="Grupo Etario",
        ylab="Proporción de Clientes",
        main="Distribución del historial de compra según grupo etario")
legend("topright", legend=levels(df$Historial), col=2:4,
       pch=15, title="Historial de Compra")

# Usando ggplot2
ggplot(data = na.omit(df), aes(x = Edad, y = ..count.., fill = Historial)) +
  geom_bar(position = "fill") +
  labs(y = "Prop. de clientes", title = "Distribución del historial de compra según grupo etario") +
  theme_bw() +
  theme(legend.position = "bottom")


mosaicplot(~ Edad+Historial, data = df, color = 2:4, 
           main="Distribución del historial de compra según grupo etario")


# ------------------------------------------------------------------------------
#  5. Distribución de frecuencias para # hijos (datos cuantitativos discretos)
# ------------------------------------------------------------------------------

# Tabla de Frecuencias
# """"""""""""""""""""
frec <- table(df$Hijos)
porcent <- prop.table(table(df$Hijos))*100
hijos.tabla <- cbind(frec, porcent)
hijos.tabla

# Visualización de la Distribución de la Variable
# """"""""""""""""""""""""""""""""""""""""""""""""
# Gráfico de varas
plot(porcent, type="h", lwd=2, xlab="Número de hijos", ylab="Porcentaje de clientes",
     main="Distribución del número de hijos por cliente")
points(x = as.numeric(row.names(porcent)),
       y = as.numeric(porcent),
       pch = 19, cex = 1.5)

# Usando ggplot2
df2 <- as.data.frame(porcent)
names(df2) <- c("Porcentaje", "Frecuencia")
df2

ggplot(df2, aes(x=Porcentaje, y=Frecuencia)) +
  geom_segment( aes(x=Porcentaje, xend=Porcentaje, y=0, yend=Frecuencia)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, 
              shape=21, stroke=2) +
  labs(x = "# de hijos", y = "% de clientes", 
       title = "Distribución del número de hijos por cliente") 


# ------------------------------------------------------------------------------
# 6. Distribución de frecuencias para monto gastado (datos cuantitativos continuos)
# ------------------------------------------------------------------------------

# Tabla de frecuencia (usando la regla de Sturges)
factorx <- factor(cut(df$Monto, breaks=nclass.Sturges(df$Monto),right=TRUE))
xout <- as.data.frame(table(factorx))
colnames(xout)<-c("Monto","ni")
xout

# Visualización de la Distribución de la Variable
# ------------------------------------------------
# Histograma y polígono de frecuencia
h1 <- hist(df$Monto, breaks = "Sturges",
           xlab="Monto",
           ylab="Número de clientes")

# Usando ggplot2
ggplot(data=df, aes(Monto)) + geom_histogram(aes(y =..count..), fill="black", 
                                             alpha = .75, bins=20) + 
      geom_density(col=2) + labs(title="Monto") + 
      labs(x="Monto", y="# de clientes") + xlim(c(0, 6300)) 

# Histograma y Densidad
hist(df$Monto, prob=TRUE)
lines(density(df$Monto))

# Gráfico de Densidad
plot(density(df$Monto))

# Usando ggplot2
ggplot(data = df, aes(x = Monto),fill = Monto) +
  geom_density(color="darkblue", fill="lightblue") +
  geom_rug() +
  theme_bw()

# Boxplots
boxplot(df$Monto)

# Usando ggplot2
ggplot(data = df, aes(y = Monto)) + geom_boxplot(fill="lightblue") + theme_bw()


# ------------------------------------------------------------------------------
# 7. Análisis descriptivo para el monto (gasto)
# ------------------------------------------------------------------------------

# Resumen básico
summary(df)
summary(df$Monto)

# Función para calcular asimetria (Pearson)
ASP <- function(x){
  3*(mean(x)-median(x))/sd(x)
}
# Función para calcular el rango
rango <- function(x){
  diff(range(x))
}

me <- mean(df$Monto)
med <- median(df$Monto)
q1 <- quantile(x = df$Monto, probs = 0.25, type = 6)
q3 <- quantile(x = df$Monto, probs = 0.75, type = 6)
r <- rango(df$Monto)
ric <- IQR(df$Monto)
s <- sd(df$Monto)
asp <- ASP(df$Monto)

resumen <- as.matrix(rbind(me, med, q1, q3, r, ric, s, asp))
colnames(resumen)<-c("Valor")
resumen

# Usando tidyverse
df %>% summarise(Media = mean(Monto),
                 Mediana = median(Monto),
                 Q1 = quantile(Monto, probs = 0.25),
                 Q3 = quantile(Monto, probs = 0.75),
                 Min = min(Monto),
                 Max = max(Monto),
                 Rango = rango(Monto),
                 RIC = IQR(Monto),
                 S = sd(Monto),
                 Asimetria = ASP(Monto)
)

library(fBasics)
basicStats(df$Monto)

skewness(df$Monto)
kurtosis(df$Monto)


#-------------------------------------------------------------------------------
# 8. Análisis descriptivo comparativo del monto por estado civil
#-------------------------------------------------------------------------------

#Usando funciones apply
me <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=mean)
med <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=median)
q1 <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=quantile, probs=0.25, type = 6)
q3 <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=quantile, probs=0.75, type = 6)
r <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=rango)
ric <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=IQR)
s <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=sd)
asp <- tapply(X=df$Monto, INDEX=df$Ecivil, FUN=ASP)

resumen <- as.matrix(rbind(me, med, q1, q3, r, ric, s, asp))
resumen

#Usando tidyverse
df %>% group_by(Ecivil) %>%
                    summarise(Media = mean(Monto),
                    Mediana = median(Monto),
                    Q1 = quantile(Monto,probs = 0.25),
                    Q3 = quantile(Monto,probs = 0.75),
                    Min = min(Monto),
                    Max = max(Monto),
                    Rango = rango(Monto),
                    RIC = IQR(Monto),
                    S = sd(Monto),
                    Asimetria = ASP(Monto)
)

# Análisis comparativo usando visualización de datos
# """"""""""""""""""""""""""""""""""""""""""""""""""
boxplot(df$Monto ~ df$Ecivil,
        xlab="Estado Civil",ylab="Gasto",
        main="Comparacion del gasto por estado civil")

# Usando ggplot2
ggplot(data = df, aes(x = Ecivil, y = Monto, color = Ecivil)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  theme_bw()

ggplot(data = df, aes(x = Monto, fill = Ecivil)) +
  geom_density(alpha = 0.5) +
  geom_rug(aes(color = Ecivil), alpha = 0.5) +
  theme_bw()

