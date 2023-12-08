# Ejemplos de gráficos estadísticos en R

# demo(graphics)

# Lectura de datos
df <- read.csv('resultados.txt', header=TRUE, sep="\t")
df$genero <- factor(df$genero, levels = c("f", "m"), 
                    labels = c("Femenino", "Masculino"))

datos <- c(0, 1, 2, 12, 12, 14, 18, 21, 21, 23, 24, 25,
           28, 29, 30, 30, 30, 33, 36, 44, 45, 47, 51)


# ==============================================================================
#    DIAGRAMAS DE CAJA: BOXPLOTS
# ==============================================================================

boxplot(datos)
boxplot(datos, xlab = "Tiempo de parada", ylab = "Minutos")

# Varios diagramas en una misma figura
boxplot(df$algeb1, df$algeb2,
        xlab = "Resultados de semestres 1 y 2", ylab="Puntaje sobre 100")

boxplot(df$algeb1, df$algeb2,
        xlab = "Resultados de semestres 1 y 2", ylab="Puntaje sobre 100",
        names=c("Semestre 1", "Semestre 2"))

boxplot(df$algeb1~df$genero, ylab = "Puntaje (%)", xlab="",
        main="álgebra: Semestre 1")

# Mostrar subplots usando "par": diagramas organizados como matriz
par(mfrow = c(2,2))
boxplot(df$algeb1~df$genero, main="álgebra semestre 1", ylab="", xlab="")
boxplot(df$algeb2~df$genero, main="álgebra semestre 2", ylab="", xlab="")
boxplot(df$prog1~df$genero, main="Programación semestre 1", ylab="", xlab="")
boxplot(df$prog2~df$genero, main="Programación semestre 2", ylab="", xlab="")
par(mfrow = c(1,1))


# ==============================================================================
#  HISTOGRAMAS
# ==============================================================================
hist(df$prog1, xlab="Puntaje (%)", main="Programación semestre 1")

# Indicación explícita del número de categorías: "breaks"
hist(df$prog1, xlab="Puntaje (%)", main="Programación semestre 1", breaks=5)

par(mfrow = c(2,2))
hist(df$algeb1, xlab="álgebra", main = "Semestre 1", ylim = c(0, 35))
hist(df$algeb2, xlab="álgebra", main = "Semestre 2", ylim = c(0, 35))
hist(df$prog1, xlab="Programación", main = " ", ylim = c(0, 35))
hist(df$prog2, xlab="Programación", main = " ", ylim = c(0, 35))
par(mfrow=c(1,1))

# Histograma de porcentajes
h <- hist(df$prog1, plot=FALSE, breaks = 5)
h
h$density <- h$counts/sum(h$counts)*100       # calcula porcentajes
plot(h, xlab="Puntajes (%)", freq = FALSE, ylab="Porcentaje", 
     main = "Programación semestre 1")

# Especificación intervalos arbitrarios (R utiliza "densidad": área=1)
bins <- c(0, 40, 60, 80, 100) 
hist(df$prog1, xlab="Puntaje (%)", main="Programación semestre 1", breaks=bins)


# ==============================================================================
#   OTROS DIAGRAMAS
# ==============================================================================

# Diagrama de tallos y hojas
stem(df$prog1)

# Diagrama de dispersión
plot(df$prog1, df$prog2, xlab="Programación sem 1", ylab="Programación sem 2")

# Diagrama para todos los pares posibles: "pairs"
cursos <- df[2:5]
pairs(cursos)

# Diagrama temporal
fallas <- c(6,3,2,4,7,8,0,5,3,2,7,2,1,0,2,5,0,1,0,1)
ts.plot(fallas)


# ==============================================================================
#   DATOS CUALITATIVOS
# ==============================================================================

# Tablas
# """"""
notas <- c("A", "D", "C", "D", "C", "C", "C", "C", "F", "B")
notas
# Tabla de frecuencia
table(notas)
# Tabla de frecuencia relativa
table(notas)/length(notas)
prop.table(table(notas))

# Uso con data frames
table(df$genero)
with(data=df, table(genero))

# Gráficos de barras
# """"""""""""""""""
opar <- par(no.readonly = TRUE)      # Lee los parámetros actuales
par(mfrow=c(2, 2))                   # Gráfico con 2 filas y 2 columnas
barplot(table(notas), col="gray40", xlab = "Notas", ylab = "Frecuencia")
barplot(prop.table(table(notas)), col="gray40", xlab = "Grades",
        ylab = "Frecuencia relativa")
barplot(table(df$genero), col = "gray90", xlab = "Color", 
        ylab = "Frecuencia")
barplot(prop.table(table(df$genero)), col = "gray90",
        xlab = "Color", ylab = "Frecuencia relativa")
par(opar)


# Gráficos de puntos
# """"""""""""""""""
tabnotas = table(notas)
dotchart(as.numeric(tabnotas), labels = names(tabnotas), 
         main = "notas", bg = "gray40", xlim = c(0, 6))

# Gráficos circulares ("Pie chart")
# """""""""""""""""""
slices <- c(26,30,19,24)
lbls <- c("álgebra", "Inglés", "Física", "Biología")
pie(slices, labels=lbls, main="Matrícula")

