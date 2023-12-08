# ==============================================================================
#      DATA FRAMES
# ==============================================================================

# Ejemplos
# """""""""""

df <- data.frame(id = 11:13, 
                 genero = c("F", "F", "M"), 
                 masa = c(17, 18, 18))
df

colnames(df)          # nombres de cada columna ("estructura")
str(df)               # estructura del data frame

df2 <- data.frame(x = 1:3, y = c("a", "b", "c")) 
str(df2)

# Conversión de cadenas de caracteres (strings) en "factors"
df3 <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = TRUE) 
str(df3)

# Convertir matrix a data frame
mat <- matrix(1:12, 4, 3)   # Data, nrow, ncol
as.data.frame(mat)

# Acceso
# """""""""
df3$x                # selección de columna usando $
df3[c("y", "x")]	   # usando nombre de columas
ToothGrowth[10,1]    # usando índices (ToothGrowth es un df que existe en R)
ToothGrowth[2:3,1]

# Eliminar una columna
df$masa <- NULL
df

# Datos disponibles en R
data()
ToothGrowth

# Examinar un data frame
# """"""""""""""""""""""
str(ToothGrowth)   # Info sobre la estructura de un objeto
head(ToothGrowth)  # Primeros elementos: head(x, 10)
tail(ToothGrowth)  # Últimos elementos: tail(x, 10)
names(ToothGrowth) # Nombres de las variables
ToothGrowth$len    # Acceso a una columna


# Almacenamiento
# """"""""""""""
# Almacenamiento de la variable
save(df2, file="df2.RData")

# Almacenamiento como CSV
write.csv(df2, file = "df2a.csv")
write.csv(df2, file = "df2b.csv", row.names = FALSE)


