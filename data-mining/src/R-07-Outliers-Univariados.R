#===============================================================================
#  AN?LISIS UNIVARIADO DE VALORES AT?PICOS (OUTLIERS)
#===============================================================================


#===============================================================================
# EJEMPLO 1: USANDO DATOS SIMULADOS
#===============================================================================


set.seed(123)
# Creaci?n de una matrix (100x5) de valores distribuidos normalmente
M <- matrix(rnorm(5*100),100,5)

summary(M)
boxplot(M)
pairs(M) 

# Introducci?n de un valor at?pico (outlier) en la posici?n (23,4)
M[23,4] <- M[23,4]*10

summary(M)
boxplot(M)
pairs(M)

# Posici?n (?ndice) del valor m?nimo de la columna 4
which.min(M[,4])



#===============================================================================
#  EJEMPLO 2: EMPLEADOS DE UNA EMPRESA
#===============================================================================
#    ID = C?digo del empleado
#    SALINIT = Salario inicial
#    GENERO = G?nero
#    TTRABAJO = Meses en el trabajo
#    EDAD = Edad (a?os)
#    SALACTUAL = Salario actual
#    EXPACAD = Experiencia acad?mica (a?os)
#    EXPTRAB = Experiencia laboral previa (a?os)
#    CATTRAB = Categor?a laboral
#    MINORIA = Pertenencia a minor?a racial
#-------------------------------------------------------------------------------

df <- read.delim("DatosEmpresa.dat")
head(df)

# Asignaci?n de etiquetas
df$GENERO <- as.factor(df$GENERO);
levels(df$GENERO) <- c("Hombre", "Mujer", NA)

df$MINORIA <- as.factor(df$MINORIA);
levels(df$MINORIA) <- c("Blanco", "No blanco", NA)

df$CATTRAB <- as.factor(df$CATTRAB)
levels(df$CATTRAB)<- c(NA, "Oficinista", "Asistente", "Seguridad", 
                       "Académico", "Empleado", "Ejecutivo", "Técnico")

# Conversi?n de valores perdidos a NA (por defecto est?n como 0)
df$SALINIT[df$SALINIT == 0] <- NA
df$TTRABAJO[df$TTRABAJO == 0] <- NA
df$EDAD[df$EDAD == 0] <- NA
df$SALACTUAL[df$SALACTUAL == 0] <- NA
df$EXPACAD[df$EXPACAD == 0] <- NA
df$EXPTRAB[df$EXPTRAB == 0] <- NA

summary(df)

# Por "simplicidad" se omitir? los valores perdidos
df <- na.omit(df)
summary(df)


# Detecci?n usando la puntuaci?n Z
#---------------------------------

is.outlier_z <- function(x, k=2) {
  return(abs(scale(x)) > k)           # scale: (x-media)/desv_est
}

# ?ndices (T/F) de los salarios at?picos
idx_outliers_z <- is.outlier_z(df$SALINIT, k=3)
which(idx_outliers_z)

# Salarios at?picos
df$SALINIT[idx_outliers_z]

# Registros asociados con los salarios at?picos
df[idx_outliers_z, ]


# Detecci?n usando la regla de Tukey
#-----------------------------------

library(dplyr)
library(ggplot2)

# Funci?n para detectar outliers usando la regla de Tukey
is.outlier <- function(x, k=1.5) {
  return(x < quantile(x,0.25)-k*IQR(x) | x > quantile(x,0.75)+k*IQR(x))
}

# ?ndices (T/F) de salarios at?picos
idx_outliers <- is.outlier(df$SALINIT, k=3)

# Salarios "outlier"
df$SALINIT[idx_outliers]
# Registros asociados con los salarios "outliers"
df[idx_outliers,]

# Registros asociados con los valores at?picos para la variable EXPACAD
df[is.outlier(df$EXPACAD),]

# Boxplots para 6 variables (de manera independiente)
p1 <- df %>%
  mutate(outlier = ifelse(is.outlier(SALINIT), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = SALINIT)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()
p1

p2 <- df %>%
  mutate(outlier = ifelse(is.outlier(SALACTUAL), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = SALACTUAL)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p3 <- df %>%
  mutate(outlier = ifelse(is.outlier(TTRABAJO), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = TTRABAJO)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p4 <- df %>%
  mutate(outlier = ifelse(is.outlier(EDAD), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = EDAD)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p5 <- df %>%
  mutate(outlier = ifelse(is.outlier(EXPACAD), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = EXPACAD)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p6 <- df %>%
  mutate(outlier = ifelse(is.outlier(EXPTRAB), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = EXPTRAB)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

# install.packages('ggpubr')
library(ggpubr)
# Generar una sola gr?fica
final_plot <- annotate_figure(
  ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3),
  top = text_grob("Análisis Univariado de Valores Extremos", size = 15))
final_plot

# Detecci?n de valores extremos segmentada por grupos (seg?n g?nero)
p1 <- df %>%
  group_by(GENERO) %>%
  mutate(outlier = ifelse(is.outlier(SALINIT), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENERO, y = SALINIT)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p2 <- df %>%
  group_by(GENERO) %>%
  mutate(outlier = ifelse(is.outlier(SALACTUAL), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENERO, y = SALACTUAL)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p3 <- df %>%
  group_by(GENERO) %>%
  mutate(outlier = ifelse(is.outlier(TTRABAJO), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENERO, y = TTRABAJO)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p4 <- df %>%
  group_by(GENERO) %>%
  mutate(outlier = ifelse(is.outlier(EDAD), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENERO, y = EDAD)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p5 <- df %>%
  group_by(GENERO) %>%
  mutate(outlier = ifelse(is.outlier(EXPACAD), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENERO, y = EXPACAD)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p6 <- df %>%
  group_by(GENERO) %>%
  mutate(outlier = ifelse(is.outlier(EXPTRAB), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENERO, y = EXPTRAB)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

final_plot <- annotate_figure(
  ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3),
  top = text_grob("An?lisis Univariado de Valores Extremos por G?nero", size = 15))
final_plot



#-------------------------------------------------------
# Detecci?n usando diagramas de caja ajustados
# (Hubert and Vandervieren, 2008)   
# ------------------------------------------------------

# install.packages('robustbase')
library(robustbase)

# Histograma asim?trico
hist(df$EXPTRAB)

# Boxplots
par(mfrow=c(1,2))
boxplot(df$EXPTRAB)      # Usando Tukey (l?mites equidistantes)
adjbox(df$EXPTRAB)       # Usando Hubert ("asim?trico")
par(mfrow=c(1,1))
