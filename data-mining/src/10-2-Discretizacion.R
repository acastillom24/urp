# =====================================================
#  Discretización                                      
# =====================================================
# install.packages("arules")
# install.packages("discretization")

# -----------------------------------------------------------------------------
# Ejemplo: Créditos de un Banco Alemán
#------------------------------------------------------------------------------

library(Fahrmeir)

help(credit)
df <- credit

head(df)
hist(df$DM)

# Discretización con intervalos de igual amplitud
# """""""""""""""""""""""""""""""""""""""""""""""
k <- nclass.scott(df$DM)        # Usando regla de Scott
# k <- nclass.Sturges(df$DM)    # Usando regla de Sturges
# k <- nclass.FD(df$DM)         # Usando regla de Friedman-Diaconis
k

# install.packages('arules')
library(arules)
# Discretizar (breaks: número de intervalos)
df$DM_int <- discretize(df$DM, method="interval", breaks=k)
table(df$DM_int)

# Discretización con intervalos de igual frecuencia
# """""""""""""""""""""""""""""""""""""""""""""""""
df$DM_dif <- discretize(df$DM, method="frequency", breaks=10)
table(df$DM_dif)

# Discretización por clusters (K-Medias)
# """"""""""""""""""""""""""""""""""""""
df$DM_cl <- discretize(df$DM, method="cluster", breaks=5)
table(df$DM_cl)

# Discretización por entropía
# """""""""""""""""""""""""""
# install.packages('discretization')
library(discretization)
# Se coloca primero los atributos (DM) y al final la clase (Y)
DM_entropia = mdlp( data = df[, c("DM","Y")] )

# Puntos de corte
DM_entropia$cutp
head(DM_entropia$Disc.data, n = 15)
table(DM_entropia$Disc.data)

df$DM_dentr <- DM_entropia$Disc.data[,1]
table(df$DM_dentr)

#Discretización con chiMerge (demora un poco)
DM_chim = chiM(data = df[,c("DM","Y")], alpha = 0.005)

DM_chim$cutp
head(DM_chim$Disc.data, n = 15)
table(DM_chim$Disc.data)

df$DM_chim <- DM_chim$Disc.data[,1]
table(df$DM_chim)

head(df, n = 15)
