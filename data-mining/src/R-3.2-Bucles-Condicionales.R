# ==============================================================================
#   Expresiones Condicionales
# ==============================================================================

# if-else
# """"""""
# Ejemplo 1: mostrar la inversa si el valor es diferente de cero
a <- 5
if (a != 0){
  print(1/a)
} else{
  print("Cero no tiene inversa")
}

# Ejemplo 2: encontrar si al menos un estado tiene tasa<0.5 por cada 100000 hab
df <- read.csv('crimenes.csv') 
tasa <- df$total/df$population*1e5   # Tasa por cada 100000 habitantes
ind <- which.min(tasa)               # Encontrar la mínima tasa por cada 100000

if(tasa[ind] < 0.5){
  print(df$state[ind])
} else{
  print("No hay estados con tasa menor a 0.5")
}

# ifelse
# """"""""
a <- -5
ifelse(a>0, 1/a, NA)

# Usando vectores
a <- c(0, 1, 2, -4, 5)
resultado <- ifelse(a>0, 1/a, NA)
resultado

# Ejemplo: reemplazar NA con 0
load(file='datos_na.rda')
sum(is.na(na_example))

NoNAs <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(NoNAs))


# ==============================================================================
#   Bucles
# ==============================================================================

# Ejemplo 1
for(i in 1:5){
  print(i)
}

# Encontrar la suma (Sn) de los n primeros números hasta el número indicado
N <- 25 
Sn <- vector(length=N)  # Vector "vacío" (de FALSE)

for(n in 1:N){
  x <- 1:n
  Sn[n] <- sum(x)
}

plot(1:N, Sn)


# ==============================================================================
#   Funciones
# ==============================================================================

# Ejemplo: promedio
avg <- function(x){
  s <- sum(x)        # Interno a la función (no aparece en el workspace)
  n <- length(x)
  s/n
}

x = 1:100
avg(x)

identical(mean(x), avg(x))    # Comprobación con la función "mean"

# Ejemplo con 2 argumentos
avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

avg(x)
avg(x, arithmetic=F)


# "sapply": aplica la misma función a cada elemento de un vector, matriz, 
# data frame o lista (término a término)
x <- 1:10
sapply(x, sqrt)

calcular_Sn <- function(n){
  sum(1:n)
}
sapply(x, calcular_Sn)
