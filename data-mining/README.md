# Normalización, Discretización y Reducción

## Transformaciones de datos

### Suavizamiento
- Remoción de datos que tienen ruido

### Agregación
- Resumen de datos (ejemplo: medidas de tendencia central)
- Construcción de cubos de datos

### Normalización
- Normalización min-max
- Normalización por puntaje z (z-score)
- Normalización por escalamiento decimal

### Construcción de atributos
- Nuevos atributos con base en los atributos iniciales

## Normalización de los datos
- Puede mejorar la fase de aprendizaje
- Puede acelerar la fase de aprendizaje
- Puede evitar problemas numéricos como pérdida de precisión o desbordamiento aritmético

### Normalización usando puntaje Z
- Normaliza $x$ restando su media $(\bar{x})$ y dividiendo entre la desviación estándar $(s)$.

  $$z = \frac{x-\bar{x}}{s}$$

- No requiere conocimiento del mínimo o máximo de los datos originales.
- No elimina valores atípicos
- Mantiene la misma distribución probabilística
- Implementación en `R`
  ```r
  base::scale(var)
  reshape::rescaler(var)
  ```

### Normalización Min-Max
- Transforma de manera lineal los valores de una variable $x$ al rango $[a, b]$ especificado.

  $$x^{'}=\frac{(x-x_{\text{min}})(b-a)}{x_{\text{max}-x_{min}}}+a$$

  - $x_{min}$: valor mínimo de los datos originales
  - $x_{max}$: valor máximo de los datos originales

- Preserva las relaciones entre los datos.
- Se debe tener cuidado al escoger el rango $[a, b]$ de destino, ya que, si un nuevo dato cae fuera del rango original ocasionará un error.
- Implementación en `R`
  ```
  DMwR::ReScaling(var)
  ```

### Normalización por escalamiento decimal
- Transforma los valores $x$ al rango $[-1, 1]$ moviendo el punto decimal.
- Para un valor $x$, su valor normalizado $x^{'}$ será:
  $$x^{'} = \frac{x}{10^k}$$
  - $k$: entero más pequeño tal que $\text{max}(|x^{'}|) < 1$
- Ejemplo:
  - Si el valor de $x$ varía entre $-951$ y $812$, el valor máximo de $|x|$ es $951$.
  - Para normalizar se divide entre $10^3$ obteniendo $-0.951$ y $0.812$
- Solo es útil cuando los valores de los atributos son mayores que 1 en valor absoluto.

### Normalización sigmoidal
- Transformación no lineal (a veces llamada *softmax*)
- Si se basa en la tangente hiperbólica transforma los datos al rango $[-1, 1]$:
  $$x^{'} = \frac{1-e^{-z}}{1+e^{-z}} \text{ , donde $z$ es el puntaje $z$}$$
- Si se basa en la función logística, transforma los datos al rango $[0, 1]$:
  $$x^{'} = \frac{1}{1+e^{-z}} \text{ , donde $z$ es el puntaje $z$}$$
- La región central es cuasi lineal (y los extremos son no lineales)
- Efectos:
  - Los valores dentro de una desviación estándar son mapeados a la región cuasi lineal de la sigmoide.
  - Los puntos anómalos son comprimidos a lo largo de las colas de la sigmoide.
- Útil cuando se tiene datos anómalos que se desea incluir. 

## Discretización de datos

### Discretización Estática
- Los atributos continuos se discretizan antes de la tarea de clasificación.
- Se considera preprocesamiento de datos.

### Discretización Dinámica
- Los atributos continuos se discretizan dentro de los algoritmos de clasificación.
- No son un paso de preprocesamiento (son parte del método de clasificación).
- Ejemplo: árboles de decisión.

## Reducción de datos



# Modelo lineal normal (MLN)

$$y_i = \beta_0 + \beta_1x_{i1} + \beta_2x_{i2} + \cdots + \beta_px_{ip} + $$

$$y_i \sim $$

## Modelo Gamma



## Modelo Normal Inversa

### Distribución

$$p(x|\mu, \lambda) = \sqrt{\frac{\lambda}{2\pi x^3}}\text{exp}(-\frac{\lambda(x-\mu)^2}{2\mu^2x})$$

### Modelo

- Componente aleatorio: $y_i \sim NI(\mu_i,\phi)$
- Funciones de enlace
  - Identidad: $\mu_i = \beta_0 + \beta_1x_{i1} + \beta_2x_{i2} + \cdots + \beta_px_{ip}$
  - Logarítmica: $log(\mu_i) = \beta_0 + \beta_1x_{i1} + \beta_2x_{i2} + \cdots + \beta_px_{ip}$
  - Recíproca cuadrática: $\mu_i = \frac{1}{(\beta_0 + \beta_1x_{i1} + \beta_2x_{i2} + \cdots + \beta_px_{ip})^2}$

## Calidad del ajuste

- Indica qué tan bien se estima los parámetros del modelo.
- Se utiliza con fines explicativos.

## Evaluación de la predicción

- Se utiliza la validación cruzada para evaluar el comportamiento de cada modelo por separado.
- La evaluación se puede realizar con respecto a alguna de las métricas (RMSE, R2, MAE)
- Se puede gráfica la distribución para ver la variabilidad a lo largo de las muestras de la validación cruzada.
- Para modelos con similar RMSE, se prefiere el que tenga menor variabilidad.