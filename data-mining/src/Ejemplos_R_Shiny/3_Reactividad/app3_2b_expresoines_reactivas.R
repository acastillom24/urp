# ====================================================
# Ejemplo 2 de reactividad (con expresiones reactivas)
# ====================================================

library(shiny)
library(ggplot2)

# ------------------------------------------------------------------------------

# Función: visualizar polígonos de frecuencia para cada dataset
poligfrec <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  # Creación del data frame
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  # Gráfico del polígono
  ggplot(df, aes(x, color=g)) +
    geom_freqpoly(binwidth=binwidth, size=1) + coord_cartesian(xlim=xlim)
}

# Función: prueba de hipótesis (t-test) que compara las medias de 2 datasets
t_test <- function(x1, x2) {
  t <- t.test(x1, x2)
  sprintf("valor p: %0.6f", t$p.value)
}

# ------------------------------------------------------------------------------


ui <- fluidPage(
  # Fila 1
  fluidRow(
    column(4, 
           "Distribución 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("media1", label = "µ", value = 0, step = 0.1),
           numericInput("desv1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4, 
           "Distribución 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("media2", label = "µ", value = 0, step = 0.1),
           numericInput("desv2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Polígono de frecuencia",
           numericInput("ancho", label = "Tamaño intervalo", 
                        value = 0.1, step = 0.1),
           sliderInput("rango", label = "range", value = c(-3, 3), 
                       min = -5, max = 5)
    )
  ),
  # Fila 2
  fluidRow(
    column(9, plotOutput("histog")),
    column(3, verbatimTextOutput("ttest"))
  )
)


server <- function(input, output, session) {
  
  # Expresiones reactivas
  x1 <- reactive(rnorm(input$n1, input$media1, input$desv1))
  x2 <- reactive(rnorm(input$n2, input$media2, input$desv2))
  
  output$histog <- renderPlot({
    # Gráfico
    poligfrec(x1(), x2(), binwidth = input$ancho, xlim = input$rango)
  }, res = 96)
  
  output$ttest <- renderText({
    # Valor de la prueba  
    t_test(x1(), x2())
  })
}


shinyApp(ui, server)