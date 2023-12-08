# =====================================
# Ejemplo 3 de reactividad (con tiempo)
# =====================================

library(shiny)
library(ggplot2)

# Función: visualizar polígonos de frecuencia para cada dataset
poligfrec <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  # Creación del data frame
  df <- data.frame(x = c(x1, x2),
                   g = c(rep("x1", length(x1)), rep("x2", length(x2))))
  # Gráfico del polígono
  ggplot(df, aes(x, color=g)) +
    geom_freqpoly(binwidth=binwidth, size=1) + coord_cartesian(xlim=xlim)
}


ui <- fluidPage(
  fluidRow(
    column(3, 
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0)
    ),
    column(9, plotOutput("histog"))
  )
)


server <- function(input, output, session) {

  # Temporización (cambio cada 500 ms = 2 veces por segundo)
  timer <- reactiveTimer(500)
  
  # Expresiones reactivas
  x1 <- reactive({
    timer() # Dependencia reactiva del timer
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    timer() # Dependencia reactiva del timer
    rpois(input$n, input$lambda2)
  })
  
  # Gráfico
  output$histog <- renderPlot({
    poligfrec(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
  
}


shinyApp(ui, server)