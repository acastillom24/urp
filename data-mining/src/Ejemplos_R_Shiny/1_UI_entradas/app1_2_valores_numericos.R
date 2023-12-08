# ============================
# Ingreso de valores numéricos
# ============================

library(shiny)

ui <- fluidPage(
  
  # Entrada de valores numéricos (enteros)
  numericInput("num1", "Número uno", value=10, min=0, max=20),
  
  # Entrada de valores numéricos (enteros)
  numericInput("num2", "Número dos", value=5.2, min=0, max=10, 
               step=0.2),
  
  # Entrada usando una barra de desplazamiento ("slider")
  sliderInput("num3", "Número tres", value=50, min=0, max=100),
  
  sliderInput("rng", "Rango de números", value=c(10, 20),
              min=0, max=100),
  
  # Al presionar "play", la barra se mueve automáticamente
  sliderInput("num4", "Seleccionar un número:", value=0,
              min=0, max=50, step=5, animate=TRUE)

)


server <- function(input, output, session){}
shinyApp(ui, server)