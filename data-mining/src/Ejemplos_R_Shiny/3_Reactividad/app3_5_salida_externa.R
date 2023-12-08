# ========================================
# Ejemplo 5 de reactividad: salida externa
# ========================================
library(shiny)


ui <- fluidPage(
  textInput("nombre", "¿cuál es tu nombre?"),
  textOutput("saludo")
)


server <- function(input, output, session) {
  
  # Cadena de salida
  msg <- reactive(paste0("¡Hola ", input$nombre, "!"))
  
  # Salida
  output$saludo <- renderText(msg())
  
  # Observador (salida externa)
  observeEvent(input$nombre, {
    message("Saludo realizado")
  })
}


shinyApp(ui, server)