# ========================
# Ejemplo 1 de reactividad 
# ========================

library(shiny)

ui <- fluidPage(
  textInput("nombre", "¿Cuál es tu nombre?"),
  textOutput("saludo")
)

server <- function(input, output, session) {
  output$saludo <- renderText({
    paste0("¡Hola ", input$nombre, "!")
  })
}

shinyApp(ui, server)