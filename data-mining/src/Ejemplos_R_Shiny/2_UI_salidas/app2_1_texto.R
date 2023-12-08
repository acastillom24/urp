# =============
# Salida: texto
# =============

library(shiny)

ui <- fluidPage(
  
  textOutput("texto"),
  verbatimTextOutput("codigo")

)


server <- function(input, output, session){

  output$texto <- renderText({
    "Hola a todos"
  })
  
  output$codigo <- renderPrint({
    summary(1:10)
  })
  
}


shinyApp(ui, server)