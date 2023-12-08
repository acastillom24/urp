# ===================================
# Ejemplo de layout: sidebar y tabs
# ===================================
library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      textOutput("panel")
    ),
    mainPanel(
      tabsetPanel(
        id = "idtab",
        
        # Primer panel
        tabPanel("panel 1", 
                 "Este es el panel 1",
                 textInput("valor1", "Elemento en el primer panel")
        ),
        
        # Segundo panel
        tabPanel("panel 2", 
                 "Este es el panel 2",
                 textInput("valor2", "Elemento en el segundo panel")
        ),
        
        # Tercer panel
        tabPanel("panel 3", 
                 "Este es el panel 3",
                 textInput("valor3", "Elemento en el tercer panel")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$panel <- renderText({
    paste("Panel actual: ", input$idtab)
  })
  
}

shinyApp(ui, server)