# ===================================
# Ejemplo de layout: barra lateral
# ===================================
library(shiny)

ui <- fluidPage(
  navlistPanel(
    id = "tabset",
    
    "Encabezado 1",
    
    # Panel 1
    tabPanel("panel 1",
             "Contenido del panel 1",
             textInput("valor11", "Elemento 1 en el panel 1", ""),
             textInput("valor12", "Elemento 2 en el panel 1", "")
    ),
    
    "Encabezado 2",
    
    # Panel 2
    tabPanel("panel 2",
             "Contenido del panel 2",
             textInput("valor21", "TextInput 1 en el panel 2", ""),
             textInput("valor22", "TextInput 2 en el panel 2", "")
    ),
    
    # Panel 3
    tabPanel("panel 3", 
             "Contenido del panel 3",
             textInput("valor31", "Entrada 1 en el panel 3", ""),
             textInput("valor32", "Entrada 2 en el panel 3", "")
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)