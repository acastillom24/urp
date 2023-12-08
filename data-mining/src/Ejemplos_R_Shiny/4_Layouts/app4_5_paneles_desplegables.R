# =======================================
# Ejemplo de layout: paneles desplegables
# =======================================
library(shiny)

ui <- navbarPage(
  "Título de la página",
  # Panel 1
  tabPanel("panel 1", 
           "Contenido del panel 1"
  ),
  
  # Panel 2
  tabPanel("panel 2", 
           "Contenido del panel 2"
  ),
  
  # Panel 3
  tabPanel("panel 3", 
           "Contenido del panel 3"
  ),
  
  # Panel 4: desplegable
  navbarMenu("subpaneles", 
             # Subpanel 1
             tabPanel("panel 4a", 
                      "Contenido del panel 4a"
             ),
             # Subpanel 2
             tabPanel("panel 4b", 
                      "Contenido del panel 4b"
             ),
             # Subpanel 3
             tabPanel("panel 4c", 
                      "Contenido del panel 4c"
             )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)