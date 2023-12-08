#install.packages("shiny")

library(shiny)

# Interfaz de la aplicación: estructura visual básica de la página
# FluidPage es la función de "layout"
ui <- fluidPage(   
  "Hola Mundo"
)

# Comportamiento de la aplicación (lado del "servidor")
server <- function(input, output, session){
  
}

# Construir e iniciar la aplicación
shinyApp(ui, server)

# Si no se utiliza RStudio, se puede ejecutar como
# shiny::runApp("ruta al directorio que contiene el app.R")