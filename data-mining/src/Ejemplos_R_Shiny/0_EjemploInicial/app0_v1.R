library(shiny)

# Interfaz de la aplicación
ui <- fluidPage(
  
  # Control de entrada: caja de selección (ID, etiqueta, alternativas)
  #    Permite seleccionar uno de los datasets de R 
  selectInput("dataset", label="Conjunto de datos",
              choices = ls("package:datasets"))

)

# Comportamiento de la aplicación (lado del "servidor")
server <- function(input, output, session){
  
}

# Construir e iniciar la aplicación
shinyApp(ui, server)