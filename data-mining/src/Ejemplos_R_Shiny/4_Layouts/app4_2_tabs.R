# ===================================
# Ejemplo de layout con tabs
# ===================================
library(shiny)

ui <- fluidPage(
  
  tabsetPanel(
    # Primer panel
    tabPanel("Importar datos", 
             fileInput("file", "Datos", buttonLabel = "Subir..."),
             textInput("delim", "Delimitador", ""),
             numericInput("omit", "Filas para omitir", value = 0, min = 0),
             numericInput("filas", "Filas para visualizar", value = 10, min = 1)
    ),
    
    # Segundo panel
    tabPanel("Establecer parámetros",
             textInput("valor2", "Elemento en el segundo panel")
    ),
    
    # Tercer panel
    tabPanel("Visualizar resultados",
             textInput("valor3", "Elemento en el tercer panel")
    )
  )
  
)


server <- function(input, output, session) {
}


shinyApp(ui, server)