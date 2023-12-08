library(shiny)

# =========================
# Interfaz de la aplicación
# =========================
ui <- fluidPage(
  
  # Control de entrada: caja de selección (ID, etiqueta, alternativas)
  #    Permite seleccionar uno de los datasets de R 
  selectInput("dataset", label="Conjunto de datos",
              choices = ls("package:datasets")),
  
  # Control de salida
  verbatimTextOutput("resumen"),    # Muestra cuadro de texto ("resumen")
  tableOutput("tabla")              # Muestra una tabla ("tabla")
  
)


# ===============================
# Comportamiento de la aplicación (lado del "servidor")
# ===============================
server <- function(input, output, session){
  
  # Creación de una expresión reactiva
  df <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  # Comportamiento del cuadro de texto "resumen"
  output$resumen <- renderPrint({
    # Texto que irá en el cuadro
    summary(df())   # Se usa la expresión reactiva como si fuese una función
  })
  
  # Comportamiento de la tabla "tabla"
  output$tabla <- renderTable({
    # dataframe que irá en la tabla
    df()            # Se usa la expresión reactiva como si fuese una función
  })
  
}


# Construir e iniciar la aplicación
shinyApp(ui, server)