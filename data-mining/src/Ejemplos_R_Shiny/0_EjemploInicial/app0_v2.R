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
  
  # Comportamiento del cuadro de texto "resumen"
  output$resumen <- renderPrint({
    df <- get(input$dataset, "package:datasets")
    summary(df)     # Texto que irá en el cuadro
  })
  
  # Comportamiento de la tabla "tabla"
  output$tabla <- renderTable({
    df <- get(input$dataset, "package:datasets")
    df              # dataframe que irá en la tabla
  })
  
}


# Construir e iniciar la aplicación
shinyApp(ui, server)