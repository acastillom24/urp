# =================
# Botones de acción
# =================

library(shiny)

ui <- fluidPage(
  
  # Permitir que el usuario suba un archivo al servidor
  fileInput("IDupload", "Subir un archivo",
            buttonLabel = "Buscar",
            placeholder = "No se ha seleccionado nada"),
  
  # Realizar una acción
  actionButton("click1", "Hacer click"),
  actionButton("click2", "Hacer otro click", icon = icon("cocktail")),
  
  # Coloca todo en una nueva fila
  fluidRow(
    actionButton("click3", "Botón 1", class = "btn-primary btn-sm"),
    actionButton("click4", "Botón 2", class = "btn-success btn-xs"),
    actionButton("click5", "Botón 3", class = "btn-info"),
    actionButton("click6", "Botón 4", class = "btn-warning"),
    actionButton("click7", "Botón 5", class = "btn-danger btn-lg"),
  ),
  # Coloca todo en una nueva fila
  fluidRow(
    actionButton("click8", "Botón largo", class = "btn-block")
  )
)


server <- function(input, output, session){}
shinyApp(ui, server)