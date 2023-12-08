# ================================
# Ingreso de Cadenas de caracteres
# ================================

# install.packages("shiny")
library(shiny)

ui <- fluidPage(
  
  # Entrada de texto (pequeña)
  textInput("name", "¿Cuál es tu nombre?", placeholder = "nombre"),

  # Entrada de contraseñas (no se muestran los caracteres)
  passwordInput("password", "¿Cuál es tu contraseña?"),
  
  # Entrada de párrafos
  textAreaInput("story", "Cuéntame sobre ti", rows = 3)
)


server <- function(input, output, session){}
shinyApp(ui, server)