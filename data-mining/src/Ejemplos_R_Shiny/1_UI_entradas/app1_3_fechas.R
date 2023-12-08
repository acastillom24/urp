# =================
# Ingreso de fechas
# =================

library(shiny)

ui <- fluidPage(
  
  # Fecha única 
  dateInput("fecha1", "¿Cuándo naciste?", language="es"),
  
  # Rango entre dos días
  dateRangeInput("vac", "Fechas para vacaciones:", language="es",
                 separator = " hasta "),
  
  # Slider para escoger una fecha
  sliderInput("fecha2", "Escoger una fecha:", 
              value = as.Date("2021-02-15"),
              min = as.Date("2021-02-01"),
              max = as.Date("2021-02-28")),

  # Slider para escoger un rango de fechas
  sliderInput("fecha3", "Escoger una fecha:", 
              value = c(as.Date("2021-02-15"),as.Date("2021-02-20")),
              min = as.Date("2021-02-01"),
              max = as.Date("2021-02-28"))
  
)


server <- function(input, output, session){}
shinyApp(ui, server)