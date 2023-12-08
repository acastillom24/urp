# ===============
# Salida: tablas
# ===============

library(shiny)

ui <- fluidPage(
  
  tableOutput("testatica"),
  dataTableOutput("tdinamica"),
  dataTableOutput("tdinamica2")
  
)


server <- function(input, output, session){
  
  output$testatica <- renderTable(head(mtcars))
  
  output$tdinamica <- renderDataTable(mtcars, 
                                      options = list(pageLength = 5))

  output$tdinamica2 <- renderDataTable(
    mtcars, 
    options = list(pageLength = 5, searching = FALSE)
    )
  
}


shinyApp(ui, server)