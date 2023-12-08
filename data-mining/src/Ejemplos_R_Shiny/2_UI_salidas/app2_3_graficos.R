# ================
# Salida: gráficos
# ================

library(shiny)

ui <- fluidPage(
  
  plotOutput("plot", width = "600px"),
  
  fluidRow(
    column(width = 6, plotOutput("plot1")),
    column(width = 6, plotOutput("plot2"))
  )
  
)


server <- function(input, output, session){
  
  output$plot <- renderPlot(
    plot(1:5, xlab = "x", ylab = "y", main="Título"), 
    res=96
  )
  
  output$plot1 <- renderPlot(plot(1:5))
  output$plot2 <- renderPlot(plot(1:10))
}


shinyApp(ui, server)