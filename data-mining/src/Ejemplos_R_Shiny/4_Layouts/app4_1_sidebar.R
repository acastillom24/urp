# ===================================
# Ejemplo de sidebarLayout
# ===================================
library(shiny)

ui <- fluidPage(
  
  titlePanel("Teorema de límite central"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Número de muestras:", value = 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("histog")
    )
  )

)


server <- function(input, output, session) {
  
  output$histog <- renderPlot({
    means <- replicate(1e4, mean(runif(input$n)))
    hist(means, breaks = 20)
  }, res = 96)

}


shinyApp(ui, server)