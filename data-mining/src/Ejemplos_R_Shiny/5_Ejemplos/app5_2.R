library(shiny)
library(tidyverse)

df <- read_csv("datos2.csv")

# df %>% ggplot(aes(x=varX,y=varY,color=Group))+geom_point()

# Interfaz de usuario
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "grupos",
                         label = "¿Qué grupos se desea mostrar?",
                         choices = c("a","b","c"),
                         selected = c("a","b","c"))
      ),
    mainPanel( plotOutput("grafico") )
  )
)


# Lógica del servidor
server <- function(input, output) {
  
  output$grafico <- renderPlot({
    
    plotdf <- filter(df, grupo %in% input$grupos)
    ggplot(dat = plotdf,
           aes(x=varX, y=varY, color=grupo)) + geom_point()
  })

}


shinyApp(ui, server)