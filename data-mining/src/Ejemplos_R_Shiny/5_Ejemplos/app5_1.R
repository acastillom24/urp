library(shiny)
library(tidyverse)

#-------------------------------------------------------------------------------

# Leer datos
df <- read_csv("datos_congreso_usa.csv")
# Seleccionar solo 3 columnas
df <- df %>% select(c(Congreso=congress, Ideologia=dwnom1, Partido=dem))
# Renombrar los elementos de una columna
df$Partido <- recode(df$Partido,`1`="Demócrata",`0`="Republicano")
# Eliminar datos faltantes
df = drop_na(df)

#-------------------------------------------------------------------------------

# Interfaz de usuario
ui <- fluidPage(
  
  # Título de la aplicación
  titlePanel("Ideología en el congreso de EE.UU."),
  
  # Barra con una barra deslizadora para ingresar el número de intervalos
  sidebarLayout(
    sidebarPanel(
      sliderInput("ncongreso", "Congreso Nro:", value = 93, min = 93, max = 114)
      ),
    
    # Gráfico de la distribución general
    mainPanel( plotOutput("plot_congreso") )
  )
  
)


# Lógica del servidor
server <- function(input, output) {
  
  output$plot_congreso <- renderPlot({
    
    ggplot(
      filter(df, Congreso == input$ncongreso),
      aes(x = Ideologia, color = Partido, fill=Partido))+
      geom_density(alpha = 0.5)+
      xlim(-1.5, 1.5)+
      xlab("Ideología - Valor nominal")+
      ylab("Densidad")+
      scale_fill_manual(values = c("blue", "red"))+
      scale_color_manual(values = c("blue", "red"))
  })
  
}


shinyApp(ui, server)
