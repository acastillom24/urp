# ================
# Ejemplo de temas
# ================
library(shiny)
library(ggplot2)

# Librería para aplicar temas a los gráficos
# install.packages("thematic")

# Para ver los temas disponibles: bslib::bootswatch_themes()

# Lista con los temas disponibles
temas <- c("cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", 
           "litera", "lumen", "lux", "materia", "minty", "pulse", "sandstone", 
           "simplex", "sketchy", "slate", "solar", "spacelab", "superhero", 
           "united", "yeti")
# Escoger un tema
tema <- temas[4]


ui <- fluidPage(
  
  # Asignar un tema
  theme = bslib::bs_theme(bootswatch = tema),
  
  # Layout de la interfaz
  sidebarLayout(
    sidebarPanel(
      textInput("txt", "Entrada de texto:", "escribir aquí"),
      sliderInput("slider", "Entrada de barra de desplazamiento:", 1, 100, 30)
    ),
    mainPanel(
      h1(paste0("Tema: ", tema)),
      h2("Gráfico que usa el tema"),
      p("Se requiere instalar la librería thematic"),
      plotOutput("plot")
    )
  )
)


server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
    }, res = 96)
  
}

shinyApp(ui, server)