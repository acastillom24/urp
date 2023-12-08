# =========================
# Selección de Alternativas
# =========================

library(shiny)

animales <- c("gato", "perro", "hamster", "loro", "otro", "ninguno")

ui <- fluidPage(
  
  # Lista desplegable con opciones
  # """"""""""""""""""""""""""""""
  # Solo selecciona una opción
  selectInput("animal1", "¿Cuál es tu animal favorito?", animales),
  
  # Selecciona más de una opción
  selectInput("animal2", "¿Qué animales te gustan?", animales,
              multiple = TRUE),
  
  # Selección con sub-clases
  selectInput("animalsub", "¿Qué raza prefieres?",
    choices =
      list(`perros` = list('Pastor alemán', 'Bulldog', 'Labrador'),
           `gatos` = list('Persa', 'Siamés', 'Criollo'))
  ),
  
  
  # Botones radiales (escoge una sola alternativa)
  # """"""""""""""""
  # Las alternativas muestran texto
  radioButtons("animal3", "¿Cuál es tu animal favorito?", animales),
  
  # Las alternativas pueden usar otros elementos (ejm: íconos)
  # Íconos disponibles: https://fontawesome.com/icons
  radioButtons("ID", "Escoger una alternativa:",
               # Lo que se muestra al usuario
               choiceNames = list(icon("truck-pickup"), icon("dog"), icon("bitcoin")),
               # Lo que se retorna al "servidor"
               choiceValues = list("gato", "perro", "bitcoin")
  ),
  
  # Checkbox (escoge varias alternativas)
  # """"""""
  # Varias alternativas
  checkboxGroupInput("animal4", "¿Qué animales te gustan?", animales),
  # Preguntas tipo Sí/No independientes
  checkboxInput("opcion1", "¿Habilitar opción 1?", value = TRUE),
  checkboxInput("opcion2", "¿Habilitar opción 2?")
    
)


server <- function(input, output, session){}
shinyApp(ui, server)