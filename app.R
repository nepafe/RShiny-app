library(shiny)
library(dplyr)
library(lattice)

# Importem les dades
stressecho <- read.csv("C:/Users/NEUS/Desktop/UOC Màster en Bioinformàtica i Bioestadística/Assignatures/Software para el análisis de datos (Aula 2)/APPs/PEC3/PEC3-prova2/stressEcho.csv", header = TRUE, sep = ",") 

# Seleccionem les dades que utilitzarem
heavy <- subset(stressecho,hxofCig == "heavy", select = c(maxhr, dose))
moderate <- subset(stressecho,hxofCig == "moderate", select = c(maxhr, dose))
non_smoker <- subset(stressecho,hxofCig == "non-smoker", select = c(maxhr, dose))


# Definim una UI (interfície d'usuari) per la APP
ui <- fluidPage(
  
  # Hi afegim un títol
  titlePanel("“UCLA Stress Echocardiography Data”"),
  
  # Disseny de la barra lateral de l'APP amb les definicions de les entrades (inputs) i sortides (outputs)
  sidebarLayout(
    
    # Panell per les entrades de la barra lateral
    sidebarPanel(
      
      # Hi afegim una mica d'introducció mitjançant un 'helpText()'
      helpText("Aquestes dades són d'un estudi que volia determinar si un fàrmac anomenat 'dobutamina' es podria utilitzar eficaçment en un test per mesurar el risc d'un pacient a patir un atac de cor o un 'esdeveniment cardíac'. Aquest test implica posar en estrés fisiològic el cor del pacient augmentant el ritme cardíac fent exercici continuat mitjançant una cinta de còrrer i després prenent vaires mesures com la freqüència cardíaca i la pressió sanguínia, conjuntament amb altres mesures complicades del cor. Ara bé, aquest estrés en pacients d' edat avançada no es pot obtenir mitjançant l'exercici, però sí amb la dobutamina. Així que part d'aquest estudi era per comprovar que es mantingués l'eficàcia de la prova d'ecocardiografia ('stress echocardiography test') utilitzant dobutamina en comptes d'exercici físic.
                     Recuperat de:"),
      
      # Hi afegim un enllaç
      url <- a("https://hbiostat.org/data/repo/stressEcho.html", href="https://hbiostat.org/data/repo/stressEcho.html"),
      
      helpText("En aquest cas també en mirem els efectes segons els antecedents de tabaquisme del pacient: fumador compulsiu, moderat o no-fumador:"),
      # Indiquem les possibles entrades
      radioButtons("tipus_fumador", "Tipus de fumador:",
                   c("Compulsiu",
                     "Moderat",
                     "No fumador/a"),
                   selected = "No fumador/a"),
      
      # La funció 'br()' és un element per introduïr espai vertical extra
      br(),
      
      
      # Indiquem les possibles entrades de desplegable
      selectInput("dosi",
                  "Dosi de dobutamina:",
                  c("15 mg" = 15 ,
                    "20 mg" = 20,
                    "25 mg" = 25,
                    "30 mg" = 30, 
                    "35 mg" = 35,
                    "40 mg" = 40
                  ))
      
    ),
    
    # Panell principal per mostrar les sortides
    mainPanel(
      
      # Indiquem quines sortides volem i amb quin nom 
      tabsetPanel(type = "tabs",
                  tabPanel("Histograma", plotOutput("plot")),
                  tabPanel("Resums", verbatimTextOutput("summary")),
                  tabPanel("Dades", tableOutput("table"))
      )
      
    )
  )
)






# Definim la lògica del servidor per l'APP
server <- function(input, output) {
  
  # Creem una funció que ens seleccioni les dades que desitgem segons les opcions triades per l'usuari (entrades)
  d <- reactive({
    if (input$tipus_fumador == "Compulsiu") 
      tipus_fumador <- subset(heavy$maxhr, heavy$dose == input$dosi) 
    else if (input$tipus_fumador == "Moderat")
      tipus_fumador <- subset(moderate$maxhr, moderate$dose == input$dosi)
    else tipus_fumador <- subset(non_smoker$maxhr, non_smoker$dose == input$dosi)
  }
  )
  
  
  # Creem una gràfica de les dades, en aquest cas, un histograma ulitzant la funció creada anteriorment i n'indiquem les etiquetes
  output$plot <- renderPlot({
    tipus_fumador<- input$tipus_fumador
    dosi <- input$dose
    
    histogram(d() ,
              xlab = "Freqüència cardíaca màxima",
              ylab = "Percentatge",
              main = paste( tipus_fumador, " (", input$dosi, "mg", ")", sep = ""),
              col = "#75AADB", border = "white")
  })
  
  # Generem un resum de les dades
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generem una taula que ens permeti veure la llista de les dades seleccionades
  output$table <- renderTable({
    d()
  })
  
  
}

# Creem l'aplicació Shiny
shinyApp(ui, server)