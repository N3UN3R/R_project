if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)
library(readr)
library(ggmap)

# Daten laden
daten <- read_csv("sindy_data.csv")
zulassungen <- read_csv("ZulassungenGefiltert.csv") # Annahme: Pfad angepasst

ui <- fluidPage(
  theme = shinytheme("flatly"), # Verwendung eines vordefinierten Themes; anpassbar
  tags$head(
    tags$style(HTML("
      .navbar { background-color: Lightsteelblue !important; }
      body { font-family: 'Arial', sans-serif; }
      .shiny-output-error { display: none; }
      .shiny-logo-text-container {
        display: flex;
        align-items: center;
      }
      .shiny-logo-text-container img {
        margin-right: 15px;
        height: 100px; /* Höhe des Logos anpassen */
      }
      .shiny-logo-text-container .title {
        color: white; /* Farbe des Titels anpassen */
        font-size: 24px; /* Größe des Titels anpassen */
      }
    "))
  ),
  
  titlePanel("Ausfallanalyse und Prognose"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedPart", "Teil auswählen", choices = unique(sindy_data$ID_T01)),
      selectInput("selectedLocation", "Ort auswählen", choices = NULL), # Wird serverseitig gefüllt
      actionButton("update", "Daten aktualisieren")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Balkendiagramm", plotOutput("stackedBarPlot")),
        tabPanel("Ausfallverlauf", plotOutput("failureTrend")),
        tabPanel("Interaktive Karte", leafletOutput("interactiveMap")),
        tabPanel("Datentabelle", DTOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reaktive Ausdrücke für gefilterte Daten
  filteredData <- reactive({
    sindy_data %>% 
      filter(ID_T01 == input$selectedPart & Gemeinden == input$selectedLocation & 
               between(as.Date(Fehlerhaft_Datum_T01), input$dateRange[1], input$dateRange[2]))
  })
  
  # Balkendiagramm
  output$barPlot <- renderPlot({
    # Umwandeln der Datumsstrings in Datumstypen
    daten$Fehlerhaft_Datum_T01 <- as.Date(daten$Fehlerhaft_Datum_T01)
    daten$Fehlerhaft_Datum_T02 <- as.Date(daten$Fehlerhaft_Datum_T02)
    
    # Berechnung der Anzahl von ID_T01 und ID_T02 pro Monat
    count_per_month_T01 <- daten %>%
      group_by(Monat_Jahr_T01 = floor_date(Fehlerhaft_Datum_T01, "month")) %>%
      summarise(Anzahl_T01 = n())
    
    count_per_month_T02 <- daten %>%
      group_by(Monat_Jahr_T02 = floor_date(Fehlerhaft_Datum_T02, "month")) %>%
      summarise(Anzahl_T02 = n())
    
    
    ggplot(filteredData(), aes(x = as.factor(Monat), fill = as.factor(Teil))) +
      geom_bar(position = "stack") +
      theme_minimal() +
      labs(title = "Monatliche Ausfälle nach Teil", x = "Monat", y = "Anzahl der Ausfälle")
  })
  
  # Ausfallverlauf
  output$trendPlot <- renderPlot({
    # Hier muss der Ausfallverlauf basierend auf `filteredData()` gezeichnet werden
  })
  
  # Karte
  output$map <- renderLeaflet({
    leaflet(data = zulassungen) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = ~Gemeinden) # Annahme: lon und lat Spalten müssen vorhanden sein oder erstellt werden
  })
  
  # Datentabelle
  output$dataTable <- renderDT({
    datatable(filteredData())
  })
}

shinyApp(ui, server)
