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
df <- read_csv("final_vorläufig.csv")
#zulassungen <- read_csv("ZulassungenGefiltert.csv") # Annahme: Pfad angepasst

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
      #selectInput("selectedPart", "Teil auswählen", choices = unique(df$ID_T01)),
      radioButtons("auswahlModusGemeinden", "Anzeigemodus:",
                   choices = list("Alle Gemeinden anzeigen" = "alle", "Eine Gemeinde auswählen" = "einzel")),
      uiOutput("ortAuswahlUI"),
      #selectInput("selectedLocation", "Ort auswählen", choices = NULL), # Wird serverseitig gefüllt
      #selectInput("selectedMonth", "Monat auswählen", choices = NULL),
      #actionButton("update", "Daten aktualisieren"),
      # Dropdown-Menü für die Auswahl zwischen allen Monaten oder einem spezifischen Monat
      radioButtons("auswahlModusMonat", "Anzeigemodus:",
                   choices = list("Alle Monate anzeigen" = "alle", "Einen Monat auswählen" = "einzel")),
      # Dropdown-Menü für die Monatsauswahl, das dynamisch basierend auf den vorhandenen Daten gefüllt wird
      uiOutput("monatAuswahlUI")
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
    monatliche_zaehlungen_liste <- list()
    
    # Schleife durch die Spalten _T01 bis _T40
    for (i in 1:40) {
      # Erzeuge den Spaltennamen
      spaltenname <- paste0("Fehlerhaft_Datum_T", sprintf("%02d", i))
      
      # Überprüfe, ob die Spalte im DataFrame existiert
      if (spaltenname %in% names(df)) {
        # Wandle das Datum um und zähle pro Monat und Jahr
        monatliche_zaehlung <- df %>%
          mutate(Datum = as.Date(.[[spaltenname]], format = "%Y-%m-%d"),
                 Monat = month(Datum),
                 Jahr = year(Datum)) %>%
          group_by(Jahr, Monat) %>%
          summarise(Anzahl = n(), .groups = 'drop')
        
        # Speichere das Ergebnis in der Liste
        monatliche_zaehlungen_liste[[spaltenname]] <- monatliche_zaehlung
      } else {
        # Gib eine Nachricht aus, wenn die Spalte nicht existiert
        message(spaltenname, " existiert nicht im DataFrame.")
      }
    }
    
    gesammelte_daten <- bind_rows(monatliche_zaehlungen_liste, .id = "Bauteil")
    
    # Konvertiere 'Bauteil' zu einem lesbaren Format (entferne 'Fehlerhaft_Datum_T' und lasse nur die Nummer)
    gesammelte_daten$Bauteil <- gsub("Fehlerhaft_Datum_T", "T", gesammelte_daten$Bauteil)
    
    # Erstelle eine neue Spalte 'MonatJahr' für die x-Achse des Diagramms
    gesammelte_daten <- gesammelte_daten %>%
      mutate(MonatJahr = paste(Jahr, sprintf("%02d", Monat), sep = "-"),
             MonatJahr = as.Date(paste0(MonatJahr, "-01"))) # Setze einen Dummy-Tag
  })
  

  
  # UI die die möglichen Städte anzeigt
  output$ortAuswahlUI <- renderUI({
    if (input$auswahlModus == "einzel") {
      choices <- unique(df$Gemeinden)
      selectInput("ortAuswahl", "Wähle eine Gemeinde:",
                  choices = choices)
    }
  })
  
  # UI die die möglichen Monate anzeigt
  output$monatAuswahlUI <- renderUI({
    if (input$auswahlModus == "einzel") {
      choices <- unique(format(filteredData()$MonatJahr, "%Y-%m"))
      selectInput("monatAuswahl", "Wähle einen Monat:",
                  choices = choices)
    }
  })
  
  # Erstelle das Balkendiagramm basierend auf der Auswahl
  output$stackedBarPlot <- renderPlot({
    daten <- filteredData()
    
    # Filtere die Daten, wenn ein spezifischer Monat ausgewählt wurde
    if (input$auswahlModus == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    
    ggplot(daten, aes(x = MonatJahr, y = Anzahl, fill = Bauteil)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d() +
      labs(x = "Monat und Jahr", y = "Anzahl der Fehler", fill = "Bauteil",
           title = "Kumulierte Fehler pro Monat für alle Bauteile") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
