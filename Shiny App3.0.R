if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}
if (!require("shinydashboard")) install.packages("shinydashboard")

library(shinydashboard)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)
library(readr)
library(ggmap)

shiny::addResourcePath("res", "C:/Users/Timon/OneDrive/Dokumente/R_project")
# Daten laden
df <- read_csv("final_vorläufig.csv")

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
      .tab-content {
        border: 2px solid #4682B4; /* Steel Blue */
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 20px;
        color: white; /* Textfarbe */
      }
    "))
  ),
  
  
  
  titlePanel(HTML('<div style="text-align: center;">Ausfallanalyse und Prognose in unseren Filialen</div>')),
  sidebarLayout(
    sidebarPanel(
      radioButtons("auswahlModusGemeinde", "Anzeigemodus:",
                   choices = list("Alle Gemeinden anzeigen" = "alle", "Eine Gemeinde auswählen" = "einzel")),
      uiOutput("ortAuswahlUI"),
      radioButtons("auswahlModusMonat", "Anzeigemodus:",
                   choices = list("Alle Monate anzeigen" = "alle", "Einen Monat auswählen" = "einzel")),
      uiOutput("monatAuswahlUI"),
      uiOutput("BauteilAuswahlUI"),
      uiOutput("dynamicValueBox"),
      uiOutput("logoOutput")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Balkendiagramm", plotOutput("stackedBarPlot")),
        tabPanel("Ausfallverlauf", plotOutput("failureTrend")),
        tabPanel("Interaktive Karte", leafletOutput("map"),
                 absolutePanel(top = 250, left = 20)),
        tabPanel("Datentabelle", DTOutput("dataTable"))
        
        
        
      )
    )
  )
)
df$Laengengrad <- as.numeric(gsub("(\\d+)(\\d{6})$", "\\1.\\2", df$Laengengrad))
df$Breitengrad <- as.numeric(gsub("(\\d+)(\\d{6})$", "\\1.\\2", df$Breitengrad))

# Selektiere nur die benötigten Spalten und entferne Duplikate
gemeinden_geodaten <- df %>%
  select(Gemeinden, Laengengrad, Breitengrad) %>%
  distinct()


server <- function(input, output, session) {
  output$logoOutput <- renderUI({
    div(
      img(src = "res/Logo_Autowerkstatt.jpg", style = "height: 100%; width: 100%; object-fit: cover;"),
      style = "display: flex; align-items: center; justify-content: center; height: 200px; background-color: Lightsteelblue; border-radius: 5px; border: 2px solid #4682B4; margin-bottom: 20px;"
    )
  })
  
  
  
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
          group_by(Gemeinden, Jahr, Monat) %>%
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
    
    # Füge die Koordinaten hinzu
    coordinates_list <- list()
    for (gemeinde in unique(df$Gemeinden)) {
      coordinates <- df %>%
        filter(Gemeinden == gemeinde) %>%
        select(Laengengrad, Breitengrad) %>%
        distinct()
      coordinates_list[[gemeinde]] <- coordinates
    }
    
    # Füge die Koordinaten zu gesammelte_daten hinzu
    gesammelte_daten <- gesammelte_daten %>%
      left_join(bind_rows(coordinates_list, .id = "Gemeinden"), by = "Gemeinden")
    
    return(gesammelte_daten)
  })  
  
  # Überwache Änderungen bei den beiden Auswahlmöglichkeiten
  observeEvent(c(input$auswahlModusGemeinde, input$auswahlModusMonat), {
    # Überprüfe, ob bei beiden 'Einzel' ausgewählt wurde
    if(input$auswahlModusGemeinde == "einzel" && input$auswahlModusMonat == "einzel") {
      # Zeige eine Benachrichtigungsnachricht an
      #shiny::showNotification("Du hast 'Einzel' für Gemeinde und Monat ausgewählt! Dadurch ist die Darstellung des Ausfallverlaufs nicht mehr sinnvoll. Den Wert kannst du dennoch ablesen!", type = "message", duration = 20)
      
      # Alternativ: Zeige ein Modal an
      showModal(modalDialog(
        title = "Benachrichtigung",
        "Du hast 'Einzel' für Gemeinde und Monat ausgewählt! Dadurch ist die Darstellung des Ausfallverlaufs nicht mehr sinnvoll. Den Wert kannst du dennoch ablesen!",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  }, ignoreInit = TRUE) # `ignoreInit = TRUE` sorgt dafür, dass dieser Code nicht beim Initialisieren der App ausgeführt wird.

  
  # UI für Gemeindenauswahl
  output$ortAuswahlUI <- renderUI({
    if (input$auswahlModusGemeinde == "einzel") {
      selectInput("ortAuswahl", "Wähle eine Gemeinde:", choices = unique(df$Gemeinden))
    }
  })
  
  # UI für Monatsauswahl
  output$monatAuswahlUI <- renderUI({
    if (input$auswahlModusMonat == "einzel") {
      choices <- unique(format(filteredData()$MonatJahr, "%Y-%m"))
      selectInput("monatAuswahl", "Wähle einen Monat:", choices = choices)
    }
  })
  
  #Prognose und Verlauf 
  # UI für die Auswahl der Einzelteile
  output$BauteilAuswahlUI <- renderUI({
    selectInput("BauteilAuswahl", "Wähle ein Teil:", choices = unique(filteredData()$Bauteil))
  })
  
  # Reaktive Expression für gefilterte Daten (Balkendiagramm)
  filteredData2 <- reactive({
    daten <- filteredData()
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      daten <- daten %>% filter(Gemeinden == input$ortAuswahl)
    }
    if (input$auswahlModusMonat == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    return(daten)
  })
  
  # Reaktive Expression für gefilterte Daten (Ausfallverlauf)
  filteredData3 <- reactive({
    daten <- filteredData()
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      daten <- daten %>% filter(Gemeinden == input$ortAuswahl)
    }
    if (input$auswahlModusMonat == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    if (input$auswahlModusGemeinde == "alle") {
      daten <- daten %>%
        group_by(MonatJahr, Bauteil) %>%
        summarise(Anzahl = sum(Anzahl), .groups = 'drop')
      
    }
    return(daten)
  })
  
  filteredData4 <- reactive({
    daten <- filteredData()
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      daten <- daten %>% filter(Gemeinden == input$ortAuswahl)
    }
    if (input$auswahlModusMonat == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    return(daten)
  })

  # Erstellen des Balkendiagramms basierend auf der Auswahl
  output$stackedBarPlot <- renderPlot({
    #daten <- filteredDataByMonth()
    
    ggplot(filteredData2(), aes(x = MonatJahr, y = Anzahl, fill = Bauteil)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d() +
      labs(x = "Monat und Jahr", y = "Anzahl der Fehler", fill = "Bauteil", 
           title = "Kumulierte Fehler pro Monat für alle Bauteile in ausgewählter Gemeinde") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # Reaktive Funktion zur Berechnung der Prognose
  prognosisData <- reactive({
    req(filteredData())  # Stellen Sie sicher, dass filteredData verfügbar ist
    
    # Filtern der Daten für die ersten Quartale von 2014, 2015 und 2016
    first_quarters <- filteredData() %>%
      filter(MonatJahr >= as.Date("2014-01-01") & MonatJahr <= as.Date("2014-03-31"))
    
    # Aggregieren der Daten nach Bauteil, um den Durchschnitt für das ausgewählte Bauteil im ersten Quartal zu erhalten
    #selected_prognosis_item <- input$BauteilAuswahl
    
    #ferstllenaggregierte werte
    first_quarter_aggregated <- first_quarters %>%
      filter(Bauteil == input$BauteilAuswahl) %>%
      summarise(Anzahl = mean(Anzahl))
    
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      first_quarter_aggregated <- first_quarters %>%
        filter(Bauteil == input$BauteilAuswahl) %>%
        filter(Gemeinden == input$ortAuswahl) %>%
        summarise(Anzahl = mean(Anzahl))
    
    }
    return(first_quarter_aggregated)
  })
  
  # Reaktive Funktion zur Berechnung der Prognose
  prognosisDataMap <- reactive({
    req(filteredData())  # Stellen Sie sicher, dass filteredData verfügbar ist
    
    # Filtern der Daten für die ersten Quartale von 2014, 2015 und 2016
    first_quarters <- filteredData() %>%
      filter(MonatJahr >= as.Date("2014-01-01") & MonatJahr <= as.Date("2014-03-31"))
    
    # Aggregieren der Daten nach Bauteil, um den Durchschnitt für das ausgewählte Bauteil im ersten Quartal zu erhalten
    selected_prognosis_item <- input$BauteilAuswahl
    
    # (2) Berechnet den Durchschnitt pro Gemeinde für das ausgewählte Bauteil
    durchschnittProGemeinde <- first_quarters %>%
      filter(Bauteil == selected_prognosis_item) %>%
      group_by(Gemeinden) %>%
      summarise(DurchschnittProGemeinde = mean(Anzahl), .groups = 'drop')
    
    return(durchschnittProGemeinde)
  })
  
  output$dynamicValueBox <- renderUI({
    value <- round(prognosisData()$Anzahl) # Hier könntest du einen reaktiven Ausdruck oder eine berechnete Variable einsetzen
    subtitleText <- paste("Prognose für", input$BauteilAuswahl, "im ersten Quartal 2017")
    tags$div(
      id = "custom-value-box",
      style = "padding: 20px; background-color: Lightsteelblue; color: white; border-radius: 5px; border: 2px solid #4682B4; margin-bottom: 20px; text-align: center;", # Hinzugefügtes text-align: center;
      tags$h3(value, style = "margin-top: 0;"),
      tags$p(subtitleText, style = "margin-bottom: 0;")
    )
  })
  
  # Ausfallverlauf für jedes Einzelteil von 2014-2016
  output$failureTrend <- renderPlot({
    # Daten von 2014 bis 2016 filtern
    daten <- filteredData3()
    daten <- daten %>% 
      filter(Bauteil == input$BauteilAuswahl) %>%
      na.omit(daten)
    
    y_max <- max(daten$Anzahl, na.rm = TRUE) * 1.4 # 10% mehr Platz oben
    y_min <- min(daten$Anzahl, na.rm = TRUE) * 0.6 # 10% Puffer unten

    # Plot erstellen
    plot <- ggplot(daten, aes(x = MonatJahr, y = Anzahl, fill = Bauteil)) +
      geom_line() +
      geom_point() +
      labs(x = "Monat und Jahr", y = "Anzahl der Fehler", color = "Bauteil",
           title = "Ausfallverlauf von 2014 bis 2016 für jedes Einzelteil") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(y_min, y_max)  
    
    return(plot)
    
  })
    
    #maps
    # Hinzufügen der Karten-Render-Funktion
    output$map <- renderLeaflet({
      popup_data <- left_join(gemeinden_geodaten,prognosisDataMap(), by = "Gemeinden")
      # Erstellen der Leaflet-Karte mit den vorbereiteten Daten
      leaflet(popup_data) %>%
        setView(lng = 10.4515, lat = 51.1657, zoom = 6) %>%
        addTiles() %>%
        addMarkers(lng = ~Laengengrad, lat = ~Breitengrad, popup = ~paste(Gemeinden, ": ", round(DurchschnittProGemeinde)))
    })
  
    # Datentabelle
    output$dataTable <- renderDT({
      # Verwende `select()` um nur die gewünschten Spalten zu behalten
      filtered_data_selected <- filteredData4() %>%
        select(Bauteil, Gemeinden, Jahr, Monat, Anzahl)
      
      # Anzeigen der gefilterten und ausgewählten Daten als Datentabelle
      datatable(filtered_data_selected)
    })
}

# App ausführen
shinyApp(ui, server = server)