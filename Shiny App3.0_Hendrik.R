# Laden der benötigten Pakete. Pakete werden installiert, falls sie nicht bereits installiert sind.
# Diese Pakete ermöglichen die Erstellung von interaktiven Webanwendungen (shiny), 
# die Nutzung von Themen für die Anwendung (shinythemes), 
# die Erstellung von Diagrammen (ggplot2), 
# den Zugriff auf Karten (ggmap), 
# und das Erstellen von Dashboard-Elementen (shinydashboard).
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("ggmap", quietly = TRUE)) install.packages("ggmap")
if (!require("shinydashboard")) install.packages("shinydashboard")

# Einbindung der Pakete in die R-Sitzung.
library(shinydashboard)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)
library(readr)
library(ggmap)

# Festlegen eines Pfades, unter dem Ressourcen wie Bilder gespeichert sind.
# Dies ermöglicht es uns, Bilder in der App leichter zu referenzieren.

#shiny::addResourcePath("res", "C:/Users/Timon/OneDrive/Dokumente/R_project")


# Laden und Vorbereiten der Daten für die Anwendung.
df <- read_csv("hendrik_timon_Lueko_final_ihr_bitches.csv")

# Auswahl relevanter Spalten für die Visualisierung und Entfernung von Duplikaten,
# um die Effizienz und Genauigkeit der Datenverarbeitung zu erhöhen.
gemeinden_geodaten <- df %>%
  select(Gemeinden, Laengengrad, Breitengrad) %>%
  distinct()


# Benutzerdefinierte Funktion zur Anwendung der Transformation der Koordinaten
apply_transformation <- function(x) {
  # Zählen der Zeichen
  char_count <- nchar(as.character(x))
  
  # Bedingung und Transformation für 7 oder 8 Zeichen
  if (char_count == 8 || char_count == 7) {
    return(as.numeric(gsub("(\\d+)(\\d{6})$", "\\1.\\2", x)))
  }
  # Bedingung und Transformation für 5 oder 6 Zeichen
  else if (char_count == 6 || char_count == 5) {
    return(as.numeric(gsub("(\\d+)(\\d{4})$", "\\1.\\2", x)))
  }
  # Keine Transformation, falls keine Bedingung zutrifft
  else {
    return(as.numeric(x))
  }
}

# Anwenden der Funktion auf die Spalten
gemeinden_geodaten$Laengengrad <- sapply(gemeinden_geodaten$Laengengrad, apply_transformation)
gemeinden_geodaten$Breitengrad <- sapply(gemeinden_geodaten$Breitengrad, apply_transformation)

#Bielefeld wird über die Funktion nicht richtig bereinigt und deshalb als direkter Wert übergeben
gemeinden_geodaten <- gemeinden_geodaten %>%
  mutate(Laengengrad = if_else(Gemeinden == "BIELEFELD", 8.531200, Laengengrad))


# Definition der Benutzeroberfläche (UI) der Shiny-App.
ui <- fluidPage(
  theme = shinytheme("flatly"), # Anwendung eines vordefinierten Themes für das Aussehen der App.
  
  # CSS-Styles, um das Aussehen der App anzupassen, z.B. die Farbe der Navigationsleiste,
  # die Schriftart, das Ausblenden von Fehlermeldungen und das Design der Tabs.
  tags$head(
    tags$style(HTML("
      .navbar { background-color: Lightsteelblue !important; }
      body { font-family: 'Arial', sans-serif; }
      .shiny-output-error { display: none; }
      .shiny-logo-text-container img { margin-right: 15px; height: 100px; }
      .shiny-logo-text-container .title { color: white; font-size: 24px; }
      .tab-content { border: 2px solid #4682B4; border-radius: 5px; padding: 10px; margin-bottom: 20px; color: white; }
    "))
  ),
  
  # Der Titel der Anwendung, zentriert dargestellt.
  titlePanel(HTML('<div style="text-align: center;">Ausfallanalyse und Prognose in unseren Filialen</div>')),
  
  # Sidebar für Filteroptionen und Hauptbereich für die Anzeige von Diagrammen, Karten und Datentabellen.
  sidebarLayout(
    sidebarPanel(
      # Auswahlmöglichkeiten für den Benutzer, um Daten nach Gemeinde und Monat zu filtern.
      radioButtons("auswahlModusGemeinde", "Anzeigemodus:", choices = list("Alle Gemeinden anzeigen" = "alle", "Eine Gemeinde auswählen" = "einzel")),
      uiOutput("ortAuswahlUI"), # Dynamische UI für die Auswahl einer Gemeinde.
      radioButtons("auswahlModusMonat", "Anzeigemodus:", choices = list("Alle Monate anzeigen" = "alle", "Einen Monat auswählen" = "einzel")),
      uiOutput("monatAuswahlUI"), # Dynamische UI für die Auswahl eines Monats.
      uiOutput("BauteilAuswahlUI"), # Dynamische UI für die Auswahl eines Bauteils.
      uiOutput("dynamicValueBox"), # Dynamische Box für Prognosewerte
      uiOutput("logoOutput") # Bereich für das Firmenlogo
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Balkendiagramm", plotOutput("stackedBarPlot")),
        tabPanel("Ausfallverlauf", plotOutput("failureTrend")),
        tabPanel("Interaktive Karte", leafletOutput("map"), absolutePanel(top = 250, left = 20)),
        tabPanel("Datentabelle", DTOutput("dataTable"))
      )
    )
  )
)

# Definition der Serverlogik
server <- function(input, output, session) {
  # Darstellung des Logos
  output$logoOutput <- renderUI({
    div(
      # Lädt ein Bild aus dem spezifizierten Pfad und passt es stilistisch an
      img(src = "res/Logo_Autowerkstatt.jpg", style = "height: 100%; width: 100%; object-fit: cover;"),
      # Stilisierung des Div-Containers, der das Bild umgibt
      style = "display: flex; align-items: center; justify-content: center; height: 200px; background-color: Lightsteelblue; border-radius: 5px; border: 2px solid #4682B4; margin-bottom: 20px;"
    )
  })
  
  # Reaktive Ausdrücke für gefilterte Daten
  filteredData <- reactive({
    # Initialisiert eine leere Liste, um die monatlichen Zählungen für jede Spalte zu speichern
    monatliche_zaehlungen_liste <- list()
    
    # Schleife durch die Spalten _T01 bis _T40
    for (i in 1:40) {
      # Generiert dynamisch den Spaltennamen basierend auf der Schleifeniteration
      spaltenname <- paste0("Fehlerhaft_Datum_T", sprintf("%02d", i))
      
      # Prüft, ob die generierte Spalte im DataFrame existiert
      if (spaltenname %in% names(df)) {
        # Transformiert das Datum, berechnet Monat und Jahr, und zählt die Anzahl der Einträge pro Monat und Gemeinde
        monatliche_zaehlung <- df %>%
          mutate(Datum = as.Date(.[[spaltenname]], format = "%Y-%m-%d"),
                 Monat = month(Datum),
                 Jahr = year(Datum)) %>%
          group_by(Gemeinden, Jahr, Monat) %>%
          summarise(Anzahl = n(), .groups = 'drop')
        
        # Speichert das Ergebnis in der Liste unter dem dynamisch generierten Spaltennamen
        monatliche_zaehlungen_liste[[spaltenname]] <- monatliche_zaehlung
      } else {
        # Gibt eine Warnung aus, falls die Spalte nicht im DataFrame existiert
        message(spaltenname, " existiert nicht im DataFrame.")
      }
    }
    # Kombiniert die monatlichen Zählungen aus allen Listen-Elementen in einen einzigen DataFrame
    gesammelte_daten <- bind_rows(monatliche_zaehlungen_liste, .id = "Bauteil")
    
    # Konvertiert 'Bauteil' zu einem lesbaren Format, indem 'Fehlerhaft_Datum_T' entfernt wird
    gesammelte_daten$Bauteil <- gsub("Fehlerhaft_Datum_T", "T", gesammelte_daten$Bauteil)
    
    # Fügt eine neue Spalte 'MonatJahr' hinzu, die das Datum im Format 'Jahr-Monat' repräsentiert
    gesammelte_daten <- gesammelte_daten %>%
      mutate(MonatJahr = paste(Jahr, sprintf("%02d", Monat), sep = "-"),
             MonatJahr = as.Date(paste0(MonatJahr, "-01"))) # Fügt einen Dummy-Tag hinzu, um ein vollständiges Datum zu erhalten
    
    # Erstellt eine leere Liste, um die Koordinaten für jede Gemeinde zu speichern
    coordinates_list <- list()
    
    # Beginnt eine Schleife, die jede einzigartige Gemeinde in der Spalte 'Gemeinden' des Dataframes 'df' durchläuft
    for (gemeinde in unique(df$Gemeinden)) {
      # Filtert den Dataframe 'df' für jede spezifische 'gemeinde' und wählt nur die Spalten 'Laengengrad' und 'Breitengrad'
      # Entfernt dabei doppelte Einträge, um eindeutige Koordinaten für die Gemeinde zu erhalten
      coordinates <- df %>%
        filter(Gemeinden == gemeinde) %>%
        select(Laengengrad, Breitengrad) %>%
        distinct()
      
      # Fügt die gefundenen Koordinaten der Gemeinde zur Liste 'coordinates_list' hinzu
      # Der Schlüssel in der Liste ist der Name der Gemeinde
      coordinates_list[[gemeinde]] <- coordinates
    }
    
    # Fügt den gesammelten Daten 'gesammelte_daten' die Koordinaten für jede Gemeinde hinzu
    # Dies geschieht durch einen Linken Join auf der Grundlage der Gemeindenamen
    # 'bind_rows' wird genutzt, um die Liste 'coordinates_list' in einen Dataframe zu verwandeln, wobei 'Gemeinden' als ID-Spalte dient
    gesammelte_daten <- gesammelte_daten %>%
      left_join(bind_rows(coordinates_list, .id = "Gemeinden"), by = "Gemeinden")
    
    # Gibt den erweiterten Dataframe 'gesammelte_daten' zurück, der jetzt auch Koordinaten enthält
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
  
  # UI für die Auswahl der Einzelteile
  output$BauteilAuswahlUI <- renderUI({
    selectInput("BauteilAuswahl", "Wähle ein Teil:", choices = unique(filteredData()$Bauteil))
  })
  
  # Reaktive Expression für gefilterte Daten (Balkendiagramm)
  filteredData2 <- reactive({
    # Holen der gefilterten Daten
    daten <- filteredData()
    
    # Filtern nach ausgewählter Gemeinde, wenn der Einzelmodus für Gemeinden aktiviert ist
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      daten <- daten %>% filter(Gemeinden == input$ortAuswahl)
    }
    
    # Filtern nach ausgewähltem Monat, wenn der Einzelmodus für Monate aktiviert ist
    if (input$auswahlModusMonat == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    
    # Rückgabe der gefilterten Daten
    return(daten)
  })
  
  # Reaktive Expression für gefilterte Daten (Ausfallverlauf)
  filteredData3 <- reactive({
    # Holen der gefilterten Daten
    daten <- filteredData()
    
    # Filtern nach ausgewählter Gemeinde, wenn der Einzelmodus für Gemeinden aktiviert ist
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      daten <- daten %>% filter(Gemeinden == input$ortAuswahl)
    }
    
    # Filtern nach ausgewähltem Monat, wenn der Einzelmodus für Monate aktiviert ist
    if (input$auswahlModusMonat == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    
    # Gruppieren und Zusammenfassen der Daten nach Monat und Bauteil, wenn der Gemeinde-Modus auf 'alle' gesetzt ist
    if (input$auswahlModusGemeinde == "alle") {
      daten <- daten %>%
        group_by(MonatJahr, Bauteil) %>%
        summarise(Anzahl = sum(Anzahl), .groups = 'drop')
    }
    
    # Rückgabe der gefilterten Daten
    return(daten)
  })
  
  # Reaktive Expression für gefilterte Daten (Datentabelle)
  filteredData4 <- reactive({
    # Holen der gefilterten Daten
    daten <- filteredData()
    
    # Filtern nach ausgewählter Gemeinde, wenn der Einzelmodus für Gemeinden aktiviert ist
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      daten <- daten %>% filter(Gemeinden == input$ortAuswahl)
    }
    
    # Filtern nach ausgewähltem Monat, wenn der Einzelmodus für Monate aktiviert ist
    if (input$auswahlModusMonat == "einzel" && !is.null(input$monatAuswahl)) {
      selectedMonth <- as.Date(paste0(input$monatAuswahl, "-01"))
      daten <- daten %>% filter(MonatJahr == selectedMonth)
    }
    
    # Rückgabe der gefilterten Daten
    return(daten)
  })
  
  # Erstellen des Balkendiagramms basierend auf der Auswahl
  output$stackedBarPlot <- renderPlot({
    #daten <- filteredDataByMonth()
    
    # Erstellen des Balkendiagramms basierend auf den gefilterten Daten
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
    
    # Filtern der Daten für das erste Quartal von 2014
    first_quarter_2014 <- filteredData() %>%
      filter(MonatJahr >= as.Date("2014-01-01") & MonatJahr <= as.Date("2014-03-31"))
    
    # Filtern der Daten für das erste Quartal von 2015
    first_quarter_2015 <- filteredData() %>%
      filter(MonatJahr >= as.Date("2015-01-01") & MonatJahr <= as.Date("2015-03-31"))
    
    # Filtern der Daten für das erste Quartal von 2016
    first_quarter_2016 <- filteredData() %>%
      filter(MonatJahr >= as.Date("2016-01-01") & MonatJahr <= as.Date("2016-03-31"))
    
    # Aggregieren der Daten nach Bauteil, um den Durchschnitt für das ausgewählte Bauteil im ersten Quartal zu erhalten
    first_quarter_aggregated <- bind_rows(first_quarter_2014, first_quarter_2015, first_quarter_2016) %>%
      filter(Bauteil == input$BauteilAuswahl) %>%
      summarise(Anzahl = mean(Anzahl))
    
    # Berechnung des Durchschnitts pro Gemeinde für das ausgewählte Bauteil
    if (input$auswahlModusGemeinde == "einzel" && !is.null(input$ortAuswahl)) {
      first_quarter_aggregated <- bind_rows(first_quarter_2014, first_quarter_2015, first_quarter_2016) %>%
        filter(Bauteil == input$BauteilAuswahl, Gemeinden == input$ortAuswahl) %>%
        summarise(Anzahl = mean(Anzahl))
    }
    
    # Rückgabe der berechneten Prognosedaten
    return(first_quarter_aggregated)
  })
  
  
  # Reaktive Funktion zur Berechnung der Prognose für die Kartenansicht
  prognosisDataMap <- reactive({
    req(filteredData())  # Stellen Sie sicher, dass filteredData verfügbar ist
    
    # Filtern der Daten für das erste Quartal von 2014
    first_quarter_2014 <- filteredData() %>%
      filter(MonatJahr >= as.Date("2014-01-01") & MonatJahr <= as.Date("2014-03-31"))
    
    # Filtern der Daten für das erste Quartal von 2015
    first_quarter_2015 <- filteredData() %>%
      filter(MonatJahr >= as.Date("2015-01-01") & MonatJahr <= as.Date("2015-03-31"))
    
    # Filtern der Daten für das erste Quartal von 2016
    first_quarter_2016 <- filteredData() %>%
      filter(MonatJahr >= as.Date("2016-01-01") & MonatJahr <= as.Date("2016-03-31"))
    
    # Auswahl des ausgewählten Bauteils
    selected_prognosis_item <- input$BauteilAuswahl
    
    # Berechnung des Durchschnitts pro Gemeinde für das ausgewählte Bauteil
    durchschnittProGemeinde <- bind_rows(first_quarter_2014, first_quarter_2015, first_quarter_2016) %>%
      filter(Bauteil == selected_prognosis_item) %>%
      group_by(Gemeinden) %>%
      summarise(DurchschnittProGemeinde = mean(Anzahl), groups = 'drop')
    
    # Rückgabe der berechneten Prognosedaten für die Kartenansicht
    return(durchschnittProGemeinde)
  })
    
  

  
  # Dynamische Anzeige des Wertfelds
  output$dynamicValueBox <- renderUI({
    # Runden der Prognosedaten auf ganze Zahlen
    value <- round(prognosisData()$Anzahl)
    
    # Erstellen des Untertitels für das Wertfeld
    subtitleText <- paste("Prognose für", input$BauteilAuswahl, "im ersten Quartal 2017")
    
    # Erstellen des HTML-Widgets für das Wertfeld
    tags$div(
      id = "custom-value-box",
      style = "padding: 20px; background-color: Lightsteelblue; color: white; border-radius: 5px; border: 2px solid #4682B4; margin-bottom: 20px; text-align: center;",
      tags$h3(value, style = "margin-top: 0;"),
      tags$p(subtitleText, style = "margin-bottom: 0;")
    )
  })
  
  # Ausfallverlauf für jedes Einzelteil von 2014-2016
  output$failureTrend <- renderPlot({
    # Holen der gefilterten Daten
    daten <- filteredData3()
    
    # Filtern nach ausgewähltem Bauteil
    daten <- daten %>% 
      filter(Bauteil == input$BauteilAuswahl) %>%
      na.omit(daten)
    
    # Festlegen der maximalen und minimalen y-Achsenwerte für den Plot
    y_max <- max(daten$Anzahl, na.rm = TRUE) * 1.4 # 10% mehr Platz oben
    y_min <- min(daten$Anzahl, na.rm = TRUE) * 0.6 # 10% Puffer unten
    
    # Erstellen des Ausfallverlaufsplots
    plot <- ggplot(daten, aes(x = MonatJahr, y = Anzahl, fill = Bauteil)) +
      geom_line() +
      geom_point() +
      labs(x = "Monat und Jahr", y = "Anzahl der Fehler", color = "Bauteil",
           title = "Ausfallverlauf von 2014 bis 2016 für jedes Einzelteil") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(y_min, y_max)  
    
    # Rückgabe des erstellten Plots
    return(plot)
  })
  
  # Erstellen der Kartenansicht
  output$map <- renderLeaflet({
    # Verknüpfen der Prognosedaten mit den Geodaten der Gemeinden
    popup_data <- left_join(gemeinden_geodaten,prognosisDataMap(), by = "Gemeinden")
    
    # Erstellen der Leaflet-Karte mit den vorbereiteten Daten
    leaflet(popup_data) %>%
      setView(lng = 10.4515, lat = 51.1657, zoom = 6) %>%
      addTiles() %>%
      addMarkers(lng = ~Laengengrad, lat = ~Breitengrad, popup = ~paste(Gemeinden, ": ", round(DurchschnittProGemeinde)))
  })
  
  # Datentabelle
  output$dataTable <- renderDT({
    # Auswahl der gewünschten Spalten
    filtered_data_selected <- filteredData4() %>%
      select(Bauteil, Gemeinden, Jahr, Monat, Anzahl)
    
    # Anzeigen der gefilterten und ausgewählten Daten als Datentabelle
    datatable(filtered_data_selected)
  })
}

# App ausführen
shinyApp(ui, server = server)
