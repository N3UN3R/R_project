"""
Entwickeln Sie eine Shiny-App welche folgende Kriterien erfüllt:
  
  Alle verwendeten Packages müssen auf der aktuellen R-Version lauffähig sein.

Die Applikation muss ohne weitere Anpassungen aus dem Abgabeordner gestartet werden können. Es bietet sich an, dies vor Abgabe zu testen. Die Applikation soll sich nur auf einen einzelnen, von Ihnen erstellten Datensatz beziehen.

Das Layout der Applikation soll auf die Zielgruppe angepasst sein und der Unternehmensfarbe entsprechen. Die Farbe ihres Unternehmens ist Lightsteelblue. Des Weiteren soll ein Logo in das Layout integriert werden. Es kann ein eigenes Logo erstellt werden, oder das Logo vom Fachgebiet Qualitätswissenschaft verwendet werden. Passen Sie die Schriftart ihrer Applikation nach Ihren wünschen an. Eine Anpassung ist obligatorisch.

Visualisieren Sie folgendes in der Applikation

Ein Balkendiagramm, in dem für jeden Monat die Anzahl der Ausfälle für jedes Teil gestapelt wird. Dabei soll nach Orten gefiltert werden können.

Den Ausfallverlauf und eine Prognose für den Ausfall eines auswählbaren Teils für das 1. Quartal 2017. Sowohl der Gesamtausfall, als auch der Ausfall je oben genannter Stadt, soll aus der Darstellung hervorgehen.

Eine interaktive Karte, auf der alle Orte markiert sind und integrieren Sie Pop-Ups, aus der Ihre empfohlenen Stückzahlen für das aus b. ausgewählte Teil ersichtlich sind.

Ihren zugrundeliegenden Datensatz als Tabelle, damit Sie Visualisiertes auch beweisen können. Denken Sie auch hier daran, nur die notwendigen Attribute anzuzeigen.
"""


# Überprüfen, ob das 'install.load' Paket installiert ist, und es bei Bedarf installieren. Dieses Paket hilft beim Verwalten der Installation und beim Laden anderer Pakete.
if(!require("install.load")){
  install.packages("install.load")
}
library(install.load)

# Mit der Funktion 'install_load' aus dem 'install.load' Paket mehrere Bibliotheken installieren und laden, die in dieser App verwendet werden. 
# Dazu gehören Pakete für Datenmanipulation, Erstellung von Diagrammen und interaktive UI-Elemente in der Shiny-App.
install_load("readr", "shiny", "leaflet", "htmltools", "ggplot2", 
             "shinythemes", "shinyWidgets", "ggthemes", "tidyverse", "data.table")

# Laden des Datensatzes in 'final_data'. Passen Sie den Pfad an den Speicherort Ihrer CSV-Datei an.
final_data <- read.csv("Final_dataset_group_07.csv")

# Definition der Benutzeroberfläche (UI) für die Anwendung
ui <- fluidPage(
  
  # Externes CSS für zusätzliches Styling einbinden (z.B. Font Awesome Icons für die Checkbox-Gruppe)
  tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")),
  
  # Titel der Anwendung, der oben angezeigt wird
  titlePanel("Einzelteilbestellungsprognose"),
  
  # Benutzerdefiniertes CSS, um den Körper und die Tabs der App zu stylen
  tags$head(
    tags$style(
      HTML("
        body {
          height: 100%;
          background-color: Lightsteelblue;
        }
        
        /*Ändern der Farbe des Tab-Textes*/
        .nav-tabs>li>a {
          color: black;
        }
      ")
    )
  ),
  
  # Seitenleistenlayout für Eingabesteuerelemente und Hauptpanel zur Anzeige von Ausgaben
  sidebarLayout(
    sidebarPanel(
      
      # Dropdown zur Auswahl des App-Modus mit vordefinierten Optionen
      selectInput("select_mode", label = "App-Modus", 
                  choices = list("Beispielmodus (n = 100.000)" = 1, "Normalmodus" = 2), 
                  selected = 1),
      
      # Eingabe für das Zensierungsdatum der Analyse
      dateInput("censoring_date", "Zensierungsdatum der Analyse", value = max(final_data$earliest_failure_date)), 
      
      # Eingabe für den Produktionszeitraum der Fahrzeuge
      dateRangeInput("production_period", "Produktionszeitraum der Fahrzeuge", start = min(final_data$vehicle_production_date), end = max(final_data$vehicle_production_date)),
      
      # Checkbox-Gruppe für die Fahrzeugauswahl
      checkboxGroupButtons(
        inputId = "selected_vehicle_type",
        label = "Wählen Sie den Fahrzeugtyp",
        choices = c("Typ 11" = "11", "Typ 12" = "12"), 
        selected = c("11", "12"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", style = "color: Lightsteelblue"),
          no = tags$i(class = "fa fa-circle-o", style = "color: Lightsteelblue")
        )
      ),
      # Ein Bild hinzufügen
      img(src = "URL_zum_Bild",
          height = 200, width = 200, align = "left", style = "padding-top: 30px;")
    ),
    
    # Hauptpanel
    mainPanel(
      # Hier werden die Ausgaben angezeigt: Eine Karte, zwei Arten von Boxplots und eine Datentabelle, die den zugrundeliegenden Datensatz anzeigt
    )
  )
  
)


# Definition der Serverlogik der Shiny-Anwendung
server <- function(input, output) {
  
  # Anpassen der Daten basierend auf den vom Benutzer ausgewählten Werten
  selected_data <- reactive({
    
    # Prüfung des Modus: Normalmodus oder Beispielmodus mit 100.000 Datensätzen
    if(input$select_mode == 2) {
      # Normalmodus: Verwenden aller Daten
      final_data %>%
        mutate(earliest_failure_date = as.numeric(as.Date(earliest_failure_date))) %>% # Umwandlung des ersten Ausfalldatums in numerische Werte
        replace_na(list(earliest_failure_date = 0)) %>% # Ersetzen von NA-Werten im ersten Ausfalldatum mit 0
        filter(vehicle_production_date >= input$production_period[1]) %>% # Filtern nach Start des Produktionszeitraums
        filter(vehicle_production_date <= input$production_period[2]) %>% # Filtern nach Ende des Produktionszeitraums
        filter(earliest_failure_date <= as.numeric(input$censoring_date)) %>% # Filtern nach Zensierungsdatum
        filter(vehicle_type %in% input$selected_vehicle_type) %>% # Filtern nach ausgewähltem Fahrzeugtyp
        mutate(earliest_failure_date = replace(earliest_failure_date, earliest_failure_date == 0, NA)) %>% # Zurückwandeln der 0 in NA für das erste Ausfalldatum
        mutate(earliest_failure_date = as.Date(earliest_failure_date, origin = "1970-01-01")) # Umwandlung des ersten Ausfalldatums von numerisch zurück in Datumswerte
    } else {
      # Beispielmodus: Verwendung einer Stichprobe von 100.000 Datensätzen
      final_data %>%
        sample_n(100000) %>%
        # Die folgenden Schritte sind identisch mit dem Normalmodus
        mutate(earliest_failure_date = as.numeric(as.Date(earliest_failure_date))) %>%
        replace_na(list(earliest_failure_date = 0)) %>%
        filter(vehicle_production_date >= input$production_period[1]) %>%
        filter(vehicle_production_date <= input$production_period[2]) %>%
        filter(earliest_failure_date <= as.numeric(input$censoring_date)) %>%
        filter(vehicle_type %in% input$selected_vehicle_type) %>%
        mutate(earliest_failure_date = replace(earliest_failure_date, earliest_failure_date == 0, NA)) %>%
        mutate(earliest_failure_date = as.Date(earliest_failure_date, origin = "1970-01-01"))        
    }
  })
  
  # Erstellung einer reaktiven Ausgabe für die Kartendarstellung, abhängig von der Benutzerauswahl
  data_map <- reactive({
    if (input$map_selection == "b") {
      # Berechnung der relativen Anzahl der Feldausfälle im Verhältnis zum Produktionsvolumen
      group_by(selected_data(), location) %>% 
        summarise(total = sum(field_failure)/n(), latitude, longitude) %>% 
        distinct() 
    } else {
      # Berechnung der Produktionsmengen
      group_by(selected_data(), location) %>% 
        summarise(total = n(), latitude, longitude) %>% 
        distinct() 
    }
  })
  
  # Faktor für die Größenanpassung der Kreismarker auf der Karte
  factor <- reactive({
    if (input$map_selection == "b")
    {
      # Kleinere Größe für die Darstellung relativer Zahlen
      0.01 
    } else {
      # Größere Größe für die Darstellung der Produktionsmengen, abhängig vom Modus
      if (input$select_mode == 1) {
        1500  
      } else {
        30000 
      }
    }
  })
  
  ## Kartendarstellung
  output$map <- renderLeaflet({
    data_map() %>%
      leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>%
      # Hinzufügen von Kreismarkern, deren Größe relativ zur Gesamtzahl der Autos oder Feldausfälle ist
      addCircleMarkers(lat = ~latitude, lng = ~longitude, label = ~total, radius = ~total/factor())
  })
  
  # Erstellung des Boxplots aus den ausgewählten Daten für die Zeit bis zum ersten Ausfall
  output$plot <- renderPlot({
    ggplot(selected_data(), aes(as.factor(vehicle_type), time_till_first_failure, fill = as.factor(vehicle_type))) +
      geom_boxplot(na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 800)) +
      labs(x = "Fahrzeugtyp", y = "Zeit bis zum ersten Ausfall [Tage]") +
      ggtitle("Zeit bis zum ersten Ausfall", subtitle = "Boxplot der Zeitspanne zwischen Produktionsdatum und Datum des ersten Ausfalls") +
      theme_clean() +
      theme(legend.position="none") 
  })
  
  # Erstellung des Boxplots aus den ausgewählten Daten für die Lebensdauer der Fahrzeuge
  output$plot2 <- renderPlot({
    ggplot(selected_data(), aes(as.factor(vehicle_type), vehicle_lifespan, fill = as.factor(vehicle_type))) +
      geom_boxplot(na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 600)) +
      labs(x = "Fahrzeugtyp", y = "Lebensdauer [Tage]") +
      ggtitle("Fahrzeuglebensdauer", subtitle = "Boxplot der Zeitspanne zwischen Produktionsdatum und Datum des Fahrzeugausfalls") +
      theme_clean() +
      theme(legend.position="none") 
  })
  
  # Erstellung der Tabelle zur Anzeige der zugrundeliegenden Daten
  output$table <- DT::renderDT({
    DT::datatable(selected_data(), colnames = c("Fahrzeug-ID" = "id_vehicle", "Fahrzeugtyp" = "vehicle_type", "Herstellerwerk" = "plant", "Standort" = "location", "Breitengrad" = "latitude", "Längengrad" = "longitude", "Produktionsdatum" = "vehicle_production_date", "Fahrzeugausfall" = "vehicle_failure", "Datum des Fahrzeugausfalls" = "vehicle_failure_date", "Lebensdauer des Fahrzeugs" = "vehicle_lifespan", "Feldausfall" = "field_failure", "Frühestes Ausfalldatum" = "earliest_failure_date", "Zeit bis zum ersten Ausfall" = "time_till_first_failure")) 
  })
}


# Starten der Anwendung
shinyApp(ui = ui, server = server)
