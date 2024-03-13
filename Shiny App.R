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


# Installing all required packages
if(!require("install.load")){
  install.packages("install.load")
}
library(install.load)


install_load("readr", "shiny", "leaflet", "htmltools", "ggplot2", "shinythemes", "shinyWidgets", "ggthemes", "tidyverse", "data.table") 
#install_load("readr", "shiny", "leaflet", "htmltools", "dplyr", "ggplot2", "shinythemes", "shinyWidgets", "ggthemes" )


#load the data
#---->>>>>>>final_data <- read.csv("Final_dataset_group_32.csv")<<<<<<<<<<------------
#final_data <- final_data %>% sample_n(100000)

# Define UI for application
ui <- fluidPage(
  
  # load the font awesome library (necessary for the checkbox group)
  tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")),             
  
  # Application title
  titlePanel("Production volumes and field failures"),
  
  tags$head(
    tags$style(
      HTML("
        body {
          height: 100%;
          background-color: Lightsteelblue;
        }
        
        /*change the color of the tab text*/
        .nav-tabs>li>a {
          color: black;
        }
      ")
    )
  ),
  
  # create the sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      # Copy the line below to make a select box 
      selectInput("select_mode", label = "App mode", 
                  choices = list("Sample mode (n = 100.000)" = 1, "Normal mode" = 2), 
                  selected = 1),
      
      #hr(),
      #fluidRow(column(3, verbatimTextOutput("value")))
      
      #input for censoring date
      dateInput("censoring_date", "Censoring date of the analysis", value = max(final_data$earliest_failure_date )), 
      
      #input for production period
      dateRangeInput("production_period", "Production period of the vehicles", start = min(final_data$vehicle_production_date), max(final_data$vehicle_production_date)),
      
      # create the checkbox group for the car selection
      checkboxGroupButtons(
        
        inputId = "selected_vehicle_type",
        label = "Choose the vehicle type",
        choices = c("Type 11" = "11","Type 12" = "12"), 
        selected = c("11", "12"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", style = "color: Lightsteelblue"),
          no = tags$i(class = "fa fa-circle-o", style = "color: Lightsteelblue")
        )
      ),
      img(src = "https://media.licdn.com/dms/image/C4E0BAQHliuj-kkUZ0g/company-logo_200_200/0/1611768200813?e=1686787200&v=beta&t=4mmNNxQ2OmYnTBlisQ4a2BAqk7_F925U9gSGOHuYmgU",
          height = 200, width = 200, align = "left", style = "padding-top: 30px;")
    ),
    
    # Main panel
    mainPanel(
      
      
      ##OUTPUT HERE
      tabsetPanel(
        
        #Display the map
        tabPanel("Map",
                 leafletOutput("map"),
                 absolutePanel(
                   top = 250, left = 20,
                   dropdownButton(
                     selectInput(inputId = 'map_selection',
                                 label = 'select which data to show',
                                 choices = c("Production quantities"="a", "Relative number of field failures in relation to production volume"="b"),
                                 selected = "a"
                     ),
                     circle = TRUE, 
                     status = "danger",
                     icon = icon("cog"), 
                     width = "300px",
                     tooltip = tooltipOptions(title = "Click to change the display!")
                   )
                 )
                 
        )
        
        ,
        
        #Display the plot
        tabPanel("Boxplot - Time until first Failure",   
                 plotOutput("plot")),
        
        #Display the plot
        tabPanel("Boxplot - Vehicle Lifespan",   
                 plotOutput("plot2")),
        
        #Display the underlying dataset
        tabPanel("Underlying dataset",
                 DT::DTOutput("table"))
      )
    )
  )
  
)