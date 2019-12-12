library(shiny)
library(shinydashboard)
library(maps)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(shinyjs)
library(devtools)
library(shinythemes)
library(foreign)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(shinyalert)
library(rdrop2)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)
library(janitor)
library(ggplot2)
library(gganimate)
library(tidyverse)
library(gifski)
library(mapview)

Logged = FALSE
my_username <- "TCG"
my_password <- "Evansville"

ui <- fluidPage(
  titlePanel(
    fluidRow( 
      column(4, "Crash Data Demo - Made up data")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons("DataType", "Display data:", c("Clustered","Indivual data")),
      hr(),
      h5("Filter data by the following options:"),
      dateRangeInput("AccDate", "Accident Date Range:", start = "2012-01-01", end = "2018-01-01", format = "mm/dd/yyyy",
                     separator = " - "),
      hr(),
      pickerInput("InjuryStat", "Injury Status:", choices = c("INCAPACITATING","FATAL"), selected = c("INCAPACITATING","FATAL"),
                  options = list(`actions-box` = TRUE, 'live-Search' = TRUE),
                  multiple = TRUE),
      width = 3
    ),
    
    mainPanel (
      # tags$iframe(
      #   style = "height:400px; width:100%; scrolling=yes",
      #   src = "/2012/Pedestrian/901755077_diagram.pdf"
      # ),
      # hr(),
      leafletOutput("map", width = "100%", height = "750") %>% withSpinner(color="#0dc5c1", size = 0.5),
      hr(),
      uiOutput("pdfview")
      
    )
  )
)  

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    data <- data.frame(read.csv(paste(getwd(), "/Input/AllAccidents_2012-2017_Crash_Cleanup_FINAL.csv", sep = ""),
                                header = TRUE, sep = ","), stringsAsFactors = FALSE)
    data <- subset(data, as.Date(data$Colison_Da) >= input$AccDate[1] & as.Date(data$Colison_Da) < input$AccDate[2])
    
    data <- subset(data, data$Injury_Sta == input$InjuryStat[1] | data$Injury_Sta == input$InjuryStat[2])
    
    PedCyc <- subset(data, data$CrashType == "Pedalcycle")
    
    Ped <- subset(data, data$CrashType == "Pedestrian")
    
    Veh <- subset(data, data$CrashType == "Vehicle")
    
    data$CrVl <- cut(data$CrashValue,
                     breaks = c(1,2,3,4), right = FALSE,
                     labels = c("Pedalcycle", "Pedestrian", "Vehicle"))
    pal = colorFactor(palette = c("blue", "red", "black"), domain = data$CrVl)
    
    data %>% split(data$CrashType) -> data.df
    
    
    l <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) #%>% setView(lat = 39.764075, lng = -86.159019, zoom = 7)
    
    if(input$DataType == "Clustered"){
      names(data.df) %>%
        purrr::walk( function(df) {
          l <<- l %>% 
            addCircleMarkers(data = data.df[[df]], lng = ~Long, lat = ~Lat, weight = 1,
                             color= ~ pal(CrVl), fillOpacity = 0.8, radius = 5, group = df,
                             stroke = FALSE,
                             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE),
                             popup = paste( "Crash Type: ", data.df[[df]]$CrashType, "<br>",
                                            "WeatherCondition: ", data.df[[df]]$Weather_Co))
        })
    }
    else{
      names(data.df) %>%
        purrr::walk( function(df) {
          l <<- l %>% 
            addCircleMarkers(data = data.df[[df]], lng = ~Long, lat = ~Lat, weight = 1,
                             color= ~ pal(CrVl), fillOpacity = 0.8, radius = 5, group = df,
                             stroke = FALSE,
                             popup = paste( "Crash Type: ", data.df[[df]]$CrashType, "<br>",
                                            "WeatherCondition: ", data.df[[df]]$Weather_Co))
        })
    }
    
    l %>%
      addLayersControl(
        overlayGroups = names(data.df), position = "topright",
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend("bottomleft", title = "Legend", colors = c("blue", "red", "black"), 
                labels = c(paste("Pedalcycle (", nrow(PedCyc), ")", sep = ""),
                           paste("Pedestrian (", nrow(Ped), ")", sep = ""),
                           paste("Vehicle (", nrow(Veh), ")", sep = "")))
  })
  
  values <- reactiveValues(authenticated = FALSE)

  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  obs1 <- observe({
    showModal(dataModal())
  })

  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()

      } else {
        values$authenticated <- FALSE
      }
    }
  })


  output$dataInfo <- renderPrint({
    if (values$authenticated) "OK!!!!!"
    else "You are NOT authenticated"
  })
  
}

shinyApp(ui, server)
