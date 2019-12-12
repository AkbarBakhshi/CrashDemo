library(shiny)

library(leaflet)



ui <- bootstrapPage(

  leafletOutput("map")
  
)


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
      
  })
  
  
}

shinyApp(ui = ui, server = server)
