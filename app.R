'###############################################################################
#   World Travel                     ###########################################
#   Shiny App                        ###########################################
################################################################################'


library(shiny)
library(dplyr)
library(leaflet)
library(shinythemes)


# Define UI for application
ui <- fluidPage(theme = shinytheme("darkly"),
   
  titlePanel(title=div(img(src="me.jpg", height=200),"  World Travel")),
  sidebarLayout(
    sidebarPanel(width = 3,
      uiOutput("yearlist"),
      uiOutput("maplist"),
      tableOutput("top_countries")


    ),
    mainPanel(
      verbatimTextOutput("Level_Select"),
      leafletOutput('myMap', height = 700, 
                    width = 1000)
      )
   ),
  fluidRow(column(width = 2, paste0('Last update: ', Sys.Date())))
)

# Define server (Output objects)
server <- function(input, output) {
   
  # load data
  travel <- read.csv("data/output/travel_coords.csv", header = TRUE, sep = ';')
  travel$year <- as.numeric(substr(travel$inbound,1,4))
  travel$Lng <- as.numeric(gsub(',', '.', travel$lng))
  travel$Lat <- as.numeric(gsub(',', '.', travel$lat))
  
  # Input
  output$yearlist <- renderUI({
    years <- c('All', sort(unique(travel$year), decreasing = T))
    selectizeInput("yearchoose", "Year:", years)
  })
  
  output$maplist <- renderUI({
    maps <- c("Esri.WorldStreetMap", 
              "Esri.WorldImagery",
              "Stamen.TonerLite")
    selectizeInput("mapchoose", "Map Style:", maps)
  })
  
  
  # Country
  top_countries <- reactive({

    # Enable Filters
    req(input$yearchoose)

    # Filter By Year (Interactive Input)
    if(input$yearchoose == "All") {cond <- quote(year != "@?><")}
    else {cond <- quote(year == input$yearchoose)}
    travel <- travel %>% filter_(cond)

    # Group by and get Top 10
    top_countries <- travel %>% group_by(country) %>% 
      summarize(n = n(), days = sum(days)) %>% data.frame() %>%
      arrange(-n,-days) %>% select(country) %>% 
      rename('Top Countries:' = country) %>% head(15)


  })

  output$top_countries <- renderTable(top_countries())
  
  
  # Reactive Map
  map <- reactive({
    
    # Enable Filters
    req(input$yearchoose)
    req(input$mapchoose)
    
    # Filter By Year (Interactive Input)
    if(input$yearchoose == "All") {cond <- quote(year != "@?><")} 
    else {cond <- quote(year == input$yearchoose)}
    travel <- travel %>% filter_(cond)
    
    # Group By Days
    travel <- travel %>% group_by(place, country, Lat, Lng) %>%
                         summarize(days = sum(days))
    
    # Build map
    m <- leaflet(data = travel) %>% addProviderTiles(input$mapchoose, 
                                                     options = providerTileOptions(minZoom = 2, maxZoom = 12)) %>% 
      addCircleMarkers(color = "red", lng = travel$Lng, lat = travel$Lat, 
                       radius = 5 + (travel$days/3), 
                       stroke = FALSE, fillOpacity = 0.5, 
                       popup = paste(travel$place, ": ", as.character(travel$days), " days", sep=' '))
  })
  
  output$myMap = renderLeaflet(map())
  
}

# Run the application 
shinyApp(ui = ui, server = server)
