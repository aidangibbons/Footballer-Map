library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("map", width = "100%", height = 600),
  
  absolutePanel(
    top = 20,
    right = 20,
    width = 300,
    fixed = TRUE,
    tabsetPanel(
      tabPanel(
        "Filters",
        wellPanel(
          ### Options to be developed
          # selectInput(
          #   "competition",
          #   "Filter by Competition",
          #   c(
          #     "Select Competition",
          #     "Champion's League (European Cup)",
          #     "World Cup"
          #   ),
          #   selected = 2
          # ),
          
          # selectInput("year",
          #             "Filter from earliest year",
          #             c("Select Year", 1956:2018)),
          
          # Filter data by an individual competition-winning team
          selectInput("team",
                      "Filter by Team",
                      c("Select Team",
                        list(
                          paste(uclWinners$Year, uclWinners$Team)
                        )[[1]])),
          
          # Search for a play in the dataset
          # Hover text informing about players with accents in their name
          tags$div(
            title = "Some players may not appear due to accents in their name, try various options",
            textInput("playersearch",
                      "Search for an individual player")
          ),
          
          radioButtons(
            "filter",
            "Filter data by:",
            choices = c("Show All",
                        "Winning Team",
                        #"Club",
                        #"Earliest Year",
                        "Individual Player"),
            selected = "Show All"
          ),
          "Avoid changing values and settings too quickly as the map has to refresh each time",
          style = "opacity: 0.9"
        )
      ),
      tabPanel(
        "Map Options",
        wellPanel(
          sliderInput("maxzoom",
                      "Zoom level at which to stop clustering",
                      2,
                      17,
                      7),
          sliderInput(
            "clusterradius",
            "Maximum pixel distance for points sharing a cluster",
            2,
            200,
            56
          ),
          "Avoid changing values and settings too quickly as the map has to refresh each time",
          style = "opacity : 0.9"
        )
      )
    )
  )
  
  
)

server <- function(input, output) {
  selectedComp <- reactive({
    input$competition
  })
  
  selectedTeam <- reactive({
    input$team
  })
  
  selectedYear <- reactive({
    input$year
  })
  
  output$test <- renderText({
    input$filter
  })
  
  ### Make locs.exp reactive?
  
  output$map <- renderLeaflet({
    fPlayers <- Players
    # Filter players by selected criteria
    if (input$filter == "Winning Team") {
      fPlayers <-
        fPlayers[grep(gsub("\\D", "", input$team), fPlayers$uclyears),] # Find only winners from a specific year
    }##### uclyears won't work for new competitions
    
    # Search for player in the dataset
    else if (input$filter == "Individual Player" &&
             input$playersearch != "") {
      if (any(grep(input$playersearch, fPlayers$Name))) {
        fPlayers <-
          fPlayers[grep(input$playersearch, fPlayers$Name, ignore.case = T),]
        
        # If none found, reset to the Players dataset
      } else {
        fplayers <- Players
      }
    }
    
    locs <- Locations[unique(fPlayers$Index),]
    
    # Get associated player text for each location
    locs$ucl <- sapply(1:nrow(locs), function(x) {
      paste(collapse = "<br/>",
            fPlayers$Name[fPlayers$Birthplace == locs$Location[x]])
    })
    
    # Get the frequency of each location within fPlayers, including zero values
    locs$val = as.vector(table(factor(fPlayers$Index, levels = row.names(locs))))
    
    locs.exp <- locs[rep(row.names(locs), locs$val),]
    
    # Create palette for colours based off the countries
    cols <-
      heat.colors(length(levels(locs.exp$Country)), alpha = NULL)
    
    
    leaflet(data = locs.exp) %>%
      addTiles() %>%
      flyTo(lat = 10,
            lng = 50,
            zoom = 2) %>%
      
      addCircleMarkers(
        lat = ~ lon,
        lng = ~ lat,
        label = ~ City,
        popup = ~ paste(sep = "<br/>",
                        City,
                        ucl),
        radius = ~ (5 + sqrt(val)),
        fillOpacity = 0.8,
        color = ~ cols[Country],
        stroke = T,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = input$clusterradius,
          showCoverageOnHover = F,
          disableClusteringAtZoom = input$maxzoom
        )
      )
    
  })
  
}
# #let's add some color!
# pdf$Study <- factor(rep(1:10,10))
# cols <- rainbow(length(levels(factor(locs.exp$Country))))
# pdf$colors <- cols[unclass(pdf$Study)]

shinyApp(ui = ui, server = server)