library(shiny)
library(leaflet)

ui <- fluidPage(
  
    leafletOutput(
      "map", width = "100%", height = 600
    ),
    
    absolutePanel(
      
      top = 20,
      right = 20,
      width = 300,
      fixed = TRUE,
      wellPanel(
        selectInput(
          "competition",
          "Filter by Competition",
          c(
            "Select Competition",
            "Champion's League (European Cup)",
            "World Cup"
          ),
          selected = 2
        ),
        selectInput("team",
                    "Filter by Team",
                    c("Select Team",
                      list(
                        paste(uclWinners$Year, uclWinners$Team)
                      )[[1]])),
        selectInput("year",
                    "Filter from earliest year",
                    c("Select Year", 1956:2018)),
        radioButtons(
          "filter",
          "Filter data by:",
          choices = c(
            "Show All",
            "Winning Team",
            "Club",
            "Earliest Year",
            "Individual Player"
          ),
          selected = "Show All"
        ),
        textOutput("test"),
        style = "opacity: 0.9"
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
  
  # locs.exp <- reactive({
  # 
  #   # Get total players born in each location
  #   selectedYear <- input$year
  #   selectedComp <- input$competition
  #   
  #   # Filter players by selected criteria
  #   if ("Team" %in% input$filter){
  #     fPlayers <- Players[grep(input$club, Players$Years), ]
  #   
  #   } else {
  #     fPlayers <- Players
  #   }
  #   
  #   locs <- Locations
  #   locs$val <- sapply(1:nrow(locs), function(x){
  #     length(which(fPlayers$Birthplace == locs$Location[x]))
  #   })
  #   
  #   locs.exp <- locs[rep(row.names(locs), locs$val),]
  #   
  # })
  
  output$map <- renderLeaflet({
    
    # Get total players born in each location
    selectedYear <- input$year
    selectedComp <- input$competition
    
    # Filter players by selected criteria
    if (input$filter == "Winning Team"){
      fPlayers <- Players[grep(gsub("\\D", "", input$team), Players$Years), ]
      
    } else {
      fPlayers <- Players
    }
    
    locs <- Locations
    locs$val <- sapply(1:nrow(locs), function(x){
      length(which(fPlayers$Birthplace == locs$Location[x]))
    })
    
    locs.exp <- locs[rep(row.names(locs), locs$val),]
    
    leaflet(data = locs.exp) %>%
      addTiles() %>%
      flyTo(lat = 0, lng = 0, zoom = 2) %>%
      # addAwesomeMarkers(
      #   lat = ~ lon,
      #   lng = ~ lat,
      #   label = ~ Location,
      #   popup = ~ paste(sep = "<br/>",
      #                   Location,
      #                   ucl),
      #   icon = ~ makeAwesomeIcon(
      #     text = val,
      #     iconColor = 'black',
      #     library = 'fa',
      #     markerColor = 'blue'
      #   ),
      #   clusterOptions = markerClusterOptions(maxClusterRadius = 60,
      #                                         showCoverageOnHover = F)
      # )
    addCircleMarkers(
      lat = ~ lon,
      lng = ~ lat,
      label = ~ Location,
      #labelOptions = labelOptions(noHide = TRUE,
      #                            offset = c(0, -10),
      #                            textOnly = TRUE),
      popup = ~ paste( sep = "<br/>",
                       Location,
                       ucl),
      radius = 5,
      fillOpacity = 0.8,
      color = col[cols2],
      stroke = T,
      clusterOptions = markerClusterOptions(maxClusterRadius = 60,
                                            showCoverageOnHover = F)
    )
    
  })
  
}
shinyApp(ui = ui, server = server)