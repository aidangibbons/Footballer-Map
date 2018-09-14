# Plot map with player data
#library(leaflet)
{
  
  # Get total players born in each location
  dat <- Locations
  dat$val <- sapply(1:nrow(dat), function(x){
    length(which(Players$Birthplace == dat$Location[x]))
  })
  dat.exp <- dat[rep(row.names(dat), dat$val),]
  
  # define icons for the markers
  icons <- makeAwesomeIcon(
    text = '1',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'blue'
  )
  
  # create the map
  map <- leaflet(data = dat.exp) %>%
    addTiles() %>%
    addAwesomeMarkers(
      lat = ~ lon,
      lng = ~ lat,
      label = ~ Location,
      popup = ~ paste(sep = "<br/>",
                    Location,
                    ucl),
      icon = ~ makeAwesomeIcon(
        text = val,
        iconColor = 'black',
        library = 'fa',
        markerColor = 'blue'
      ),
      clusterOptions = markerClusterOptions(maxClusterRadius = 45,
                                            showCoverageOnHover = F)
    )
  map
}
