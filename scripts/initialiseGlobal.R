# Initialise global variables
initialisePlayersLocations <- function(){
Players <- data.frame(Name = character(), 
                      Birthplace = character(), 
                      Index = integer(),
                      stringsAsFactors = F
                      )

Locations <- data.frame(Location = character(),
                        City = character(),
                        Country = factor(),
                        lat = numeric(),
                        lon = numeric(),
                        stringsAsFactors = F)
}

  # Add unique players and new competition
updatePlayers <- function(currPlayers, comp){
  Players <<- rbind(Players, currPlayers[which(!(currPlayers$Name %in% Players$Name)), 
                                         1:3])
  if (!(comp %in% colnames(Players))){
    Players$new <<- 0
    Players$new[which(currPlayers$Name %in% Players$Name)] <<- 1
    names(Players)[ncol(Players)] <<- comp
    Players$yrs <<- ""
    Players$yrs[which(currPlayers$Name %in% Players$Name)] <<-
      currPlayers$Years
    names(Players)[ncol(Players)] <<- paste0(comp, "years")
  }
}

# Add unique locations 
updateLocations <- function(currLocations){
  Locations <<- rbind(Locations, 
                      currLocations[which(!(currLocations$Location %in% Locations$Location)), ])
}
