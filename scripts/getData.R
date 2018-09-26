#### Development file for getData

# Import player data

library(XML)
library(httr)
library(stringr)
library(ggmap) # development version to allow google API key to be used
#register_google(key = "AIzaSyAPq-ALJ1RVQHNziP2kPaFLG8Jfb4esYuY")

getUCLData <- function(){
  # Original data import ----
  urlUCL <- "https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_winning_players"
  dat <- GET(url = urlUCL)
  doc <- readHTMLTable(
    doc = content(dat, "text"), 
    StringsAsFactors = F)
  
  uclData <- doc[2][[1]]
  uclData[] <- lapply(uclData, as.character)
  colnames(uclData) <- uclData[1, ]
  uclData <- uclData[-1 ,]
  
  uclData$Nationality <- as.factor(uclData$Nationality)
  uclData$`Titles won` <- as.numeric(uclData$`Titles won`)
  
  # Get each player's URL information ----
  hrefFun <- function(x){ xpathSApply(x,"./a/@href") } 
  table2 <- readHTMLTable(content(dat, "text"), elFun = hrefFun, stringsAsFactors = FALSE)
  
  df2 <- table2[2][[1]]
  uclData$URLs <- df2$V1[-1]
  
  # Create and populate uclPlayers ----
  uclPlayers <- data.frame(Name = uclData$Player, Years = uclData$Years, stringsAsFactors = F)
  
  # Get UCL Player birthplaces from URLs
  i <- 1
  
  uclPlayers$Birthplace <- sapply(uclData$URLs, FUN = function(x) {
    if(i %% 50 == 0){print(i)}
    i <<- i + 1
    urlUCLPlayer <- paste0("https://en.wikipedia.org", x)
    datUCLP <- GET(url = urlUCLPlayer)
    docUCLP <- readHTMLTable(doc = content(datUCLP, "text"), StringsAsFactors = F)
    indPOB <- which(as.character(docUCLP[1][[1]]$V1[1:10]) == "Place of birth")
    as.character(docUCLP[1][[1]]$V2[indPOB])
  })
  
  # Get broken locations
  brokenLocs <- c()
  for(i in 1:793){
    if (identical(uclPlayers$Birthplace[[i]], character(0))){
      brokenLocs <- c(brokenLocs, i)
    }
  }
  
  # Fix broken locations with 'lack of citation' topper
  i <- 1
  uclPlayers$Birthplace[brokenLocs] <- sapply(uclData$URLs[brokenLocs], FUN = function(x) {
    if(i %% 50 == 0){print(i)}
    i <<- i + 1
    urlUCLPlayer <- paste0("https://en.wikipedia.org", x)
    datUCLP <- GET(url = urlUCLPlayer)
    docUCLP <- readHTMLTable(doc = content(datUCLP, "text"), StringsAsFactors = F)
    indPOB <- which(as.character(docUCLP[2][[1]]$V1[1:10]) == "Place of birth")
    as.character(docUCLP[2][[1]]$V2[indPOB])
  })
  
  # Get broken locations
  brokenLocs <- c()
  for(i in 1:793){
    if (identical(uclPlayers$Birthplace[[i]], character(0))){
      brokenLocs <- c(brokenLocs, i)
    }
  }
  
  # Remaining locations with varying issues -- Entered manually
  importBrokenUCLLocs()
  
  # Final broken location check
  brokenLocs <- c()
  for(i in 1:793){
    if (identical(uclPlayers$Birthplace[[i]], character(0))){
      brokenLocs <- c(brokenLocs, i)
    }
  }
  # Convert locations column into a vector rather than a list
  names(uclPlayers$Birthplace) <- NULL
  uclPlayers$Birthplace <- unlist(uclPlayers$Birthplace)
  
  
  # store variable before editing
  saveUCLP <- uclPlayers
  #### Manually change player birthplaces here
  
  # Regex to remove all wikipedia references (e.g [2], [nb 1]) from ALL of uclPlayers' columns
  uclPlayers$Name <- gsub("(\\[.*\\])", "",
                     uclPlayers$Name)
  uclPlayers$Birthplace <- gsub("(\\[.*\\])", "",
                     uclPlayers$Birthplace)
  ## Regex commands to remove by a set of given patterns:
  patterns <- paste(c("Kingdom of ", # Remove kingdom from certain italy/yugoslavia references)
                      "(SFR|FPR) " # Remove Yugoslavian prefixes),
                      ),
                    collapse = "|") # combine all patterns with OR character
                    
  # Apply all within patterns vector to the birthplaces
  uclPlayers$Birthplace <- gsub(patterns, "",
                                uclPlayers$Birthplace)
  # Replace Soviet Croatia with Croatia
  uclPlayers$Birthplace <- gsub("SR Croatia, Yugoslavia", "Croatia", 
                                uclPlayers$Birthplace)
  # Replace Yugoslav countries by modern name, calculated manually
  yugoslav <- list(
    Bosnia = c(282, 291, 460),
    Croatia = c(168, 208, 404, 420, 453),
    Macedonia = c(109, 456),
    Montenegro = c(319, 405, 448, 451, 452, 454, 458),
    Serbia = c(66, 90, 105, 149, 284, 369, 457, 459, 461, 647),
    Slovenia = c(106)
  )
  # Substitute Yugoslavia with the modern country name for the given indices
  x <- sapply(1:length(yugoslav), function(x){
    uclPlayers$Birthplace[yugoslav[[x]]] <<- gsub("Yugoslavia", 
                                                 names(yugoslav)[x], 
                                                 uclPlayers$Birthplace[yugoslav[[x]]]
                                                 )
  })
  rm(x)
  
  # Replace Soviet countries with their modern equivalent
  # Ukraine
  uclPlayers$Birthplace <- gsub("Ukrainian SSR, Soviet Union", "Ukraine", 
                                uclPlayers$Birthplace)
  # Belarus
  uclPlayers$Birthplace <- gsub("Byelorussian SSR, Soviet Union", "Belarus", 
                                uclPlayers$Birthplace)
  # Lithuania
  uclPlayers$Birthplace <- gsub("Lithuanian SSR, Soviet Union", "Lithuania", 
                                uclPlayers$Birthplace)
  # Remaining Soviet entries (in ucl data) are Russia
  uclPlayers$Birthplace <- gsub("Soviet Union", "Russia", 
                                uclPlayers$Birthplace)
  
  # Remove Gran Canaria from Las Palmas name
  uclPlayers$Birthplace <- gsub("Las Palmas, Gran Canaria,", "Las Palmas,", 
                                uclPlayers$Birthplace)
  # Remove West from west Berlin
  uclPlayers$Birthplace <- gsub("West Berlin", "Berlin,", 
                                uclPlayers$Birthplace)
  # Remove all text between two commas, leaving in format town, country
  uclPlayers$Birthplace <- gsub(",.*,", ",",
                                uclPlayers$Birthplace)
  # Change Murcia as a country name to Spain
  uclPlayers$Birthplace <- gsub(", Murcia", ", Spain", 
                                uclPlayers$Birthplace)
  # Replace all East, West, Allied-Occupied Germany references
  uclPlayers$Birthplace <- gsub(",.*Germany", ", Germany", 
                                uclPlayers$Birthplace)
  # Add a space before the country name
  uclPlayers$Birthplace <- gsub(",(?=[^\\s])", ", ",
                                uclPlayers$Birthplace, perl = T)
  # Reformat and populate Players and Locations ----
  # Get unique uclPlayer birthplaces and split into City, Country
  
  # Create and populate locations variable 
  locs <- data.frame(Location = unique(uclPlayers$Birthplace), stringsAsFactors = F)
  
  # Get location coordinates from location name via geocode   
  uclCoords <- data.frame(lat = rep(NA, nrow(locs)),
                          lon = rep(NA, nrow(locs)))
  broken <- 1:nrow(locs)
  
  # Repeatedly run this section importing coordinates until no errors
  # or until only errors that can be solved manually
  z <- 1
  repeat {
    
    i <- 1
    y <- sapply(broken, function(x) {
      if (i %% 50 == 0) {print(i)}
      i <<- i + 1
      curr <- geocode(locs$Location[x])
      uclCoords[x, ] <<- curr
    })
    rm(y)
    broken <- which(is.na(uclCoords$lat))
    print(length(broken))
    
    if (z >= 10) {break}
    z <- z + 1
  }
  
  uclLocations <-
    data.frame(
      unique(uclPlayers$Birthplace),
      do.call(rbind, strsplit(locs$Location, ', ')),
      uclCoords,
      stringsAsFactors = F
    )
  dimnames(uclLocations)[[2]] <- c("Location", "City", "Country", "lat", "lon")
  uclLocations$Country <- as.factor(uclLocations$Country)

  # Get location index for each player
  uclPlayers$Index <- rep(NA, nrow(uclPlayers))
  uclPlayers$Index <- unlist(sapply(uclPlayers$Birthplace, function(x){
    which(uclLocations$Location == x)
  }))
  
  updatePlayers(uclPlayers,"ucl")
  updateLocations(uclLocations)
  
  # Save data files ----
  save(uclPlayers, file = "data/UCLPlayers.dat")
  save(uclLocations, file = "data/UCLLocations.dat")
  save(uclCoords, file = "data/uclCoords.dat")
  save(Players, file = "data/Players.dat")
  save(Locations, file = "data/Locations.dat")
}

