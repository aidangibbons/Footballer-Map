# Import player data

library(XML)
library(httr)
library(stringr)
library(ggmap)

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

  # Create Players df with names
  uclPlayers <- data.frame(Name = uclData$Player, stringsAsFactors = F)
  
  # Get UCL Player birthplaces from URLs ----
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
  
  # Get all unique locations from the players df
  uclLocations <- data.frame(Locations = unique(uclPlayers$Birthplace), stringsAsFactors = F)

  
  # Get location coordinates from location name via geocode ----
  uclCoords <- data.frame(lat = rep(NA, nrow(uclLocations)),
                          lon = rep(NA, nrow(uclLocations)))
  broken <- 1:nrow(uclLocations)
  
  # Repeatedly run this section importing coordinates until no errors
  # or until only errors that can be solved manually
  z <- 1
  repeat {
    
    i <- 1
    y <- sapply(broken, function(x) {
      if (i %% 50 == 0){print(i)}
      i <<- i + 1
      curr <- geocode(uclLocations$Locations[x])
      uclCoords[x, ] <<- curr
    })
    rm(y)
    broken <- which(is.na(uclCoords$lat))
    print(length(broken))
    
    if ((length(broken) == 1) | (z >= 10)) {
      break
    }
    z <- z + 1
  }
  
  importBrokenUCLCoords()
  uclLocations <- cbind(uclLocations, uclCoords)
  

  # Get location index for each player ----
  uclPlayers$Index <- rep(NA, nrow(uclPlayers))
  uclPlayers$Index <- sapply(uclPlayers$Birthplace, function(x){
    which(uclLocations$Locations == x)
  })
}


# Save data files ----
save(uclPlayers, file = "data/UCLPlayers.dat")
save(uclLocations, file = "data/UCLLocations.dat")
save(uclCoords, file = "data/uclCoords.dat")
