# Import player data

library(XML)
library(httr)
library(stringr)
library(ggmap)

getUCLData <- function(){
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
  
  # Get each player's URL information
  hrefFun <- function(x){ xpathSApply(x,"./a/@href") } 
  table2 <- readHTMLTable(content(dat, "text"), elFun = hrefFun, stringsAsFactors = FALSE)
  
  df2 <- table2[2][[1]]
  uclData$URLs <- df2$V1[-1]

  # Create Players df with names
  uclPlayers <- data.frame(Name = uclData$Player, stringsAsFactors = F)
  
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
  
  ## Remaining locations with varying issues -- Entered manually
  importBrokenUCL()
  
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

  # Get location coordinates from location name via geocode
  coordErrs <- c()
  uclCoords <- data.frame()
  i <- 1
  ##### Re-run this code to correctly obtain errors, then 
  ##### repeat code to fix limit errors, and manually enter others
  x <- sapply(uclLocations$Locations, function(x) {
    if (i %% 50 == 0){print(i)}
    i <<- i + 1
    curr <- geocode(x)
    if (!is.numeric(curr$lat) |
        !is.numeric(curr$lon) | 
        is.na(curr$lat) | 
        is.na(curr$lon)) {
      coordErrs <- c(coordErrs, x)
    }
    uclCoords <<- rbind(uclCoords, curr)
  })
  rm(x)
  
  uclLocations <- cbind(uclLocations, uclCoords)
  #### --- fix location apply to correctly give locations
  #### --- get coords from locations
}


# Save data files
save(Players, file = "scripts/UCLPlayers.dat")
save(Locations, file = "scripts/UCLLocations.dat")