# Import player data

library(XML)
library(httr)
library(stringr)
library(ggmap)
getUCLData <- function(){
  urlUCL <- "https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_winning_players"
  dat <- GET(url = urlUCL)
  doc <- readHTMLTable(
    doc = content(dat, "text"), StringsAsFactors = F)
  uclData <- doc[2][[1]]
  uclData[] <- lapply(uclData, as.character)
  colnames(uclData) <- uclData[1, ]
  uclData <- uclData[-1 ,]
  
  uclData$Nationality <- as.factor(uclData$Nationality)
  uclData$`Titles won` <- as.numeric(uclData$`Titles won`)
  
  
  hrefFun <- function(x){ xpathSApply(x,"./a/@href") } 
  table2 <- readHTMLTable(content(dat, "text"), elFun = hrefFun, stringsAsFactors = FALSE)
  
  df2 <- table2[2][[1]]
  uclData$URLs <- df2$V1[-1]

  Players <- data.frame(Name = uclData$Player)
  
  # UCL Player birthplaces from URLs
  i <- 1
  Players$Location <- sapply(uclData$URLs, FUN = function(x) {
    if(i %% 50 == 0){print(i)}
    i <<- i + 1
    urlUCLPlayer <- paste0("https://en.wikipedia.org", x)
    datUCLP <- GET(url = urlUCLPlayer)
    docUCLP <- readHTMLTable(doc = content(datUCLP, "text"), StringsAsFactors = F)
    indPOB <- which(as.character(docUCLP[1][[1]]$V1[1:10]) == "Place of birth")
    as.character(docUCLP[1][[1]]$V2[indPOB])
  })
  
  # UCL Location coordinates
  UCLCoords <- as.vector(sapply(Players$Location[1:10], FUN = geocode))
  
  
  #### --- fix location apply to correctly give locations
  #### --- get coords from locations
}
