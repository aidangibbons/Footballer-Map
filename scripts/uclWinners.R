uclWinners <- data.frame(Year = 1956:2018)
uclWinners$Team <- sapply(1956:2018, function(x){
  names(table(uclData$Clubs[which(x == uclData$Years)]))[[1]]
})

uclWinners$Team[4] <- "Real Madrid"
uclWinners$Team[17] <- "Ajax"
uclWinners$Team[23] <- "Liverpool"
uclWinners$Team[31] <- "Steaua Bucuresti"
uclWinners$Team[61] <- "Real Madrid"

