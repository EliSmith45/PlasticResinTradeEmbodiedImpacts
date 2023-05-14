
nameToCode <- function(country_name)
{
  string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
  reporters <- fromJSON(file=string)
  reporters <- as.data.frame(t(sapply(reporters$results,rbind)))
  reporters$V1 <- unlist(reporters$V1)
  reporters$V2 <- unlist(reporters$V2)
  # converts a country name to the proper country code (Name should be properly capitalized,
  # include spaces as necessary)
  
  
  index <- which(reporters$V2 == "Algeria", arr.ind = TRUE)
  code =  unlist((reporters$V1[index])[1])
}

write.csv(x = reporters,file = "countrycodes.csv", row.names = FALSE)
