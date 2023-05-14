library(tidyverse)
library(ggplot)
library(data.table)

data <- read_csv("Data/cleanRequestNew.csv") %>% filter(partner != "World")
flowDataFrame <- as.data.frame(CJ(reporter = as.vector(reporters$countries), partner = as.vector(reporters$countries)))


for(y in unique(data$year) %>% sort()){
  for(cc in unique(data$commodity_code) %>% sort()){
    for(f in unique(data$trade_flow) %>% sort()){
     
      #Create matrices for mass
      
      mass <- data %>%
        filter(year == y, commodity_code == cc, trade_flow == f) %>%
        select(reporter, partner, qty)
      
      mass <- left_join(flowDataFrame, mass) 
      mass[is.na(mass$qty),3] = 0
      
      mass <- mass %>%  spread(key = partner, value = qty)
      
      rownames(mass) <- mass$reporter
      mass$reporter <- NULL
      
    
      
      
      #Create matrices for trade value
      
      value <- data %>%
        filter(year == y, commodity_code == cc, trade_flow == f) %>%
        select(reporter, partner, trade_value_us)
      
      value <- left_join(flowDataFrame, value) 
      
      value[is.na(value$trade_value_us),3] = 0
      
      value <- value %>%  spread(key = partner, value = trade_value_us)
      
      rownames(value) <- value$reporter
      value$reporter <- NULL
      
      
      if(f == "Import"){
        
        write.csv(as.data.frame(t(mass)), paste("mass",y,cc,paste(f,".csv",sep = ""), sep = "_"))
        write.csv(as.data.frame(t(value)), paste("value",y,cc,paste(f,".csv",sep = ""), sep = "_"))
      }else {
        write.csv(mass, paste("mass",y,cc,paste(f,".csv",sep = ""), sep = "_"))
        write.csv(value, paste("value",y,cc,paste(f,".csv",sep = ""), sep = "_"))  
        
      }
      
     
    }
  }
  
}

