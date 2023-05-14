library(tidyverse)
library(stringr)
library(corpustools)
library(igraph)


#get all the reporters and filter out countries/regions from ignoreList
reporters <- getReporters() 
reporters <- reporters %>%
  filter(!(V2 %in% ignoreList)) %>%
  rename(code = V1, countries = V2)

#write_csv(reporters, "./Data/reporters.csv")

#Creates the data frame of reporters and partners. It is the list of reporters cross-joined with itself
flowDataFrame <- as.data.frame(CJ(Reporter = as.vector(reporters$countries), Partner = as.vector(reporters$countries)))




flows <- read_csv("./Data/BACI_HS92_Y2018_V202001.csv")  
countries <- read_csv("./Data/reporters.csv")

flows <- flows %>% 
  mutate(twoDigit = substr(k, 1, 2)) %>%
  filter(twoDigit == "39")

flows$k <- as.numeric(flows$k)

primary <- flows %>% filter(k < 391500)
manufactured <- flows %>% filter(k > 391500)


#2016 data
flows2016 <- read_csv("./Data/BACI_HS92_Y2016_V202001.csv") 

flows2016 <- flows2016 %>% 
  filter(k %in% c("390110", "390120", "390210", "390760" )) %>%
  rename(year = t, exporter = i, importer = j, commodity = k, value = v, quantity = q)

flows2016 <- left_join(flows2016, countries, by = c("exporter" = "code"))
flows2016 <- flows2016 %>% rename(exp_name = countries)

flows2016 <- left_join(flows2016, countries, by = c("importer" = "code"))
flows2016 <- flows2016 %>% rename(imp_name = countries)


gwp <- data.frame(resin = c("390110", "390120","390760","390210"),
                  gwp = c(2.1426, 1.7564, 1.7281, 1.8163), stringsAsFactors = F)

flows2016 <- left_join(flows2016, gwp, by = c("commodity" = "resin"))

flows2016 <- flows2016 %>% mutate(embodiedCO2 = quantity * gwp * 1000) %>% select(exp_name, imp_name, commodity, embodiedCO2)

flowSummary <- flows2016 %>%
  group_by(exp_name, imp_name) %>%
  summarize(GWP = sum(embodiedCO2, na.rm = TRUE))

flowDataFrame <- left_join(flowDataFrame, flowSummary, by = c("Reporter" = "exp_name", "Partner" = "imp_name"))

flowDataFrame <- flowDataFrame %>% rename(exporter = Reporter, importer = Partner)
flowDataFrame <- spread(flowDataFrame, key = exporter, value = GWP)
flowDataFrame[is.na(flowDataFrame)] = 0


rownames(flowDataFrame) <- flowDataFrame[,1]
flowDataFrame[,1] <- NULL

network <- graph_from_adjacency_matrix(as.matrix(flowDataFrame), mode = "directed", weighted = TRUE, add.colnames = NULL, add.rownames = NULL)

network_backbone <- backbone_filter(network, alpha = .000000001)
plot(network_backbone, layout = layout_in_circle(network_backbone), vertex.size = 4, edge.size = E(network_backbone)$weight)

backbone_mat <- as.data.frame(as.matrix(network_backbone[]))


write.csv(backbone_mat, "./Data/circos_baci.csv")
