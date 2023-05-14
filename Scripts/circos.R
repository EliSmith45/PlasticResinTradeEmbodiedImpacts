library(igraph)
library(data.table)
library(tidygraph)
library(disparityfilter)
library(corpustools)
library(tidyverse)

flows2016imp <- flows2016 %>% filter(trade_flow == "Import", commodity_code != "3915")
flows2016imp$commodity_code <- as.character(flows2016imp$commodity_code)

gwp <- data.frame(resin = c("390110", "390120","390760","390210"),
                  gwp = c(2.1426, 1.7564, 1.7281, 1.8163), stringsAsFactors = F)

flows2016imp <- left_join(flows2016imp, gwp, by = c("commodity_code" = "resin"))

flows2016imp <- flows2016imp %>% mutate(embodiedCO2 = qty * gwp) %>% select(reporter, partner, commodity_code, embodiedCO2)

flowSummary <- flows2016imp %>%
  group_by(reporter, partner) %>%
  summarize(GWP = sum(embodiedCO2, na.rm = TRUE))


flowDataFrame <- left_join(flowDataFrame, flowSummary, by = c("Reporter" = "reporter", "Partner" = "partner"))
flowDataFrame[is.na(flowDataFrame$GWP), 3] = 0

mat <- flowDataFrame %>% spread(key = Partner, value = GWP)
rownames(mat) <- mat[,1]
mat[,1] <- NULL


network <- graph_from_adjacency_matrix(as.matrix(mat), mode = "directed", weighted = TRUE, add.colnames = NULL, add.rownames = NULL)

V(network)$strength <- strength(network, V(network), mode = "all")

network <- as_tbl_graph(network)
e <- as.list(network)$edges
n <- as.list(network)$nodes

s <- (n %>% arrange(-strength))[1:75,] 

flowDataFrame <- as.data.frame(CJ(Reporter = as.vector(s$name), Partner = as.vector(s$name)))
flowDataFrame[flowDataFrame$Reporter == "Areas, nes",1] = "Other"
flowDataFrame[flowDataFrame$Reporter == "Other Asia, nes",1] = "Taiwan"

flowDataFrame[flowDataFrame$Partner == "Areas, nes",2] = "Other"
flowDataFrame[flowDataFrame$Partner == "Other Asia, nes",2] = "Taiwan"
write.csv(backbone_mat, "./Data/circos.csv")


network_backbone <- backbone_filter(network, alpha = .000000001)
plot(network_backbone, layout = layout_in_circle(network_backbone), vertex.size = 4, edge.size = E(network_backbone)$weight)

backbone_mat <- as.data.frame(as.matrix(network_backbone[]))
