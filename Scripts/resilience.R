library(igraph)
library(tidyverse)
library(ggplot2)
library(tidygraph)

#make graphs
waste <- read_csv("Data/waste.csv")
waste_agg <- waste %>% 
  group_by(t, exporter, importer) %>%
  summarize(value = sum(v),
            quantity = sum(q)) %>%
  ungroup()

for(y in 1995:2018){
  dataframe <- waste_agg %>%
    filter(t == y) %>%
    select(exporter, importer, quantity) %>% 
    rename(weight = quantity)
  
  dataframe$weight <- dataframe$weight * 1000
  
  graph <- graph_from_data_frame(dataframe) %>% simplify() %>% as_tbl_graph()
  
  V(graph)$name[V(graph)$name == "N/A"] = "TWN"
  assign(paste("graph", y, sep = ""), graph)
  #V(graph)$mismanaged = mismanaged[mismanaged$Code == V(graph)$name,2]
  
  graph <- as_tbl_graph(graph)
  #graph <- graph %>%
  #  activate(nodes) %>% 
  #  mutate(mismanaged = mismanaged[mismanaged$Code == V(graph)$name,2])
  #n <- as.list(graph)$nodes
  
  rm("graph")
  rm("dataframe")
}

for(y in 1995:2018) {
  
  #make graphs
  graph <- get(paste("graph", y, sep = ""))
  graph <- as_tbl_graph(graph)
  V(graph)$in_deg <- igraph::degree(graph = graph, mode = "in")
  V(graph)$out_deg <- igraph::degree(graph = graph, mode = "out", )
  V(graph)$in_str <- strength(graph = graph, mode = "in")
  V(graph)$out_str <- strength(graph = graph, mode = "out")
  V(graph)$tot_str <- strength(graph = graph, mode = "total")
  V(graph)$eigen <- eigen_centrality(graph = graph, directed = F, scale = T)$vector
  V(graph)$betweenness <- igraph::betweenness(graph = graph)
  V(graph)$closeness <- igraph::closeness(graph = graph, mode = "total")
  V(graph)$in_str <- strength(graph = graph, mode = "in")
  V(graph)$year <- rep(y, length(V(graph)))
  E(graph)$year <- rep(y, length(E(graph)))
  
  #make node lists
  nodes <- as.list(as_tbl_graph(graph))$nodes
  nodes <- nodes %>%
    arrange(-in_deg) %>%
    mutate(in_deg_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-out_deg) %>%
    mutate(out_deg_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-in_str) %>%
    mutate(in_str_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-out_str) %>%
    mutate(out_str_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-tot_str) %>%
    mutate(tot_str_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-betweenness) %>%
    mutate(betweenness_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-closeness) %>%
    mutate(closeness_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50))) %>%
    arrange(-eigen) %>%
    mutate(eigen_rank = c(seq(100, 2, -2), rep(0, length(V(graph)) - 50)))
  
  nodes$year <- as.factor(nodes$year)
  
  #make edge lists
  
  
  
  assign(paste("graph", y, sep = ""), graph)
  assign(paste("nodes", y, sep = ""), nodes)
  rm("nodes", "graph")
}

#simulate node removal


resilience_test <- function(node_list) {
  i = 1
  
  resilience_data <- data.frame(iteration = integer(), targeted = numeric(), random = numeric())
  n_random = node_list
  n_targeted = node_list
  for(i in 1:length(node_list$name)) {
    r = data.frame(iteration = i, targeted = sum(n_targeted$tot_str) / 2, random = sum(n_random$tot_str) / 2)
    resilience_data <- rbind(resilience_data, r)
    
    #remove nodes
    n_random <- n_random[-round(runif(1, 1, length(n_random$name))),]
    n_targeted <- arrange(n_targeted, -in_str)
    n_targeted <- n_targeted[-1,]
    i = i+1
  }
  
  resilience_data <- gather(resilience_data, key = "mode", value = "throughput", 2:3)
}


resilience_2018 <- resilience_test(nodes2018) 
resilience_2013 <- resilience_test(nodes2013)

resilience_2018_p <- ggplot(resilience_2018) + 
  geom_line(aes(x = iteration, y = throughput, color = mode), size = 1.25)
resilience_2018_p

resilience_2013_p <- ggplot(resilience_2013) + 
  geom_line(aes(x = iteration, y = throughput, color = mode), size = 1.25)
resilience_2013_p


resilience_2018$year = 2018
resilience_2013$year = 2013

resilience_both <- ggplot() + 
  geom_line(data = filter(resilience_2018, mode == "targeted"), aes(x = iteration, y = throughput), size = 1.25, color = "Red") + 
  geom_line(data = filter(resilience_2013, mode == "targeted"), aes(x = iteration, y = throughput), size = 1.25, color = "blue")
resilience_both
