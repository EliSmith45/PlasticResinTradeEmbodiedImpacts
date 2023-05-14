library(tidyverse)
library(igraph)
library(tidygraph)
library(stringr)
library(ggplot2)
library(qgraph)
library(caret)

# read data
thermoplastics <- read_csv("./Data/thermoplastics.csv")
thermoplastic_agg <- thermoplastics %>%
  group_by(t, exporter, importer) %>%
  summarize(quantity = sum(q, na.rm = T), value = sum(v, na.rm = T)) %>%
  ungroup()

#make graphs
for(y in 1995:2018){
  dataframe <- thermoplastic_agg %>%
    filter(t == y) %>%
    select(exporter, importer, quantity) %>% 
    rename(weight = quantity)
  
  dataframe$weight <- dataframe$weight * 1000
  
  graph <- graph_from_data_frame(dataframe) %>% simplify() %>% as_tbl_graph()
  
  V(graph)$name[V(graph)$name == "N/A"] = "TWN"
  assign(paste("graph", y, sep = ""), graph)
  rm("graph")
  rm("dataframe")
}

# add node attributes
for(y in 1995:2018) {
  
  #make graphs
  graph <- get(paste("graph", y, sep = ""))
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
  
  
  assign(paste("graph", y, sep = ""), graph)
  assign(paste("nodes", y, sep = ""), nodes)
  rm("nodes", "graph")
}

# whole node list
nodelist <- data.frame(names(nodes2018)) %>% mutate(d = NA) %>% spread(key = "names.nodes2018.", value = d)
for(y in 1995:2018){
  nodes <- get(paste("nodes", y, sep = ""))
  nodelist <- rbind(nodelist, nodes)
}
nodelist <- nodelist[!is.na(nodelist$name),]



#x-year stability
nodeStability <- function(x, country, mode = c("in", "out", "total")){
  stability = data.frame(year = 1995:2018, 
                         name = rep(country, 24),
                         two_year = integer(24))
  
  #calculate network stability
  for(i in seq(2018, 1995, -x)){
    edges_x_years <- data.frame(from = integer(),
                                to = integer(), 
                                year = integer())
    
    k = 0
    for(j in 1:x){
      
      if(i - j + 1 < 1995){
        break
      }else{
        k = k+1
        graph <- as_tbl_graph(get(paste("subgraph", i - j + 1, sep = "")))
        edgelist <- as.list(graph)$edges
        edgelist <- edgelist %>% mutate(from_name = V(graph)$name[edgelist$from], to_name = V(graph)$name[edgelist$to])
        edges_x_years <- rbind(edges_x_years, edgelist %>% select(-weight))
      }
    }
    
    edges_x_years$id <- paste(edges_x_years$from_name, edges_x_years$to_name)
    
    occ <- edges_x_years %>% 
      group_by(id) %>% 
      summarize(count = n()) %>% 
      ungroup()
    
    assign(paste("occ", x, sep = ""), occ %>% mutate(year = i))
    
    score = sum(occ$count, na.rm = T)
    max_score = k * length(occ$id)
    
    stability[stability$year == i, 3] <- score / max_score
    #print(score / max_score)
    
  }
  


  #normalize the indices
  stability_normalized <- stability
  stability_normalized$two_year <- (stability_normalized$two_year - 0.5) / 0.5
  # stability_normalized$three_year <- (stability_normalized$three_year - (1/3)) / (2/3)
  # stability_normalized$four_year <- (stability_normalized$four_year - 0.25) / 0.75
  # stability_normalized$six_year <- (stability_normalized$six_year - (1/6)) / (5/6)
  # stability_normalized$eight_year <- (stability_normalized$eight_year - (1/8)) / (7/8)
  # stability_normalized$twelve_year <- (stability_normalized$twelve_year - (1/12)) / (11/12)
   
  stability_normalized[is.na(stability_normalized$two_year),3] <- -1
  stability_normalized[stability_normalized$two_year <= 0,3] <- NA
  
  #stability_normalized <- stability_normalized %>% select(-eleven_year, -ten_year, -nine_year, -seven_year, -five_year)
  
  if(mode == "in"){
    stability_normalized_in <- data.frame(year = stability_normalized$year,
                                          name = stability_normalized$name,
                                          two_year_in = stability_normalized$two_year) 
                                          # three_year_in = stability_normalized$three_year,
                                          # four_year_in = stability_normalized$four_year, 
                                          # six_year_in = stability_normalized$six_year, 
                                          # eight_year_in = stability_normalized$eight_year, 
                                          # twelve_year_in = stability_normalized$twelve_year) 
    return(stability_normalized_in)
    
  }else if(mode == "out"){
    stability_normalized_out <- data.frame(year = stability_normalized$year,
                                           name = stability_normalized$name,
                                           two_year_out = stability_normalized$two_year)
                                           # three_year_out = stability_normalized$three_year,
                                           # four_year_out = stability_normalized$four_year, 
                                           # six_year_out = stability_normalized$six_year, 
                                           # eight_year_out = stability_normalized$eight_year, 
                                           # twelve_year_out = stability_normalized$twelve_year) 
    return(stability_normalized_out)
      
  }else if(mode == "total"){
    stability_normalized_tot <- data.frame(year = stability_normalized$year,
                                           name = stability_normalized$name,
                                           two_year_tot = stability_normalized$two_year) 
                                           # three_year_tot = stability_normalized$three_year,
                                           # four_year_tot = stability_normalized$four_year, 
                                           # six_year_tot = stability_normalized$six_year, 
                                           # eight_year_tot = stability_normalized$eight_year, 
                                           # twelve_year_tot = stability_normalized$twelve_year) 
    return(stability_normalized_tot)
      
  }
}

# calculating node-level stability
node_stab_in <- data.frame(name = NA,
                           year = NA,
                           two_year_in = NA)
                           # three_year_in = NA,
                           # four_year_in = NA, 
                           # six_year_in = NA, 
                           # eight_year_in = NA, 
                           # twelve_year_in = NA)
node_stab_out <- data.frame(name = NA,
                            year = NA,
                            two_year_out = NA) 
                            # three_year_out = NA,
                            # four_year_out = NA, 
                            # six_year_out = NA, 
                            # eight_year_out = NA, 
                            # twelve_year_out = NA)
node_stab_tot<- data.frame(name = NA,
                           year = NA,
                           two_year_tot = NA) 
                           # three_year_tot = NA,
                           # four_year_tot = NA, 
                           # six_year_tot = NA, 
                           # eight_year_tot = NA, 
                           # twelve_year_tot = NA)


#in stability
for(c in unique(nodelist$name)){
  for(y in 1995:2018){
    selected_edges <- get(paste("graph", y, sep = "")) %>%
      activate(edges) %>%
      filter(.N()$name[to] == c)
    subgraph <- subgraph.edges(selected_edges, eids = E(selected_edges))
    assign(paste("subgraph", y, sep = ""), subgraph)
  }
  
  stability <- nodeStability(x = 2, country = c, mode = "in")
  node_stab_in <-rbind(node_stab_in, stability)
  
}
node_stab_in <- node_stab_in[!is.na(node_stab_in$two_year_in),]

#out stability
for(c in unique(nodelist$name)){
  for(y in 1995:2018){
    selected_edges <- get(paste("graph", y, sep = "")) %>%
      activate(edges) %>%
      filter(.N()$name[from] == c)
    subgraph <- subgraph.edges(selected_edges, eids = E(selected_edges))
    assign(paste("subgraph", y, sep = ""), subgraph)
  }
  
  stability <- nodeStability(x = 2, country = c, mode = "out")
  node_stab_out <-rbind(node_stab_out, stability)
  
}
node_stab_out <- node_stab_out[!is.na(node_stab_out$two_year_in),]

#total stability
for(c in unique(nodelist$name)){
  for(y in 1995:2018){
    selected_edges <- get(paste("graph", y, sep = "")) %>%
      activate(edges) %>%
      filter(.N()$name[from] == c | .N()$name[to] == c)
    subgraph <- subgraph.edges(selected_edges, eids = E(selected_edges))
    assign(paste("subgraph", y, sep = ""), subgraph)
  }
  
  stability <- nodeStability(x = 2, country = c, mode = "total")
  node_stab_tot <-rbind(node_stab_tot, stability)
  
}
node_stab_tot <- node_stab_out[!is.na(node_stab_out$two_year_in),]
