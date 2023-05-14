library(tidyverse)
library(ggplot2)
library(igraph)
library(tidygraph)

waste <- read_csv("Data/waste.csv")
mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>%
  select(Code, "Share of plastic inadequately managed (%)") %>%
  rename(mismanaged = "Share of plastic inadequately managed (%)")

mismanaged$mismanaged <- mismanaged$mismanaged / 100
waste$v = waste$v * 1000
waste$q = waste$q * 1000
waste_sum <- waste %>% 
  group_by(t) %>%
  summarize("value, USD" = sum(v),
            "quantity, metric tons" = sum(q)) %>%
  gather(key = "Reporting_type", value = "amount", 2:3)
waste_sum$Reporting_type <- as.factor(waste_sum$Reporting_type)

g <- ggplot(waste_sum) + 
  geom_line(aes(x = t, y = amount, color = Reporting_type), size = 1.2) +
  labs(x = "year", y = "amount of plastic", title = "Plastic waste traded over time") + 
  theme_bw() +
  theme(text = element_text(size = 18))

print(g)

gp <- ggplot(mutate(waste_sum, r = v/q)) + 
  geom_line(aes(x = t, y = r))
            
print(gp)

waste_agg <- waste %>% 
  group_by(t, exporter, importer) %>%
  summarize(value = sum(v),
            quantity = sum(q)) %>%
  ungroup()

polyethylene <- filter(waste, k == 391510)
############################################## create igraph objects
y = 1995
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

nodes <- data.frame()
for(y in 1995:2018){
  nodes <- rbind(nodes, get(paste("nodes", y, sep = "")))
  
}

strengthPlot <- ggplot(nodes2018) + 
  geom_point(aes(x = in_str, y = out_str)) + 
  labs(title = "In-strength vs out-strength for all years") + 
  geom_smooth(aes(x = in_str, y = out_str), method = "lm")

strengthPlot

mismanagedPlot <- ggplot(nodes2018) + 
  geom_point(aes(x = mismanaged, y = in_str)) + 
  labs(title = "mismanaged waste vs out-strength for all years")

mismanagedPlot


#################################### waste mismanagement
x = .05

mismanagement_flows <- waste_agg %>% 
  left_join(mismanaged, by = c("exporter" = "Code")) %>%
  rename(exp_mis = mismanaged) %>% 
  left_join(mismanaged, by = c("importer" = "Code")) %>%
  rename(imp_mis = mismanaged)

mismanagement_flows <- na.omit(mismanagement_flows)
mismanagement_flows <- mismanagement_flows %>% mutate(quality = "a")

mismanagement_flows$quality <-ifelse(mismanagement_flows$exp_mis - mismanagement_flows$imp_mis >= x, "good", 
                                     ifelse(mismanagement_flows$imp_mis - mismanagement_flows$exp_mis >= x, "bad", "neutral"))

flow_quality_yearly <- mismanagement_flows %>%
  group_by(t, quality) %>%
  summarize(quantity = sum(quantity), count = n()) %>% 
  ungroup()

flow_quality_yearly_noCHN <- mismanagement_flows %>%
  filter(importer != "CHN") %>%
  group_by(t, quality) %>%
  summarize(quantity = sum(quantity), count = n()) %>% 
  ungroup()

quality_plot <- ggplot(flow_quality_yearly) + 
  geom_line(aes(x = t, y = quantity, color = quality), size = 1.5) + 
  theme(text = element_text(size = 18)) + 
  labs(x = "year", y = "quantity of plastic traded, metric tons", title = "Waste flow quality (weighted)")

quality_plot

quality_plot_unweighted <- ggplot(flow_quality_yearly) + 
  geom_line(aes(x = t, y = count, color = quality), size = 1.5) + 
  theme(text = element_text(size = 18)) + 
  labs(x = "year", y = "number of flows", title = "Waste flow quality (unweighted)")

quality_plot_unweighted

quality_plot_ave <- ggplot(flow_quality_yearly %>% mutate(ave = quantity / count)) + 
  geom_line(aes(x = t, y = ave, color = quality), size = 1.5) + 
  theme(text = element_text(size = 18)) + 
  labs(x = "year", y = "average flow weight, metric tons", title = "Average flow weight by quality")

quality_plot_ave

quality_plot_ave_noCHN <- ggplot(flow_quality_yearly_noCHN %>% mutate(ave = quantity / count)) + 
  geom_line(aes(x = t, y = ave, color = quality), size = 1.5) + 
  theme(text = element_text(size = 18)) + 
  labs(x = "year", y = "average flow weight, metric tons", title = "Average flow weight by quality, excluding China")

quality_plot_ave_noCHN

quality_plot_q_noCHN <- ggplot(flow_quality_yearly_noCHN) + 
  geom_line(aes(x = t, y = quantity, color = quality), size = 1.5) + 
  theme(text = element_text(size = 18)) + 
  labs(x = "year", y = "quantity traded, metric tons", title = "Plastic traded, excluding China")

quality_plot_q_noCHN

good <- mismanagement_flows[mismanagement_flows$quality == "good",]
bad <- mismanagement_flows[mismanagement_flows$quality == "bad",]
neutral <- mismanagement_flows[mismanagement_flows$quality == "neutral",]

write_csv(good, "Data/good.csv")
write_csv(bad, "Data/bad.csv")
write_csv(neutral, "Data/neutral.csv")
