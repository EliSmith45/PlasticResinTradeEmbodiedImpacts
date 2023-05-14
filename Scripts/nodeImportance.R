library(tidyverse)
library(ggplot2)
library(igraph)
library(tidygraph)

waste <- read_csv("Data/waste.csv")
mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>%
  select(Code, "Share of plastic inadequately managed (%)") %>%
  rename(mismanaged = "Share of plastic inadequately managed (%)")

mismanaged$mismanaged <- mismanaged$mismanaged / 100

waste_agg <- waste %>% 
  group_by(t, exporter, importer) %>%
  summarize(value = sum(v),
            quantity = sum(q)) %>%
  ungroup()
mismanagement_flows <- waste_agg %>% 
  left_join(mismanaged, by = c("exporter" = "Code")) %>%
  rename(exp_mis = mismanaged) %>% 
  left_join(mismanaged, by = c("importer" = "Code")) %>%
  rename(imp_mis = mismanaged)

mismanagement_flows <- na.omit(mismanagement_flows)
mismanagement_flows <- mismanagement_flows %>% mutate(extra_mis = (imp_mis - exp_mis) * quantity) %>% filter(t == 2018)



graph <- graph_from_data_frame(mismanagement_flows %>% select(exporter, importer, extra_mis) %>% rename(weight = extra_mis))
strength_rankings <- data.frame(country = V(graph)$name,
                                in_strength = strength(graph, V(graph), mode = "in"),
                                out_strength = strength(graph, V(graph), mode = "out"),
                                total_strength = strength(graph, V(graph), mode = "total")) %>%
  left_join(mismanaged, by = c("country" = "Code"))

impacts <- data.frame(country = V(graph)$name,
                      impact_no_mis = 0,
                      impact_no_exp = 0,
                      impact_no_imp = 0)
impacts$country <- as.character(impacts$country)


for(node in V(graph)$name){
  dropped_node <- mismanagement_flows
  dropped_node[dropped_node$exporter == node, 6] = 0
  dropped_node[dropped_node$importer == node, 7] = 0
  no_exports <- mismanagement_flows
  no_exports[no_exports$exporter == node, 5] = 0
  dropped_node$extra_mis <- (dropped_node$imp_mis - dropped_node$exp_mis) * dropped_node$quantity
  new_graph <- graph_from_data_frame(dropped_node %>% select(exporter, importer, extra_mis) %>% rename(weight = extra_mis))
  impacts[impacts$country == node, 2] = sum(strength(graph, V(graph), mode = "in")) - sum(strength(new_graph, V(new_graph), mode = "in"))
  
  no_exports <- mismanagement_flows
  no_exports[no_exports$exporter == node, 5] = 0
  no_exports$extra_mis <- (no_exports$imp_mis - no_exports$exp_mis) * no_exports$quantity
  new_graph <- graph_from_data_frame(no_exports %>% select(exporter, importer, extra_mis) %>% rename(weight = extra_mis))
  impacts[impacts$country == node, 3] = sum(strength(graph, V(graph), mode = "in")) - sum(strength(new_graph, V(new_graph), mode = "in"))
 
  no_imports <- mismanagement_flows
  no_imports[no_imports$importer == node, 5] = 0
  no_imports$extra_mis <- (no_imports$imp_mis - no_imports$exp_mis) * no_imports$quantity
  new_graph <- graph_from_data_frame(no_imports %>% select(exporter, importer, extra_mis) %>% rename(weight = extra_mis))
  impacts[impacts$country == node, 4] = sum(strength(graph, V(graph), mode = "in")) - sum(strength(new_graph, V(new_graph), mode = "in"))
  
  
}

impacts <- impacts %>%
  arrange(-impact_no_mis) %>%
  mutate(no_mis_cumulative = cumsum(impact_no_mis), no_mis_rankings = 1:length(impacts[,1])) %>%
  arrange(-impact_no_exp) %>%
  mutate(no_exp_cumulative = cumsum(impact_no_exp), no_exp_rankings = 1:length(impacts[,1])) %>%
  arrange(-impact_no_imp) %>%
  mutate(no_imp_cumulative = cumsum(impact_no_imp), no_imp_rankings = 1:length(impacts[,1])) 

impact_cumulative <- select(impacts, no_mis_rankings, no_mis_cumulative) %>%
  left_join(select(impacts, no_exp_rankings, no_exp_cumulative), by = c("no_mis_rankings" = "no_exp_rankings")) %>%
  left_join(select(impacts, no_imp_rankings, no_imp_cumulative), by = c("no_mis_rankings" = "no_imp_rankings")) %>%
  rename(order = no_mis_rankings, "Target importers" = no_mis_cumulative, 
         "Target exporters" = no_exp_cumulative, 
         "Eliminate importers" = no_imp_cumulative) %>%
  gather(key = Type, value = impact, 2:4)

ggplot(impact_cumulative[impact_cumulative$order <=50 & impact_cumulative$Type != "Eliminate importers",]) +
  geom_line(aes(x = order, y = impact, color = Type), size = 1.2) + 
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "number of targeted countries", y = "reduction in mismanaged plastic waste (tons)", title = "")

