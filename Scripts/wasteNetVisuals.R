library(tidyverse)
library(igraph)
library(tidygraph)
library(RColorBrewer)

data <- read_csv("./Data/baci_waste_iso.csv")
mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>% 
  rename(m = "Share of plastic inadequately managed (%)") %>% select(Code, m)

agg <- data %>%
  group_by(exporter, importer, t) %>%
  summarize(value = sum(v, na.rm = TRUE),
            quantity = sum(q, na.rm = TRUE))


net2018 <- agg %>%
  filter(t == 2018) %>%
  select(exporter, importer, value)

net2018 <- graph_from_data_frame(net2018) %>% as_tbl_graph()
n <- as.list(net2018)$nodes
e <- as.list(net2018)$edges

net2018 <- net2018 %>%
  activate(nodes) %>%
  left_join(mismanaged, by = c("name" = "Code"))

E(net2018)$weight <- E(net2018)$value

net2018 <- net2018 %>% 
  activate(edges) %>%
  select(-value) %>%
  mutate(mismanagedDiff = .N()$m[to] - .N()$m[from]) %>%
  mutate(extraMismanaged = weight * mismanagedDiff / 100)

E(net2018)$color <- ifelse(is.na(E(net2018)$mismanagedDiff),"gray", ifelse(E(net2018)$mismanagedDiff >= 0, "red", "green"))

net2018_filtered <- subgraph.edges(net2018, eids = E(net2018)[E(net2018)$weight >= 2000])

Layout <- layout_with_graphopt(net2018_filtered, charge = .011, spring.constant = .0001) 
plot(net2018_filtered, vertex.size = 7, edge.width = E(net2018)$weight / 5000, layout = Layout)
################################################################################
# 2012
net2012 <- agg %>%
  filter(t == 2012) %>%
  select(exporter, importer, value)

net2012 <- graph_from_data_frame(net2012) %>% as_tbl_graph()
n <- as.list(net2012)$nodes
e <- as.list(net2012)$edges

net2012 <- net2012 %>%
  activate(nodes) %>%
  left_join(mismanaged, by = c("name" = "Code"))

E(net2012)$weight <- E(net2012)$value

net2012 <- net2012 %>% 
  activate(edges) %>%
  select(-value) %>%
  mutate(mismanagedDiff = .N()$m[to] - .N()$m[from]) %>%
  mutate(extraMismanaged = weight * mismanagedDiff / 100)

E(net2012)$color <- ifelse(is.na(E(net2012)$mismanagedDiff),"gray", ifelse(E(net2012)$mismanagedDiff >= 0, "red", "green"))

net2012_filtered <- subgraph.edges(net2012, eids = E(net2012)[E(net2012)$weight >= 2000])

Layout <- layout_with_graphopt(net2012_filtered, charge = .011, spring.constant = .0001) 
plot(net2012_filtered, vertex.size = 7, edge.width = E(net2012)$weight / 5000, layout = Layout)

