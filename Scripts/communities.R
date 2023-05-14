library(tidyverse)
library(ggplot2)
library(tidygraph)

t <- read_csv("Data/thermoplastics.csv")
agg <- t %>% 
  group_by(t, k) %>%
  summarize(q = sum(q, na.rm = T))

agg$t <- as.numeric(agg$t)
agg$k <-as.factor(agg$k)


plot <- ggplot(agg) + 
  geom_line(aes(x = t, y = q, color = k))

print(plot)            

agg2018 <- filter(agg, t == 2018)

top2018 <- filter(agg2018, q >=2600000)

frac <- sum(top2018$q)/sum(agg2018$q)


for(y in 1995:2018){
  assign(paste("graph",y,"_u", sep = ""), as.undirected(get(paste("graph",y, sep = "")), mode = "collapse",edge.attr.comb = "sum") %>% as_tbl_graph())
  
}



edges <- as.list(as_tbl_graph(graph2018))$edges
edges_u <- as.list(as_tbl_graph(graph2018_u))$edges





############################## communities

walktrap <- cluster_walktrap(graph2018_u)
V(graph2018_u)$membership_walktrap <- membership(walktrap)
modularity(walktrap)

louvain <- cluster_louvain(graph2018_u)
V(graph2018_u)$membership_louvain <- membership(louvain)
modularity(louvain)

fastgreedy <- cluster_fast_greedy(graph2018_u)
V(graph2018_u)$membership_fastgreedy <- membership(fastgreedy)
modularity(fastgreedy)





l = layout_in_circle(graph2018_u)
plot(graph2018_u, layout = l, vertex.size = 8, vertex.color = V(graph2018_u)$membership_louvain)


 nodes <- as.list(as_tbl_graph(graph2018_u))$nodes
edges <- as.list(as_tbl_graph(graph2018_u))$edges


graph2018_u <- graph2018_u %>% 
  activate(edges) %>%
  mutate(from_group_w = .N()$membership_walktrap[from],
         to_group_w = .N()$membership_walktrap[to],
         from_group_l = .N()$membership_louvain[from],
         to_group_l = .N()$membership_louvain[to],
         from_group_fg = .N()$membership_fastgreedy[from],
         to_group_fg = .N()$membership_fastgreedy[to]) %>%
  mutate(group_id_w = paste(from_group_w, to_group_w),
         group_id_l = paste(from_group_l, to_group_l),
         group_id_fg = paste(from_group_fg, to_group_fg))


summary_edges_w <- edges %>%
  group_by(group_id_w) %>%
  summarize(q = sum(weight), n = n())

summary_nodes_w <- nodes %>%
  group_by(membership_walktrap) %>%
  summarize(n = n())

summary_edges_l <- edges %>%
  group_by(group_id_l) %>%
  summarize(q = sum(weight), n = n())

summary_nodes_l <- nodes %>%
  group_by(membership_louvain) %>%
  summarize(n = n())

summary_edges_fg <- edges %>%
  group_by(group_id_fg) %>%
  summarize(q = sum(weight), n = n())

summary_nodes_fg <- nodes %>%
  group_by(membership_fastgreedy) %>%
  summarize(n = n())
