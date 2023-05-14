library(tidyverse)
library(igraph)
library(tidygraph)
library(stringr)
library(ggplot2)
library(qgraph)
library(caret)

theme_set(theme_bw(base_size = 18)) 
data <- data.frame()

for(y in 1995:2018) {
  d <- read_csv(paste0("Data/BACI_HS92_Y",y, "_V202001.csv", sep = ""))
  d$k <- as.numeric(d$k)
  d <- filter(d, k >= 390000 & k <= 400000)
  data <- rbind(data, d)
  
  
}

reporters <- read_csv("./Data/ISO_codes_countries.csv")

data <- data %>%
  left_join(select(reporters, "Country Code", "ISO3-digit Alpha"), by = c("i" = "Country Code")) %>%
  rename(exporter = "ISO3-digit Alpha") %>%
  left_join(select(reporters, "Country Code", "ISO3-digit Alpha"), by = c("j" = "Country Code")) %>%
  rename(importer = "ISO3-digit Alpha") %>%
  select(-i, -j)

  
thermoplastics <- data %>% filter(k < 391200 & 
                                  k != 391000 & 
                                  substr(k, 0, 4) != 3909 &
                                  k != 390730 &
                                  k != 390750 &
                                  k != 390770 & 
                                  k != 390791 )


thermosets <- data %>% filter(k == 391000 | 
                              substr(k, 0, 4) == 3909 |
                              k == 390730 |
                              k == 390750 |
                              k == 390791)

bio <- data %>% filter(k == 390770 | 
                       substr(k, 0, 4) == 3912 |
                       substr(k, 0, 4) == 3913)


waste <- data %>% filter(substr(k, 0, 4) == 3915)


  
write_csv(thermoplastics, "./Data/thermoplastics.csv")                         
write_csv(thermosets, "./Data/thermosets.csv")    
write_csv(bio, "./Data/bio.csv")  
write_csv(waste, "./Data/waste.csv")  
#######################################################################################################################

thermoplastics <- read_csv("./Data/thermoplastics.csv")

thermoplastic_agg <- thermoplastics %>%
  group_by(t, exporter, importer) %>%
  summarize(quantity = sum(q, na.rm = T), value = sum(v, na.rm = T)) %>%
  ungroup()


d2018 <- thermoplastic_agg %>%
  filter(t == 2018) %>%
  select(exporter, importer, quantity)

net2018 <- as_tbl_graph(graph_from_data_frame(d2018))
plot(net2018)

E(net2018)$weight <- E(net2018)$quantity *1000
net2018 <- net2018 %>%
  activate(edges) %>%
  select(-quantity)

edges <- as.list(net2018)$edges
nodes <- as.list(net2018)$nodes

ranks <- as.data.frame(page_rank(net2018, algo = "prpack", weights = E(net2018)$weight)$vector)
ranks <- rename(ranks, rank = "page_rank(net2018, algo = \"prpack\", weights = E(net2018)$weight)$vector")

hist(ranks[,1])
hist(log(ranks[,1]))

#temporal analysis 
####################################################################################################################


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

edges <- as.list(graph2018)$edges

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




####################################################################################### x-year stability


stability = data.frame(year = 1995:2018, 
                       two_year = integer(24),
                       three_year = integer(24),
                       four_year = integer(24),
                       five_year = integer(24),
                       six_year = integer(24),
                       seven_year = integer(24),
                       eight_year = integer(24),
                       nine_year = integer(24),
                       ten_year = integer(24), 
                       eleven_year = integer(24), 
                       twelve_year = integer(24))
  
for(x in 2:12){  
  
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
        graph <- as_tbl_graph(get(paste("graph", i - j + 1, sep = "")))
        edgelist <- as.list(get(paste("graph", i - j + 1, sep = "")))$edges
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
      
    stability[stability$year == i, x] <- score / max_score
    #print(score / max_score)
    
  }
  
}


stability_normalized <- stability
stability_normalized$two_year <- (stability_normalized$two_year - 0.5) / 0.5
stability_normalized$three_year <- (stability_normalized$three_year - (1/3)) / (2/3)
stability_normalized$four_year <- (stability_normalized$four_year - 0.25) / 0.75
stability_normalized$six_year <- (stability_normalized$six_year - (1/6)) / (5/6)
stability_normalized$eight_year <- (stability_normalized$eight_year - (1/8)) / (7/8)
stability_normalized$twelve_year <- (stability_normalized$twelve_year - (1/12)) / (11/12)

stability_normalized[stability_normalized <= 0] <- NA

stability <- stability %>%
  gather(key = period, value = index, 2:12)
stability_normalized <- stability_normalized %>%
  gather(key = period, value = index, 2:12)

stability[stability$index == 0, 3] <- NA
stability$period <- factor(stability$period, levels = c("two_year", 
                                                        "three_year",
                                                        "four_year", 
                                                        "five_year", 
                                                        "six_year", 
                                                        "seven_year", 
                                                        "eight_year", 
                                                        "nine_year", 
                                                        "ten_year", 
                                                        "eleven_year", 
                                                        "twelve_year"))
stability_normalized$period <- factor(stability_normalized$period, levels = c("two_year", 
                                                        "three_year",
                                                        "four_year", 
                                                        "five_year", 
                                                        "six_year", 
                                                        "seven_year", 
                                                        "eight_year", 
                                                        "nine_year", 
                                                        "ten_year", 
                                                        "eleven_year", 
                                                        "twelve_year"))

stability <- stability %>% filter(!(period %in% c("eleven_year", "ten_year", "nine_year", "seven_year", "five_year")))
stability_normalized <- stability_normalized %>% filter(!(period %in% c("eleven_year", "ten_year", "nine_year", "seven_year", "five_year")))

stabilityPlot <- ggplot(stability) + 
  geom_point(aes(x = year, y = index, color = period), size = 4)+
  geom_line(data = stability[!is.na(stability$index),], aes(x = year, y = index, color = period), size = 1.5) + 
  theme_bw() + 
  labs(title = "Network x-year stability index") + 
  theme(text = element_text(size = 20))

print(stabilityPlot)


stability_normalizedPlot <- ggplot(stability_normalized) +
  geom_point(aes(x = year, y = index, color = period), size = 4)+
  geom_line(data = stability_normalized[!is.na(stability_normalized$index),], aes(x = year, y = index, color = period), size = 1.5) +
  theme_bw() +
  labs(title = "Network x-year stability index, normalized") +
  theme(text = element_text(size = 20))
print(stability_normalizedPlot)

stability_normalizedPlot <- ggplot(stability_normalized) + 
  geom_boxplot(aes(y = index, color = period), size = 1.5)+
  theme_bw() + 
  labs(title = "Network x-year stability index, normalized") + 
  theme(text = element_text(size = 20)) + 
  theme(axis.text.x = element_blank())
print(stability_normalizedPlot)


########################################################################### rate of edge loss and gain

deltas <- data.frame(loss_rate = integer(23), 
                     gain_rate = integer(23), 
                     year = as.integer(1996:2018))


for(y in 1996:2018){
  graph <- as_tbl_graph(get(paste("graph", y - 1, sep = "")))
  graph_p <- as_tbl_graph(get(paste("graph", y, sep = "")))
  
  edges <- as.list(graph)$edges
  edges <- edges %>% mutate(from_name = V(graph)$name[edges$from], 
                            to_name = V(graph)$name[edges$to], 
                            id = paste(from_name, to_name))
  edges_p <- as.list(graph_p)$edges
  edges_p <- edges_p %>% mutate(from_name = V(graph_p)$name[edges_p$from], 
                            to_name = V(graph_p)$name[edges_p$to], 
                            id = paste(from_name, to_name)) 
                                           
  
  
  num_edges <- length(edges$id)
  new <- edges_p[!(edges_p$id %in% edges$id),]$id %>% length()
  gain = new / num_edges * 100
  
  lost <- edges[!(edges$id %in% edges_p$id),]$id %>% length()
  loss <- lost / num_edges * 100
  
  deltas[deltas$year == y, 2] = gain
  deltas[deltas$year == y, 1] = loss
}

cor(deltas$gain_rate, deltas$loss_rate)

deltas <- gather(deltas, key = type_change, value = rate, 1:2)

delta_plot <- ggplot(deltas) + 
  geom_line(aes(x = year, y = rate, color = type_change), size = 1.5) +
  geom_point(aes(x = year, y = rate, color = type_change), size = 4) + 
  labs(title = "Edge loss vs. gain rate by year", x = "Year", y = "Rate (%)") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

print(delta_plot)



####centrality rankings
################################################################################################# 

nodelist <- data.frame(names(nodes)) %>% mutate(d = NA) %>% spread(key = "names.nodes.", value = d)
edgelist <- data.frame(from_name = NA, to_name = NA, weight = NA, year = NA)


for(y in 1995:2018){
  nodes <- get(paste("nodes", y, sep = ""))
  nodelist <- rbind(nodelist, nodes)
  
  graph <- as_tbl_graph(get(paste("graph", y, sep = "")))
  edges <- as.list(graph)$edges
  edges <- edges %>% mutate(from_name = V(graph)$name[edges$from],
                            to_name = V(graph)$name[edges$to]) %>%
    select(-from, -to)
  edgelist <- rbind(edgelist, edges)
  
}

nodelist <- nodelist[!is.na(nodelist$name),]
edgelist <- edgelist[!is.na(edgelist$from),]
nodelist <- nodelist %>% mutate(ave_rank = (in_deg_rank + 
                                  out_deg_rank + 
                                  in_str_rank + 
                                  out_str_rank + 
                                  tot_str_rank + 
                                  closeness_rank + 
                                  betweenness_rank + 
                                  eigen_rank ) / 8)


#in strength
countries <- nodelist %>% 
  filter(year == 2018, in_str_rank > 80) %>%
  select(name)

plot_instr <- ggplot(nodelist[nodelist$name %in% countries$name,]) + 
  geom_line(aes(x = year, y = in_str_rank, color = name), size = 1.5) + 
  geom_point(aes(x = year, y = in_str_rank, color = name), size = 4) + 
  theme_bw() +
  labs(x = "Year", y = "In strength rank", title = "Country rankings by in-strength") + 
  theme(text = element_text(size = 20))

print(plot_instr)

#out strength
countries <- nodelist %>% 
  filter(year == 2018, out_str_rank > 80) %>%
  select(name)

plot_outstr <- ggplot(nodelist[nodelist$name %in% countries$name,]) + 
  geom_line(aes(x = year, y = out_str_rank, color = name), size = 1.5) + 
  geom_point(aes(x = year, y = out_str_rank, color = name), size = 4) + 
  theme_bw() +
  labs(x = "Year", y = "Out strength rank", title = "Country rankings by out-strength") + 
  theme(text = element_text(size = 20))

print(plot_outstr)


#total strength
countries <- nodelist %>% 
  filter(year == 2018, tot_str_rank > 80) %>%
  select(name)

plot_totstr <- ggplot(nodelist[nodelist$name %in% countries$name,]) + 
  geom_line(aes(x = year, y = tot_str_rank, color = name), size = 1.5) + 
  geom_point(aes(x = year, y = tot_str_rank, color = name), size = 4) + 
  theme_bw() +
  labs(x = "Year", y = "Total strength rank", title = "Country rankings by total strength") + 
  theme(text = element_text(size = 20))

print(plot_totstr)


#betweenness
countries <- nodelist %>% 
  filter(year == 2018, betweenness_rank > 80) %>%
  select(name)

plot_betweenness <- ggplot(nodelist[nodelist$name %in% countries$name,]) + 
  geom_line(aes(x = year, y = betweenness_rank, color = name), size = 1.5) + 
  geom_point(aes(x = year, y = betweenness_rank, color = name), size = 4) + 
  theme_bw() +
  labs(x = "Year", y = "Betweenness centrality rank", title = "Country rankings by betweenness centrality") + 
  theme(text = element_text(size = 20))

print(plot_betweenness)


#closeness
countries <- nodelist %>% 
  filter(year == 2018, closeness_rank > 80) %>%
  select(name)

plot_closeness <- ggplot(nodelist[nodelist$name %in% countries$name,]) + 
  geom_line(aes(x = year, y = closeness_rank, color = name), size = 1.5) + 
  geom_point(aes(x = year, y = closeness_rank, color = name), size = 4) + 
  theme_bw() +
  labs(x = "Year", y = "closeness centrality rank", title = "Country rankings by closeness centrality") + 
  theme(text = element_text(size = 20))

print(plot_closeness)


#eigen
countries <- nodelist %>% 
  filter(year == 2018, eigen_rank > 80) %>%
  select(name)

plot_eigen <- ggplot(nodelist[nodelist$name %in% countries$name,]) + 
  geom_line(aes(x = year, y = eigen_rank, color = name), size = 1.5) + 
  geom_point(aes(x = year, y = eigen_rank, color = name), size = 4) + 
  theme_bw() +
  labs(x = "Year", y = "Eigen centrality rank", title = "Country rankings by eigen centrality") + 
  theme(text = element_text(size = 20))

print(plot_eigen)

#centralization
##########################################################################################

centralization <- data.frame(betweenness, eigen, closeness)

############################################################ average edge weight over time

general_sum <- edgelist %>%
  group_by(year) %>%
  summarize(avg_weight = mean(weight),
            num_edges = n(), 
            throughput = sum(weight))

weight_plot <- ggplot(general_sum) + 
  geom_line(aes(x = year, y = avg_weight), size = 1.5) +
  geom_point(aes(x = year, y = avg_weight), size = 4) + 
  labs(title = "Average edge weight overtime", x = "Year", y = "Edge weight (kg)") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

print(weight_plot)


edge_plot <- ggplot(general_sum) + 
  geom_line(aes(x = year, y = num_edges), size = 1.5) +
  geom_point(aes(x = year, y = num_edges), size = 4) + 
  labs(title = "Number of edges overtime", x = "Year", y = "Number of edges") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

print(edge_plot)


throughput_plot <- ggplot(general_sum) + 
  geom_line(aes(x = year, y = throughput), size = 1.5) +
  geom_point(aes(x = year, y = throughput), size = 4) + 
  labs(title = "Network throughput overtime", x = "Year", y = "Total global trade (kg)") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

print(throughput_plot)


##### percent changes of the former
weightChange = (general_sum[general_sum$year == 2018, 2] - general_sum[general_sum$year == 1995, 2]) / 
  general_sum[general_sum$year == 1995, 2] * 100
edgeChange = (general_sum[general_sum$year == 2018, 3] - general_sum[general_sum$year == 1995, 3]) / 
  general_sum[general_sum$year == 1995, 3] * 100
throughputChange = (general_sum[general_sum$year == 2018, 4] - general_sum[general_sum$year == 1995, 4]) / 
  general_sum[general_sum$year == 1995, 4] * 100

print(weightChange)
print(edgeChange)
print(throughputChange)

############################################################################### edge distributions

edge_dist <- ggplot(log(edgelist)) + 
  geom_density(aes(x = weight))

print(edge_dist)


####################################################################### Lost edges vs edge weight

edgelist <- edgelist %>% mutate(id = paste(from_name, to_name), lost = 0, gained = 0)

for(y in 1995:2017){
  edges <- edgelist[edgelist$year == y,]$id
  edges_p <- edgelist[edgelist$year == y + 1,]$id
  lost_edges <- edges[!(edges %in% edges_p)]
  gained_edges <- edges_p[!(edges_p %in% edges)]
  edgelist[edgelist$year == y & (edgelist$id %in% lost_edges),6] = 1
  edgelist[edgelist$year == y + 1 & (edgelist$id %in% gained_edges),7] = 1
}

# edgelist$lost <- factor(edgelist$lost, levels = c("0", "1"))
# edgelist$gained <- factor(edgelist$gained, levels = c("0", "1"))



mean(filter(edgelist, year == 1995, lost == 1)$weight)
mean(filter(edgelist, year == 1995, lost == 0)$weight)

d <- edgelist  %>% select(weight, lost)
d$lost <- as.factor(d$lost)

d_id <- createDataPartition(d$lost, p = 0.75, list = FALSE)
d_trn <- d[d_id, ]
d_tst <- d[-d_id, ]

glm_mod = train(
  form = lost ~ weight,
  data = d_trn,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)

results <- cbind(as.data.frame(predict(glm_mod, newdata = d_tst)), d_tst$lost)


d <- d %>% mutate(logistic = (exp(-1.028 + -1.144*(10^-6)*weight)/(1 + exp(-1.028 + -1.144*(10^-6)*weight))))
d$lost <- as.integer(d$lost) - 1

lost_plot <- ggplot(d) +
  geom_point(aes(x = weight, y = lost))
  #geom_line(aes(x = weight, y = logistic))
print(lost_plot)

gained_plot <- ggplot(filter(edgelist, year == 2017)) +
  geom_point(aes(x = weight, y = gained))
print(gained_plot)



############################################################ economic value
thermoplastic_agg <- thermoplastic_agg %>% 
  mutate(unit_price = value / quantity)

price_summary <- thermoplastic_agg %>%
  group_by(t) %>%
  summarize(unit_price = mean(unit_price))

price_plot <- ggplot(price_summary) +
  geom_line(aes(x = t, y = unit_price))
print(price_plot)


########################################################################### exports vs imports drivers
deg_var <- nodelist %>%
  group_by(name) %>%
  summarize(in_var = sd(in_deg), 
            out_var = sd(out_deg), 
            in_var_w = sd(in_str), 
            out_var_w = sd(out_str))
sum(deg_var$in_var)
sum(deg_var$out_var) #in degree variance is much lower, so the network is import driven (maybe?)
mean(deg_var$in_var)
mean(deg_var$out_var)

sum(deg_var$in_var_w)
sum(deg_var$out_var_w) #in degree variance is much lower, so the network is import driven (maybe?)
mean(deg_var$in_var_w)
mean(deg_var$out_var_w)


t.test(deg_var$in_var_w, deg_var$out_var_w, alternative = "two.sided", conf.level = .95)

deg_var_gathered <- gather(deg_var, key = direction, value = variance, 2:5)

deg_var_plot <- ggplot(deg_var_gathered[deg_var_gathered$direction %in% c("in_var", "out_var"),]) + 
  geom_density(aes(x = variance, color = direction, fill = direction), alpha = .1, size = 1) 
  scale_x_log10()

print(deg_var_plot)

degree_deltas <- nodelist %>%
  select(name, year, in_deg, out_deg)

######################################################################### OPEC

opec <- read_csv("Data/opec_prices.csv")
opec <- opec %>% separate(col = Date, into = c("year","month","day"), )

opec_annual <- opec %>%
  group_by(year) %>%
  summarize(mean = mean(Value),
            min = min(Value),
            max = max(Value),
            stdev = sd(Value))

opec_network <- nodelist %>% 
  filter((name %in% deg_var[deg_var$out_var > 24.6,]$name)) %>% 
  select(out_deg, out_str, in_deg, in_str, name, year)

opec_plot <- ggplot(opec_network) + 
  geom_line(aes(x = year, y = out_deg, color = name), size = 1,alpha = 0.2) +
  theme(legend.position = "none")
print(opec_plot)

opec_price_plot <- ggplot(opec_annual) + 
  geom_line(aes(x = year, y = mean))
print(opec_price_plot)

################################################################ assortativity
assortativity_degree(graph1995,directed = TRUE)
assortativity(graph2018, types1 = V(graph2018)$in_deg, types2 = V(graph2018)$out_deg)

################################################################ centrality correlations
corr_centrality <-as.data.frame(cor(select(nodes2018, in_deg, out_deg, in_str, out_str, eigen)))
corr_centrality_plot <- pairs(select(nodes2018, in_deg, out_deg, in_str, out_str))
