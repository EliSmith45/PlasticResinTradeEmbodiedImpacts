library(igraph)
library(tidygraph)
library(tidygraph)
library(magick)
library(ggplot2)


waste_agg <- read_csv("Data/waste.csv") %>%
  group_by(t, exporter, importer) %>%
  summarize(q = sum(q), v = sum(v)) %>%
  ungroup()

mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>%
  select(Code, "Share of plastic inadequately managed (%)") %>%
  rename(mismanaged = "Share of plastic inadequately managed (%)")
mismanaged$mismanaged <- mismanaged$mismanaged / 100

dataframe <- waste_agg %>%
  filter(t == 2018) %>%
  select(exporter, importer, q) %>% 
  rename(weight = q)

dataframe$weight <- dataframe$weight * 1000

graph2018 <- graph_from_data_frame(dataframe) %>% simplify() %>% as_tbl_graph()

V#(graph)$name[V(graph)$name == "N/A"] = "TWN"


coordinates <- read_csv("Data/countries_regions.csv")

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "bottom")+
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

edges <- as.list(graph2018)$edges
nodes <- as.list(graph2018)$nodes

for(y in 2018:2018){
  graph <- get(paste("graph", y, sep = ""))
  edges <- as.list(graph)$edges
  nodes <- as.list(graph)$nodes
  
  edges$From <- nodes$name[edges$from]
  edges$To <- nodes$name[edges$to]
  edges$from <- NULL
  edges$to <- NULL
  
  edges <- edges %>% 
    left_join(select(coordinates, three_digit, latitude, longitude), by = c("From" = "three_digit")) %>%
    rename(from_lat = latitude, from_lon = longitude) %>%
    left_join(select(coordinates, three_digit, latitude, longitude), by = c("To" = "three_digit")) %>%
    rename(to_lat = latitude, to_lon = longitude)
  
  nodes <- left_join(nodes, select(coordinates, three_digit, latitude, longitude), by = c("name" = "three_digit"))
  
  edges <- na.omit(edges)
  nodes <- na.omit(nodes)
  assign(paste("edges", y, sep = ""), edges)
  assign(paste("nodes", y, sep = ""), nodes)
}



mapplot <- ggplot(nodes2018) + country_shapes +
  geom_curve(aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat, size = weight),
             data =filter(edges2018, weight >= 0.001*sum(edges2018$weight)), curvature = 0.33) +
  scale_size_continuous(guide = FALSE, range = c(0.1, 3)) + # scale for edge widths
  geom_point(aes(x = longitude, y = latitude),           # draw nodes
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.5, size = 2.5) + 
  #geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
  #          hjust = 0, nudge_x = 1, nudge_y = 4,
  #         size = 3, color = "white", fontface = "bold") +
  mapcoords + maptheme 

print(mapplot)
