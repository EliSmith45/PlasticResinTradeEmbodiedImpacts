library(tidyverse)
library(stringr)
library(corpustools)
library(igraph)
library(ggplot2)
library(GGally)
library(tidygraph)
library(WDI)
library(janitor)

#read country ISO codes
iso <- read_csv("./Data/ISO_codes_countries.csv")

#Type in countries/regions that you would like to ignore
ignoreList <- c("All", "World")

#get all the reporters and filter out countries/regions from ignoreList
reporters <- getReporters() 
reporters <- reporters %>%
  filter(!(V2 %in% ignoreList)) %>%
  rename(code = V1, countries = V2)

reporters$code <- as.numeric(reporters$code)

#Creates the data frame of reporters and partners. It is the list of reporters cross-joined with itself
flowDataFrame <- as.data.frame(CJ(Reporter = as.vector(reporters$countries), Partner = as.vector(reporters$countries)))

#load all BACI data
baci <- read_csv("./Data/BACI_HS92_Y2016_V202001.csv") %>% rename(year = t, exporter = i, importer = j, commodity = k, value = v, quantity = q)
baci$commodity <- as.integer(baci$commodity)

baci <- left_join(baci, reporters, by = c("exporter" = "code"))
baci <- baci %>% rename(exp_name = countries)

baci <- left_join(baci, reporters, by = c("importer" = "code"))
baci <- baci %>% rename(imp_name = countries)

#aggregate by economic value over all primary plastics
primary <- filter(baci, commodity < 391200 & commodity > 390000)
primaryAgg <- primary %>%
  group_by(exp_name, imp_name) %>%
  summarize(val = sum(value, na.rm = TRUE))
primaryAgg <- left_join(flowDataFrame, primaryAgg, by = c("Reporter" = "exp_name", "Partner" = "imp_name"))

#create a sparse adjacency matrix
adjMat <- spread(primaryAgg, key = Partner, value = val)
rownames(adjMat) <- adjMat[,1]
adjMat[,1] <- NULL
adjMat <- as.matrix(adjMat)
adjMat[is.na(adjMat)] = 0
adjMat <- Matrix(adjMat, sparse = TRUE)


network <- as_tbl_graph(graph_from_adjacency_matrix(adjMat, mode = "directed", weighted = TRUE, add.colnames = NULL, add.rownames = NULL))


#add strenth/degree data to nodes
network <- network %>%
  activate(nodes) %>%
  mutate(in_strength = strength(network, mode = "in"), 
         out_strength = strength(network, mode = "out"),
         in_degree = degree(network, mode = "in"),
         out_degree = degree(network, mode = "out"))
nodes <- as.list(network)$nodes
edges <- as.list(network)$edges

nodes <- left_join(nodes, iso %>% select("Country Name, Abbreviation", "Country Code", "ISO2-digit Alpha", "ISO3-digit Alpha"), by = c("name" = "Country Name, Abbreviation"))
nodes <- clean_names(nodes)

wdi <- WDI_data

nodes <- left_join(nodes, as.data.frame(wdi$country) %>% select(iso2c, region, income), by = "iso2c")
nodes <- nodes %>% filter(in_strength != 0 & out_strength != 0)

oil <- as.data.frame(WDIsearch("oil"))
indicators <- WDI(country = "all", indicator = c("NW.NCA.SAOI.TO","NW.NCA.SAGA.TO","NW.NCA.SACO.TO","EG.USE.PCAP.KG.OE"), start = 2014, end = 2014)

nodes <- rename(nodes, iso2c = iso2_digit_alpha )
nodes <- left_join(nodes, indicators, by = "iso2c")
nodes <- nodes %>% rename(coal = NW.NCA.SACO.TO, oil = NW.NCA.SAOI.TO, gas = NW.NCA.SAGA.TO, energy_pcap = EG.USE.PCAP.KG.OE) %>%
  select(country, country_code, iso2c, region, income, in_strength, out_strength, in_degree, out_degree, coal, oil, gas, energy_pcap )

nodes$income <- Factor(nodes$income, levels = c("Aggregates, Low income, Lower middle income, Upper middle income, High income"))

pairs <- ggpairs(nodes, columns = 5:8, mapping = aes(color = region, size = income))

print(pairs)



##########################################################link strenght distribution

