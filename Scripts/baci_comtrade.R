library(igraph)
library(Matrix)
library(tidygraph)
library(tidyverse)
library(fitdistrplus)

baci <- read_csv("./Data/BACI_HS92_Y2016_V202001.csv")
baci <- filter(baci, k %in% c("390110", "390120", "390210", "390760", "390311", "390410"))
baci$k = as.numeric(baci$k)

comtrade <- read_csv("./Data/cleanRequestNew.csv")
reporters <- read_csv("./Data/reporters.csv")

comtrade <- left_join(comtrade, reporters, by = c("reporter" = "countries"))
comtrade <- rename(comtrade, rep_code = code)
comtrade <- left_join(comtrade, reporters, by = c("partner" = "countries"))
comtrade <- rename(comtrade, prt_code = code)
comtrade_imp <- filter(comtrade, trade_flow == "Import", year == 2016, commodity_code != 3915)
comtrade_exp <- filter(comtrade, trade_flow == "Export", year == 2016, commodity_code != 3915)


#check import data discrepancies
exp_diff <- left_join(select(baci,-t), 
                      select(comtrade_exp, rep_code, prt_code, commodity_code, qty, trade_value_us),
                      by = c("i" = "rep_code", "j" = "prt_code", "k" = "commodity_code"))
exp_diff$v <- exp_diff$v * 1000
exp_diff$q <- exp_diff$q * 1000
exp_diff <- filter(exp_diff, !(i %in% c(58, 56, 442)), !(j %in% c(58, 56, 442)))

exp_diff <- mutate(exp_diff, qty_diff = q - qty, value_diff = v - trade_value_us)
hist(exp_diff$qty_diff, breaks = 50, xlim = range(-50000000,50000000), freq = FALSE)

exp_qty_plot <- ggplot(exp_diff) + 
  geom_histogram(aes(x = qty_diff), binwidth = 5000)
print(exp_qty_plot)

summary(exp_diff$qty_diff)
summary(exp_diff$value_diff)

#check export data discrepancies
imp_diff <- left_join(select(baci,-t), 
                      select(comtrade_imp, rep_code, prt_code, commodity_code, qty, trade_value_us),
                      by = c("i" = "prt_code", "j" = "rep_code", "k" = "commodity_code"))
imp_diff$v <- imp_diff$v * 1000
imp_diff$q <- imp_diff$q * 1000
imp_diff <- filter(imp_diff, !(i %in% c(58, 56, 442)), !(j %in% c(58, 56, 442)))

imp_diff <- mutate(imp_diff, qty_diff = q - qty, value_diff = v - trade_value_us)

imp_qty_plot <- ggplot(imp_diff) + 
  geom_histogram(aes(x = qty_diff), binwidth = 1000)
print(imp_qty_plot)

summary(imp_diff$qty_diff)
summary(imp_diff$value_diff)

#############################################################################
lci <- data.frame(k = c("390110", "390120", "390210", "390760", "390311", "390410"),
                  gwp = c(2.14, 1.76, 1.82, 1.73, 3.37, 2.14))
baci$k <- as.factor(baci$k)

baci <- left_join(baci, lci)
baci <- mutate(baci, co2 = q * 1000 * gwp)

baciMatrix <- baci %>%
  select(-t) %>%
  group_by(i, j) %>%
  summarize(GWP = sum(co2, na.rm = TRUE))


baciMatrix <- baciMatrix %>%
  left_join(reporters, by = c("i" = "code")) %>%
  rename(exporter = countries) %>%
  left_join(reporters, by = c("j" = "code")) %>%
  rename(importer = countries)

baciMatrix <- select(baciMatrix, exporter, importer, GWP) 
edgelist <- baciMatrix %>% ungroup() %>% select(-i)
edgelist <- rename(edgelist, weight = GWP)

network <- graph_from_data_frame(edgelist)
network <- as_tbl_graph(network)

V(network)$strength <- strength(network, mode = "total")

nodes <- as.list(network)$nodes
edges <- as.list(network)$edges

a <- edges %>%
  arrange(-weight)
b <- a[1:267,]
c <- as.data.frame(c(b$from, b$to))
length(unique(c[,1]))
d <- E(network)[E(network)$weight >= min(b$weight)]

subnetwork <- subgraph.edges(network, eids = d)
subnetwork <- as_tbl_graph(subnetwork)

s_nodes <- as.list(subnetwork)$nodes
s_edges <- as.list(subnetwork)$edges

d <- as.data.frame(as.matrix(subnetwork[]))
write.csv(d, "./Data/top_flows.csv")


mat <- read_csv("./Data/top_flows.csv")
mat <- as.data.frame(mat)
rownames(mat) <- mat[,1]
mat[,1] <- NULL
mat <- Matrix(as.matrix(mat), sparse = TRUE)

net <- graph_from_adjacency_matrix(mat)

##############################################
#link distribution

descdist(edgelist$weight, boot = 1000)





