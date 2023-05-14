library(tidyverse)
library(ggplot2)
library(igraph)
library(tidygraph)

waste <- read_csv("Data/waste.csv")
production <- read_csv("Data/waste_generation.csv")

waste_agg <- waste %>%
  group_by(t, exporter, importer) %>%
  summarize(q = sum(q), v = sum(v))


countries <- unique(c(waste_agg$importer, waste_agg$exporter))
l = length(countries)
net_flows <- data.frame(t = numeric(l*24),
                        country = character(l*24))
for(i in 1995:2018){
  time = i - 1995
  net_flows$t[(time*l):((time+1)*l)] <- i
}
net_flows$country <- rep(countries, times = 24)



net_imp <- waste_agg %>%
  group_by(t, importer) %>%
  summarize(q = sum(q), v = sum(v)) %>%
  rename(country = importer, q_imports = q, v_imports = v)
net_exp <- waste_agg %>%
  group_by(t, exporter) %>%
  summarize(q = sum(q), v = sum(v)) %>%
  rename(country = exporter, q_exports = q, v_exports = v)


net_flows <- net_flows %>%
  left_join(net_imp) %>%
  left_join(net_exp)


data <- filter(net_flows, t == 2018) %>%
  left_join(select(production, code, waste), by = c("country" = "code")) %>%
  mutate(net_exp_q = q_exports - q_imports, 
         net_exp_v = v_exports - v_imports, 
         exp_frac = net_exp_q / waste, 
         exp_price = v_exports / q_exports, 
         imp_price = v_imports / q_imports) 

priceVsRatioE <- ggplot(data) + 
  geom_point(aes(x = exp_price, y = exp_frac))
priceVsRatioE

priceVsRatioI <- ggplot(data) + 
  geom_point(aes(x = imp_price, y = exp_frac))
priceVsRatioI

ratioDensity <- ggplot(filter(data, exp_frac < 2, exp_frac > -1)) + 
  geom_density(aes(x = exp_frac)) +
  scale_x_log10()
ratioDensity


qVsRatio <- ggplot(filter(data, exp_frac > 0)) + 
  geom_point(aes(x = exp_frac, y = q_exports)) 
qVsRatio


