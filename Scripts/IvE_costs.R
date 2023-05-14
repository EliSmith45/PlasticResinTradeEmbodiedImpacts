library(tidyverse)
library(ggplot2)


waste <- read_csv("Data/waste.csv") 
mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>%
  select(Code, "Share of plastic inadequately managed (%)") %>%
  rename(mismanaged = "Share of plastic inadequately managed (%)")
mismanaged$mismanaged <- mismanaged$mismanaged / 100

x =.05

mismanagement_flows <- waste %>% 
  left_join(mismanaged, by = c("exporter" = "Code")) %>%
  rename(exp_mis = mismanaged) %>% 
  left_join(mismanaged, by = c("importer" = "Code")) %>%
  rename(imp_mis = mismanaged)
mismanagement_flows <- na.omit(mismanagement_flows)
mismanagement_flows <- mismanagement_flows %>% mutate(quality = "a")
mismanagement_flows$quality <-ifelse(mismanagement_flows$exp_mis - mismanagement_flows$imp_mis >= x, "good", 
                                     ifelse(mismanagement_flows$imp_mis - mismanagement_flows$exp_mis >= x, "bad", "neutral"))

annual_i <- waste %>% group_by(t, k, importer) %>% summarize(imports = sum(q), ave_price_i = mean(v) / mean(q))
annual_e <- waste %>% group_by(t, k, exporter) %>% summarize(exports = sum(q), ave_price_e = mean(v) / mean(q))
annual <- full_join(annual_i, annual_e, by = c("t" = "t", "k" = "k", "importer" = "exporter")) %>%
  mutate(diff = ave_price_i - ave_price_e) %>%
  rename(country = importer) 

annual_prices <- annual %>% 
  replace_na(list(imports = 0, exports = 0, ave_price_e = 0, ave_price_i = 0)) %>%
  mutate(trade = imports + exports) %>%
  mutate(price = (ave_price_i * (imports / trade)) + (ave_price_e * (exports / trade)))


annual_i_agg <- waste %>% filter(q >= 50) %>% group_by(t, importer) %>% summarize(imports = sum(q), ave_price_i = mean(v, na.rm = T) / mean(q, na.rm = T))
annual_e_agg <- waste %>% filter(q >= 50) %>% group_by(t, exporter) %>% summarize(exports = sum(q), ave_price_e = mean(v, na.rm = T) / mean(q, na.rm = T))
annual_agg <- full_join(annual_i_agg, annual_e_agg, by = c("t" = "t", "importer" = "exporter")) %>%
  mutate(diff = ave_price_i - ave_price_e) %>%
  rename(country = importer)
summary(annual_agg)
# waste <- left_join(waste, annual_i, by = c("t", "k", "importer")) %>% left_join(annual_e, by = c("t", "k", "exporter"))
# write_csv(waste, "./Data/waste.csv")

annual_by_quality <- mismanagement_flows %>% group_by(t, k, quality) %>% summarize(ave_price = mean(v) / mean(q)) 
year = 2018

diff_dist <- ggplot(annual_agg %>% filter(t == year)) + 
  geom_density(aes(x = diff)) + 
  scale_x_log10()
diff_dist

prices <- ggplot(annual_agg %>% filter(t == year)) + 
  geom_point(aes(x = ave_price_e, y = ave_price_i)) + 
  scale_y_log10() +
  scale_x_log10()
prices

mean(annual_e_agg$ave_price_e, na.rm = T)
mean(annual_i_agg$ave_price_i, na.rm = T)

annual_by_quality_m <- annual_by_quality
annual_by_quality_m$k = as.character(annual_by_quality_m$k)
annual_by_quality_m[annual_by_quality_m$k == 391510, 2] <- "Polyethylene"
annual_by_quality_m[annual_by_quality_m$k == 391520, 2] <- "Polystyrene"
annual_by_quality_m[annual_by_quality_m$k == 391530, 2] <- "PVC"
annual_by_quality_m[annual_by_quality_m$k == 391590, 2] <- "All other plastics"

qualityPlot <- ggplot(annual_by_quality_m) +
  geom_line(aes(x = t, y = ave_price, color = quality), size = 1.25) + 
  geom_point(aes(x = t, y = ave_price, color = quality), size = 2.5) + 
  labs(x = "year", y = "Price, USD/kg", title = "Plastic prices") + 
  facet_wrap(k~.) + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 50, hjust = 1)) 
qualityPlot

topExp <- data %>% arrange(-net_exp_q) %>% select(country)
topExp <- topExp[1:10,]

topImp <- data %>% arrange(net_exp_q) %>% select(country)
topImp <- topImp[1:5,]

commodity = 391590

exp_diffs <- annual_prices %>% filter(k == commodity, t == 2018, country %in% topExp) 
imp_diffs <- annual_prices %>% filter(k == commodity, t == 2018, country %in% topImp)

mean(exp_diffs$price, na.rm = T)
mean(imp_diffs$price, na.rm = T)



sd(exp_diffs$diff, na.rm = T)
sd(imp_diffs$diff, na.rm = T)

t.test(exp_diffs$diff, imp_diffs$diff, alternative = "greater")
