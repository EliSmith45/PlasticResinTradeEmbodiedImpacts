library(tidyverse)
library(ggplot2)


waste <- read_csv("Data/waste.csv") %>% mutate(unit_val = v/q)
mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>%
  select(Code, "Share of plastic inadequately managed (%)") %>%
  rename(mismanaged = "Share of plastic inadequately managed (%)")
mismanaged$mismanaged <- mismanaged$mismanaged / 100

x = .05

mismanagement_flows <- waste %>% 
  left_join(mismanaged, by = c("exporter" = "Code")) %>%
  rename(exp_mis = mismanaged) %>% 
  left_join(mismanaged, by = c("importer" = "Code")) %>%
  rename(imp_mis = mismanaged)
mismanagement_flows <- na.omit(mismanagement_flows)
mismanagement_flows <- mismanagement_flows %>% mutate(quality = "a")
mismanagement_flows$quality <-ifelse(mismanagement_flows$exp_mis - mismanagement_flows$imp_mis >= x, "good", 
                                     ifelse(mismanagement_flows$imp_mis - mismanagement_flows$exp_mis >= x, "bad", "neutral"))

annual <- mismanagement_flows %>% group_by(t) %>% summarize(total_q = sum(q))
mismanagement_flows <- left_join(mismanagement_flows, annual)
mismanagement_flows <- mutate(mismanagement_flows, unit_val_w = (q/total_q) * unit_val)

ethyl <- filter(mismanagement_flows, k ==391510) 
styr <- filter(mismanagement_flows, k ==391520)
pvc <- filter(mismanagement_flows, k ==391530)
nec <- filter(mismanagement_flows, k ==391590)

ethyl$t <- as.factor(ethyl$t)
styr$t <- as.factor(styr$t)
pvc$t <- as.factor(pvc$t)
nec$t <- as.factor(nec$t)

price_ethyl <- ethyl %>%
  group_by(importer) %>%
  summarize(q = sum(q), ave_price = mean(unit_val))
price_styr <- styr %>%
  group_by(importer) %>%
  summarize(q = sum(q), ave_price = mean(unit_val))
price_pvc <- pvc %>%
  group_by(importer) %>%
  summarize(q = sum(q), ave_price = mean(unit_val))
price_nec <- nec %>%
  group_by(importer) %>%
  summarize(q = sum(q), ave_price = mean(unit_val))


price_ethyl_plot <- ggplot(price_ethyl) + 
  geom_point(aes(x = ave_price, y = q)) + 
  scale_x_log10() + 
  scale_y_log10()
price_ethyl_plot

price_styr_plot <- ggplot(price_styr) + 
  geom_point(aes(x = ave_price, y = q)) + 
  scale_x_log10() + 
  scale_y_log10()
price_styr_plot

price_pvc_plot <- ggplot(price_pvc) + 
  geom_point(aes(x = ave_price, y = q)) + 
  scale_x_log10() + 
  scale_y_log10()
price_pvc_plot

price_nec_plot <- ggplot(price_nec) + 
  geom_point(aes(x = ave_price, y = q)) + 
  scale_x_log10() + 
  scale_y_log10()
price_nec_plot



dist_ethyl <- ggplot(filter(ethyl, (as.numeric(t) + 1) %% 5 == 0)) + 
  geom_boxplot(aes(y = unit_val, color = t), size = 1.25) +
  scale_y_log10() 
dist_ethyl

dist_styr <- ggplot(styr) + 
  geom_density(aes(x = unit_val, color = t), size = 1.25) +
  scale_x_log10() 
dist_styr

dist_pvc <- ggplot(pvc) + 
  geom_density(aes(x = unit_val, color = t), size = 1.25) +
  scale_x_log10() 
dist_pvc

dist_nec <- ggplot(nec) + 
  geom_density(aes(x = unit_val, color = t), size = 1.25) +
  scale_x_log10() 
dist_nec

year = 2018

dist_ethyl_q <- ggplot(filter(ethyl, t == year)) + 
  geom_boxplot(aes(y = unit_val, color = quality), size = 1.25) +
  scale_y_log10() + 
  labs(title = "Edge unit price distribution, ethylene waste (unweighted)") + 
  geom_text(aes(x = 0, y = .01), label = paste(paste((sum(filter(ethyl, t == year)$q) / sum(filter(waste, t == year)$q) * 100) %>% round(0), "%", sep = ""), "of total waste trade in", year), size = 5)+ 
  theme(text = element_text(size = 18))
dist_ethyl_q

dist_styr_q <- ggplot(styr) + 
  geom_density(aes(x = unit_val_w, color = quality), size = 1.25) +
  scale_x_log10() + 
  labs(title = "Edge unit price distribution, styrene waste") + 
  geom_text(aes(x = 55, y = 1.25), label = paste(paste((sum(filter(styr, t == year)$q) / sum(filter(waste, t == year)$q) * 100) %>% round(0), "%", sep = ""), "of total waste trade in", year), size = 5) + 
  theme(text = element_text(size = 18))
dist_styr_q

dist_pvc_q <- ggplot(pvc) + 
  geom_density(aes(x = unit_val_w, color = quality), size = 1.25) +
  scale_x_log10() + 
  labs(title = "Edge unit price distribution, PVC waste") + 
  geom_text(aes(x = 55, y = 1.25), label = paste(paste((sum(filter(pvc, t == year)$q) / sum(filter(waste, t == year)$q) * 100) %>% round(0), "%", sep = ""), "of total waste trade in", year), size = 5)
dist_pvc_q

dist_nec_q <- ggplot(nec) + 
  geom_boxplot(aes(y = unit_val_w, color = quality), size = 1.25) +
  scale_y_log10() + 
  labs(title = "Edge unit price distribution, non-specified waste") + 
  geom_text(aes(x = 0, y = .75), label = paste(paste((sum(filter(nec, t == year)$q) / sum(filter(waste, t == year)$q) * 100) %>% round(0), "%", sep = ""), "of total waste trade in", year), size = 5)
dist_nec_q


#######################################import and export costs

