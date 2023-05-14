library(tidyverse)
library(igraph)
library(tidygraph)
library(RColorBrewer)

# data <-data.frame()
# 
# for(y in 1995:2018){
#   
#   d <- read_csv(paste("./Data/BACI_HS92_Y", y, "_V202001.csv", sep = ""))
#   d <- filter(d, substr(k, 0, 4) == "3915")
#   data <- rbind(data, d)
#   
#   
# }
# 
# reporters <- read_csv("./Data/ISO_codes_countries.csv")
# 
# data <- data %>%
#   left_join(select(reporters, "Country Code", "ISO3-digit Alpha"), by = c("i" = "Country Code")) %>%
#   rename(exporter = "ISO3-digit Alpha") %>%
#   left_join(select(reporters, "Country Code", "ISO3-digit Alpha"), by = c("j" = "Country Code")) %>%
#   rename(importer = "ISO3-digit Alpha") %>%
#   select(-i, -j)
# 
# write_csv(data, "./Data/baci_waste_iso.csv")

data <- read_csv("./Data/baci_waste_iso.csv")

agg <- data %>%
  group_by(exporter, importer, t) %>%
  summarize(value = sum(v, na.rm = TRUE),
            quantity = sum(q, na.rm = TRUE))

world_flows <- agg %>%
  group_by(t) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE),
            value = sum(value, na.rm = TRUE))

top_exp <- agg %>%
  group_by(t, exporter) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE),
            value = sum(value, na.rm = TRUE)) %>%
  arrange(-quantity)

drivers <- c("HKG", "USA", "JPN", "DEU", "GBR", "NLD", "BEL", "MEX", "FRA")
driver_flows <- agg %>%
  filter((exporter %in% drivers)) %>%
  group_by(t) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE),
            value = sum(value, na.rm = TRUE))
  
variance <- data.frame(year = 1995:2018,
                       world = (world_flows$quantity-lag(world_flows$quantity))/lag(world_flows$quantity )*100,
                       drivers = (driver_flows$quantity-lag(driver_flows$quantity))/lag(driver_flows$quantity) * 100) %>%
  gather(key = entity, value = change, 2:3)

variance <- filter(variance, !(is.na(change)))

varPlot <- ggplot(variance) + 
  geom_line(aes(x = year, y = change, color = entity), size = 1.25) +
  geom_point(aes(x = year, y = change, color = entity), size = 2.5) +
  labs(title = "% Change in trade volume", y = "change (%)") + 
  theme_classic() + 
  theme(text = element_text(size = 20)) 

print(varPlot)

cor(filter(variance, entity =="world")$change, filter(variance, entity =="drivers")$change)

contribution = sum(filter(top_exp, t == 2015, exporter %in% drivers)$quantity) / sum(top_exp[top_exp$t == 2015,3])
print(contribution)

exp <- top_exp %>% filter(exporter %in% drivers)

expPlot <- ggplot(exp) + 
  geom_line(aes(x = t, y = quantity, color = exporter), size = 1.25) +
  geom_point(aes(x = t, y = quantity, color = exporter), size = 2.5) +
  labs(title = "Exports by year, top exporters", x = "year", y = "plastic waste exports (USD)") + 
  theme_classic() +
  theme(text = element_text(size = 20))

print(expPlot)

imports <- data %>%
  group_by(importer, t) %>% 
  summarize(val = sum(v, na.rm = T))

chn <- ggplot(filter(imports, importer == "CHN")) + 
  geom_line(aes(x = t, y = val), color = "red", size = 2.5) + 
  geom_point(aes(x = t, y = val), color = "red", size = 5) + 
  labs(title = "China waste imports", x = "year", y = "plastic waste imports (USD)") + 
  theme_classic() +
  geom_segment(aes(x = 2010, xend = 2013, yend = 3680000, y =3500000 ), size = 1) +
  geom_text(aes(label = "Start of China's Green Fence Campaign", x = 2003.5, y = 3500000), size = 6) +
  geom_segment(aes(x = 2000, xend = 2001, yend = 400000, y =800000 ), size = 1) +
  geom_text(aes(label = "China joins WTO", x = 1998, y = 900000), size = 6) +
  theme(text = element_text(size = 20))


print(chn)

#focusing on Hong Kong
hkg_exp <- data %>% filter(exporter == "HKG") %>%
  group_by(t, importer, exporter) %>%
  summarize(val = sum(v, na.rm = T))

hkg_imp <- data %>% filter(importer == "HKG") %>%
  group_by(t, importer, exporter) %>%
  summarize(val = sum(v, na.rm = T))

hkg_imp_y <- hkg_imp %>%
  group_by(t) %>%
  summarize(val = sum(val))


hkg_exp_CHN <- hkg_exp %>% filter(importer == "CHN") 
hkg_exp_CHN <- ungroup(hkg_exp_CHN)
hkg_exp_CHN <- select(hkg_exp_CHN, t, val) %>% rename(hkg_exports = val)

hkg_imp_y <- rename(hkg_imp_y, hkg_imports = val)
hkg_chn <- left_join(hkg_imp_y, hkg_exp_CHN)
hkg_chn <- gather(hkg_chn, key = "flow", value = "value", 2:3)

hkg_chn_plot <- ggplot(hkg_chn) + 
  geom_line(aes(x = t, y = value, color = flow), size = 1.25) + 
  geom_point(aes(x = t, y = value, color = flow), size = 2.5) + 
  theme_classic() + 
  theme(text = element_text(size = 20)) + 
  labs(title = "Hong Kong trade patterns", x = "year", y = "quantity of plastic waste traded (USD)") 
print(hkg_chn_plot)

cor(filter(hkg_chn, flow == "hkg_imports")$value, filter(hkg_chn, flow == "hkg_exports")$value)

hkg <- ungroup(hkg)
hkg2007 <- hkg %>%
  filter(t == 2007, val >= 5000) %>%
  select(-t)

hkg_net2007 <- as_tbl_graph(graph_from_data_frame(select(hkg2007, exporter, importer, val)))
n <- as.list(hkg_net2007)$nodes
e <- as.list(hkg_net2007)$edges

V(hkg_net2007)$size = 10
E(hkg_net2007)$weight <- E(hkg_net2007)$val 

plot(hkg_net2007, layout = layout_with_dh(hkg_net2007))

hkg2016 <- hkg %>%
  filter(t == 2016, val >= 5000) %>%
  select(-t)

hkg_net2016 <- as_tbl_graph(graph_from_data_frame(select(hkg2016, exporter, importer, val)))


V(hkg_net2016)$size = 10
E(hkg_net2016)$weight <- E(hkg_net2016)$val 

plot(hkg_net2007, layout = layout_with_dh(hkg_net2016))

########################################################################################################
#economics

dAgg <- data %>%
  group_by(t, exporter, importer) %>%
  summarize(v = sum(v, na.rm = T), q = sum(q, na.rm = T))

data$k <- as.factor(data$k)

quantVal <- ggplot(data) + 
  geom_point(aes(x = q, y = v, color = k)) + 
  geom_smooth(aes(x = q, y = v, color = k)) +
  labs(title = "Value vs. Quantity traded")

print(quantVal)

quantValAgg <- ggplot(dAgg) + 
  geom_point(aes(x = q, y = v)) +
  geom_smooth(aes(x = q, y = v))

print(quantValAgg)
