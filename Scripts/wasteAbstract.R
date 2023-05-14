library(tidyverse)
library(ggplot2)
library(rjson)
library(here)
library(janitor)


imp2010 <- read_csv("./Data/cleanRequestNew.csv") %>% filter(year == 2010, commodity_code == 3915, trade_flow == "Import")
exp2010 <- read_csv("./Data/cleanRequestNew.csv") %>% filter(year == 2010, commodity_code == 3915, trade_flow == "Export")
mismanagement <- read_csv("./Data/mismanagedPlasticWaste.csv") %>% clean_names() %>% rename(mismanaged = share_of_plastic_inadequately_managed_percent)
mismanagement$mismanaged <- mismanagement$mismanaged / 100

imp2010 <- left_join(imp2010, mismanagement %>% select(entity, mismanaged), by = c("reporter" = "entity")) 
imp2010 <- imp2010 %>% rename(impMismanaged = mismanaged)

imp2010 <- left_join(imp2010, mismanagement %>% select(entity, mismanaged), by = c("partner" = "entity")) 
imp2010 <- imp2010 %>% rename(expMismanaged = mismanaged)

imp2010 <- imp2010 %>% mutate(diff = impMismanaged - expMismanaged, extra = qty * diff)



exp2010 <- left_join(exp2010, mismanagement %>% select(entity, mismanaged), by = c("reporter" = "entity")) 
exp2010 <- exp2010 %>% rename(expMismanaged = mismanaged)

exp2010 <- left_join(exp2010, mismanagement %>% select(entity, mismanaged), by = c("partner" = "entity")) 
exp2010 <- exp2010 %>% rename(impMismanaged = mismanaged)

exp2010 <- exp2010 %>% mutate(diff = impMismanaged - expMismanaged, extra = qty * diff)



topExp <- imp2010 %>%
  group_by(partner) %>%
  summarize(exports = sum(qty, na.rm = T)) %>%
  arrange(-exports)


topImp <- imp2010 %>%
  group_by(reporter) %>%
  summarize(imports = sum(qty, na.rm = T)) %>%
  arrange(-imports)

f <- left_join(topExp, topImp, by = c("partner" = "reporter"))

f <- f %>% mutate(netImp = imports - exports, netExp = exports-imports) 

f <- f %>% arrange(-netImp)

imp2010<- imp2010 %>% arrange(-qty)
