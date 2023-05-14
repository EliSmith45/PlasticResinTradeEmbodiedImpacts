library(tidyverse)
library(ggplot2)
library(igraph)
library(tidygraph)
library(leaps)

waste <- read_csv("Data/waste.csv")
bad <- read_csv("Data/bad.csv")

waste_agg <- waste %>% 
  group_by(t, exporter, importer) %>%
  summarize(value = sum(v),
            quantity = sum(q)) %>%
  ungroup()

global_bad <- bad %>% 
  group_by(t) %>%
  summarize(global = sum(quantity))

exports <- waste_agg %>%
  group_by(t, exporter) %>%
  summarize(quantity = sum(quantity, na.rm = T)) %>%
  ungroup() %>%
  spread(exporter, quantity)

##################################### is this correct?
exports[is.na(exports)] <- 0

model_data <- left_join(exports, global_bad) %>% select(-"N/A", -t)
f <- as.formula(paste("global ~ ", paste(names(model_data[,2:211]), collapse = " + "), sep = ""))
model <- regsubsets(x = f, data = model_data, nvmax = 8, method = "seqrep")
summary(model)

