library(tidyverse)
library(ggplot2)
library(rjson)
library(here)
library(janitor)


flows <- read_csv(here("Data", "cleanRequestNew.csv"))
waste17 <- read_csv(here("Data","waste171819.csv"))
waste16 <- flows %>% filter(commodity_code == "3915")
waste <- rbind(waste16, waste17)
waste <- waste %>% filter(!(is.na(reporter)))

countries <- read_csv(here("Data","country_data.csv")) %>% clean_names() %>% select(country, region, gdp_per_capita)

reps <- unique(waste$reporter)
parts <- unique(waste$partner)
