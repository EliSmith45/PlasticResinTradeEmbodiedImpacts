library(tidyverse)
library(readxl)
library(janitor)

countries_regions <- read_csv("./Data/country_data.csv")
latlong <- read_csv("./Data/countrylatlong.csv")
country_codes <- read_excel("./Data/ISO_codes_countries.xlsx") %>% clean_names()
flows <- read_csv("./Data/cleanRequestNew.csv")

flows <- left_join(flows, country_codes %>% 
                     select(country_name_abbreviation, iso2_digit_alpha), by = c("reporter" = "country_name_abbreviation")) %>%
                     rename(reporter_code = iso2_digit_alpha)

flows <- left_join(flows, country_codes %>% 
                     select(country_name_abbreviation, iso2_digit_alpha), by = c("partner" = "country_name_abbreviation")) %>%
                     rename(partner_code = iso2_digit_alpha)



a <- flows[flows$partner == "Other Africa, nes",]
b <- flows[flows$partner == "Oceania, nes",]

countries_regions <- left_join(countries_regions, latlong, by = c("Country" = "name"))

write.csv(countries_regions, "./Data/countries_regions.csv")


flows <- left_join(flows, countries_regions %>% select(country, Region), by = c("reporter_code" = "country"))
flows <- flows %>% rename(reporter_region = Region)

flows <- left_join(flows, countries_regions %>% select(country, Region), by = c("partner_code" = "country"))
flows <- flows %>% rename(partner_region = Region)


flows[flows$partner == "Other Asia, nes", "partner_region"] = "ASIA (EX. NEAR EAST)"
flows[flows$partner == "Other Africa, nes", "partner_region"] = "ASIA (EX. NEAR EAST)"


flows2016 <- flows %>% filter(year == "2016")
c <- flows[flows$partner == "N/A" | flows$reporter == "N/A",]

b <- Comtradrclean(flows, 2016, TRUE, .01)
