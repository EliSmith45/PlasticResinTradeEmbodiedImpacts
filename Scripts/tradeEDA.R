library(tidyverse)
library(ggplot2)
library(here)

flows <- read_csv(here("Data","cleanRequestNew.csv"))
flows <- flows %>% filter(!(is.na(reporter)), partner != "World")
flows$commodity_code <- as.character(flows$commodity_code)

#investigating the NA values
naDataFrame <- flows %>% filter(is.na(qty))

#total throughput of trade network by year, in kg.
byYear <- flows %>%
  group_by(year, commodity_code) %>%
  summarize(trade = sum(qty, na.rm = TRUE))

#Plot of the above
pTotalByYear <- ggplot(byYear) + 
  geom_line(aes(x = year, y = trade, color = as.character(commodity_code)), size = 1.5) + 
  labs(title = "Total Trade by Year") + 
  theme_light()

print(pTotalByYear)





#Investigating the high PP flows in 2004
summary(filter(flows, commodity_code == "390210"))

ppSum <- filter(flows, commodity_code == "390210") %>%
  group_by(reporter, year, trade_flow) %>%
  summarize(trade = sum(qty, na.rm = TRUE)) %>%
  arrange(-trade)

ppMax <- max(ppSum$trade)



mexicoFlows <- flows %>% filter(reporter == "Mexico") %>% arrange(-qty)
mexicoFlowsByYear <- mexicoFlows %>%
  group_by(year, trade_flow, commodity_code) %>%
  summarize(qty = sum(qty, na.rm = TRUE))

pMexico <- ggplot(mexicoFlowsByYear) + 
  geom_line(aes(x = year, y = qty, color = as.character(commodity_code))) + 
  facet_grid(.~trade_flow)
print(pMexico)
#seems like an error in the database

#Mexico reported much higher exports to the US than the US reported imports from Mexico
mexicoUSA <- flows %>%
  filter(reporter %in% c("Mexico", "USA"), partner %in% c("Mexico", "USA"), year == "2004", commodity_code == "390210") %>%
  select(reporter, partner, year, trade_flow, qty) 

#fixing this presumed error by using the value that USA reported for the trade flow
flows[flows$reporter == "Mexico" & flows$partner == "USA" & flows$trade_flow == "Export" & flows$year == "2004" & flows$commodity_code == "390210","qty"] = 
  flows[flows$reporter == "USA" & flows$partner == "Mexico" & flows$trade_flow == "Import" & flows$year == "2004" & flows$commodity_code == "390210","qty"] 

flows[flows$reporter == "Mexico" & flows$partner == "USA" & flows$trade_flow == "Export" & flows$year == "2004" & flows$commodity_code == "390210","trade_value_us"] = 
  flows[flows$reporter == "USA" & flows$partner == "Mexico" & flows$trade_flow == "Import" & flows$year == "2004" & flows$commodity_code == "390210","trade_value_us"]


flows[flows$reporter == "Mexico" & flows$partner == "USA" & flows$trade_flow == "Import" & flows$year == "2004" & flows$commodity_code == "390210","trade_value_us"] = 
  flows[flows$reporter == "USA" & flows$partner == "Mexico" & flows$trade_flow == "Export" & flows$year == "2004" & flows$commodity_code == "390210","trade_value_us"]

flows[flows$reporter == "Mexico" & flows$partner == "USA" & flows$trade_flow == "Import" & flows$year == "2004" & flows$commodity_code == "390210","qty"] = 
  flows[flows$reporter == "USA" & flows$partner == "Mexico" & flows$trade_flow == "Export" & flows$year == "2004" & flows$commodity_code == "390210","qty"] 

summary(mexicoFlowsByYear)


#Now looking at Mexico's plastic waste outlier

mexicoWaste <- flows %>% filter(commodity_code == "3915",
                                reporter %in% c("USA", "Mexico"), 
                                partner %in% c("USA", "Mexico")) %>%
  group_by(reporter, partner, trade_flow, year) %>%
  summarize(trade = sum(qty, na.rm = TRUE))

pMexicoWaste <- ggplot(mexicoWaste) + 
  geom_line(aes(x = year, y = trade, color = reporter)) + 
  facet_grid(.~trade_flow)
print(pMexicoWaste)



#plastic waste also appears to be an error

flows[flows$reporter == "Mexico" & flows$partner == "USA" & flows$trade_flow == "Export" & flows$year == "2004" & flows$commodity_code == "3915","qty"] = 
  flows[flows$reporter == "USA" & flows$partner == "Mexico" & flows$trade_flow == "Import" & flows$year == "2004" & flows$commodity_code == "3915","qty"] 

flows[flows$reporter == "Mexico" & flows$partner == "USA" & flows$trade_flow == "Export" & flows$year == "2004" & flows$commodity_code == "3915","trade_value_us"] = 
  flows[flows$reporter == "USA" & flows$partner == "Mexico" & flows$trade_flow == "Import" & flows$year == "2004" & flows$commodity_code == "3915","trade_value_us"]





byCountry <- flows %>%
  group_by(reporter, trade_flow, year, commodity_code) %>%
  summarize(trade = sum(qty, na.rm = TRUE)) %>% 
  filter(trade > 1000000000)


pTotalByCountry <- ggplot(filter(byCountry, year == "2016")) + 
  geom_bar(aes(x = reorder(reporter, -trade), y = trade, fill = as.character(commodity_code)), stat = "identity", position = "dodge") + 
  facet_grid(. ~ trade_flow) + 
  labs(title = "Trade by Country") + 
  theme(axis.text.x = element_text(angle = 90, hjust = T, margin = margin(l = 20)))

print(pTotalByCountry)  



numPartners <- flows %>%
  filter(year == "2017", commodity_code =="390110") %>%
  group_by(reporter, trade_flow) %>%
  summarize(num_partners = n())

pNumPartners <- ggplot(numPartners) +
  geom_bar(aes(x = reporter, y = num_partners, fill = trade_flow), stat = "identity", position = "dodge") +
  labs(title = "Trade Partners by Country") +
  theme(axis.text.x = element_text(angle = 70, hjust = TRUE))
  

print(pNumPartners)

######################################################################################################################

