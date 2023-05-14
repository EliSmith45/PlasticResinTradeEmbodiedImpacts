library(tidyverse)
library(stringr)
library(corpustools)
library(igraph)
library(ggplot2)
library(GGally)
library(tidygraph)
library(WDI)
library(janitor)
library(RColorBrewer)

#read node data
nodeData <- read_csv("./Data/clustering.csv") 
nodeData <- nodeData[,-1]

income <- data.frame(income = c("Low income", "Lower middle income", "Upper middle income", "High income"),
                     income_code = c(1,2,3,4))
nodeData <- left_join(nodeData, income)

one <- pairs(cbind(nodeData[,6:9], nodeData[,14]))
a <- pairs(standardized)

instrength <- ggplot(nodeData) + 
  geom_histogram(aes(x = in_strength, y = (..count..)/sum(..count..))) + 
  labs(title = "Network in strength, all plastic in primary form, 2016", x = "In strength", y = "Frequency") + 
  theme_classic() + 
  theme(text = element_text(size = 20))

outstrength <- ggplot(nodeData) + 
  geom_histogram(aes(x = out_strength, y = (..count..)/sum(..count..))) + 
  labs(title = "Network out strength, all plastic in primary form, 2016", x = "Out strength", y = "Frequency") + 
  theme_classic()+ 
  theme(text = element_text(size = 20))

indegree <- ggplot(nodeData) + 
  geom_histogram(aes(x = in_degree, y = (..count..)/sum(..count..)), binwidth = 5) + 
  labs(title = "Network in degree, all plastic in primary form, 2016", x = "In degree", y = "Frequency") + 
  theme_classic()+ 
  theme(text = element_text(size = 20))

outdegree <- ggplot(nodeData) + 
  geom_histogram(aes(x = out_degree, y = (..count..)/sum(..count..))) + 
  labs(title = "Network out degree, all plastic in primary form, 2016", x = "Out degree", y = "Frequency") + 
  theme_classic()+ 
  theme(text = element_text(size = 20))

print(instrength)
print(outstrength)
print(indegree)
print(outdegree)

standardized <- cbind(nodeData[,6:9], nodeData[,14])
rownames(standardized) <- nodeData$country
standardized <- as.data.frame(scale(standardized))

clusters <- hclust(dist(standardized))
plot(clusters, labels = nodeData$country)
groups <- as.data.frame(cutree(clusters, k = 6))

groups <- cbind(rownames(groups), groups)
rownames(groups) <- NULL
groups <- rename(groups, country = "rownames(groups)")
groups <- rename(groups, cluster = "cutree(clusters, k = 6)")

standardized <- cbind(rownames(standardized), standardized)
rownames(standardized) <- NULL
standardized <- rename(standardized, country = "rownames(standardized)")

#create palette
pal <- brewer.pal(6, "Set1")
  
standardized <- left_join(standardized, groups)
pairs(standardized[,2:5], col = pal[standardized$cluster], pch = 19)

nodeData <- left_join(nodeData, select(groups, country, cluster))

summary <- nodeData %>% 
  group_by(cluster) %>%
  summarize(mean_in_degree = mean(in_degree, na.rm = TRUE),
            mean_out_degree = mean(out_degree, na.rm = TRUE),
            mean_in_strength = mean(in_strength, na.rm = TRUE),
            mean_out_strength = mean(out_strength, na.rm = TRUE),
            mean_income = mean(income_code, na.rm = TRUE))
