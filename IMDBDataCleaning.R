##
## This is all my code to analyze the IMDB database
##

## Libraries I Need
library(tidyverse)

## Read in the data
imdb <- read_csv("./IMDBTrain.csv")
imdb.test <- read_csv("./IMDBTest.csv")

##
## Exploratory Data Analysis
##

## Overall summary of the data
summary(imdb)

## Scatterplot of Budget vs. Score
## Budget is in local currency, need to convert to common currency
ggplot(data=imdb, mapping=aes(x=budget, y=imdb_score)) +
  geom_point()
imdb %>% filter(budget>100000000, country=="USA") %>% 
  select(movie_title)

## Scatterplot of gross vs imdb
ggplot(data=imdb, mapping=aes(x=gross, y=imdb_score)) +
  geom_point()
with(imdb, cor(gross, imdb_score, use="complete.obs"))
