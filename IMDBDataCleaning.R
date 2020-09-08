##
## This is all my code to analyze the IMDB database
##

## Libraries I Need
library(tidyverse)
library(DataExplorer)

## Read in the data
imdb.train <- read_csv("./IMDBTrain.csv")
imdb.test <- read_csv("./IMDBTest.csv")

## Merge the two datasets together so when I clean the
## training dataset I also treat the test set the same way
names(imdb.test)[names(imdb.test)=="Id"] <- "movie_title"
imdb <- bind_rows(train=imdb.train, test=imdb.test, .id="Set")

####################################
## Some Exploratory Data Analysis ##
####################################

## Overall summary of the data
summary(imdb)

## Which variables have missing values?
plot_missing(imdb)

## Which variables are related to each other?
plot_correlation(imdb, type="continuous", 
                 cor_args=list(use="pairwise.complete.obs"))

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

###########################################
## Go through and clean up the variables ##
###########################################

## Duration - only one missing so just look it up
## and fill it in
imdb[is.na(imdb$duration),]$duration <- 116 #or
imdb <- imdb %>%
  mutate(duration=replace(duration, is.na(duration), 116))

## Color - mode imputation and convert to 0/1
imdb$color[is.na(imdb$color)] <- "Color"
imdb <- imdb %>%
  mutate(color = ifelse(color == "Color", 1, 0))

## Median imputation for num_critic_for_reviews
imdb[is.na(imdb[["num_critic_for_reviews"]]), "num_critic_for_reviews"] <-
  median(x = imdb[["num_critic_for_reviews"]], na.rm = TRUE)
imdb <- imdb %>% 
  mutate(num_critic_for_reviews=replace(num_critic_for_reviews,
                                        is.na(num_critic_for_reviews),
                                        median(num_critic_for_reviews, na.rm=TRUE)))

## Language - only five missing values so we replace them and then 
# we will have a indicator variable for each language
# Replace values for missing languages 
missing_languages <- c("English", "None", "None", "None", "None")
imdb$language[is.na(imdb$language)] <- missing_languages  ## or
imdb <- imdb %>% 
  mutate(language=replace(language, is.na(language), missing_languages))

## Genres - get the main genre
imdb <- imdb %>% mutate(main_genre=(str_split(genres, "\\|") %>%
                                      sapply(., FUN=function(x){x[1]})))
table(imdb$main_genre) 

#Some genres only have 1 movie so create "other" category
#that contains all categories with less than 10 movies
other.cat <- names(table(imdb$main_genre))[table(imdb$main_genre)<10]
imdb <- imdb %>%
  mutate(main_genre=fct_collapse(main_genre, Other=other.cat))

# There is a somewhat strong correlation between budget and gross, so we will
# impute the budget first since budget has fewer missing values and then
# imput the budget to predict the gross

## LM for Budget using only full data
budget.lm <- lm(sqrt(budget)~num_critic_for_reviews+duration+num_voted_users+
                  cast_total_facebook_likes+title_year+
                  movie_facebook_likes+main_genre, data=imdb)
budget.preds <- (predict(budget.lm, newdata=(imdb %>% filter(is.na(budget)))))^2
imdb <- imdb %>%
  mutate(budget=replace(budget, is.na(budget), budget.preds))

## Stochastic reg imputation for gross
gross.lm <- lm(sqrt(gross)~num_critic_for_reviews+duration+num_voted_users+
                  cast_total_facebook_likes+title_year+
                  movie_facebook_likes+main_genre+budget, data=imdb)
gross.preds <- (predict(gross.lm, newdata=(imdb %>% filter(is.na(gross))))+
                  rnorm(sum(is.na(imdb$gross)), 0, sigma(gross.lm)))^2
imdb <- imdb %>%
  mutate(gross=replace(gross, is.na(gross), gross.preds))
rm(list=c("gross.lm", "budget.lm"))

####################################
## Write out cleaned up IMDB Data ##
####################################

write_csv(x=imdb, path="./CleanedIMDBData.csv")








