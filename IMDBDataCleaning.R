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
# imdb[is.na(imdb$duration),]$duration <- 116 #or
imdb <- imdb %>%
  mutate(duration=replace(duration, is.na(duration), 116))

## Color - mode imputation and convert to 0/1
#imdb$color[is.na(imdb$color)] <- "Color"
imdb <- imdb %>% 
  mutate(color=replace(color, is.na(color), "Color"))
imdb <- imdb %>%
  mutate(color = ifelse(color == "Color", 1, 0))

## num_user_for_reviews = mean imputation
imdb <- imdb %>%
  mutate(num_user_for_reviews=replace(num_user_for_reviews, is.na(num_user_for_reviews), 
                                      mean(num_user_for_reviews, na.rm=TRUE)))

## Director - convert to number of movies made by director
director_movie_count <- imdb %>%
  group_by(director_name) %>%
  summarise(movies_made = n())

imdb <- imdb %>%
  left_join(director_movie_count) %>%
  select(-director_name)

## Median imputation for num_critic_for_reviews
# imdb[is.na(imdb[["num_critic_for_reviews"]]), "num_critic_for_reviews"] <-
#   median(x = imdb[["num_critic_for_reviews"]], na.rm = TRUE)
imdb <- imdb %>% 
  mutate(num_critic_for_reviews=replace(num_critic_for_reviews,
                                        is.na(num_critic_for_reviews),
                                        median(num_critic_for_reviews, na.rm=TRUE)))

## Language - only five missing values so we replace them
missing_languages <- c("English", "None", "None", "None", "None")
#imdb$language[is.na(imdb$language)] <- missing_languages  ## or
imdb <- imdb %>% 
  mutate(language=replace(language, is.na(language), missing_languages))

## Lots of very small categories for language so combine into English vs. Non-English
imdb <- imdb %>%
  mutate(language=fct_collapse(language, Other=unique(language[language!="English"])))
table(imdb$language) 

## Content-rating - collapse GP --> PG and create "other"
## X --> NC-17, TV-?? --> TV, M-->PG13
imdb <- imdb %>%
  mutate(content_rating=fct_explicit_na(content_rating, na_level = "Unknown")) %>%
  mutate(content_rating=fct_collapse(content_rating, PG=c("GP", "PG"),
                                     NC17=c("X", "NC-17"),
                                     TV=c("TV-14", "TV-G", "TV-PG"),
                                     PG13=c("PG-13","M")))
table(imdb$content_rating)

## Genres - get the main genre and number of genres assigned
imdb <- imdb %>% mutate(main_genre=(str_split(genres, "\\|") %>%
                                      sapply(., FUN=function(x){x[1]})),
                        num_genre=(str_split(genres, "\\|") %>%
                                     sapply(., FUN=length)))
table(imdb$main_genre) 

#Some genres only have 1 movie so create "other" category
#that contains all categories with less than 10 movies
other.cat <- imdb %>% group_by(main_genre) %>% 
  summarize(n=n()) %>% filter(n<10) %>% pull(main_genre)
imdb <- imdb %>%
  mutate(main_genre=fct_collapse(main_genre, Other=other.cat))

# There is a somewhat strong correlation between budget and gross, so we will
# impute the budget first since budget has fewer missing values and then use the
# imputed the budget to predict the gross

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

## actor_name columns - we created num_top_actors which tell us 
## how many “top” actors were in a movie. “Top” actors were actors who 
## were in multiple movies. All 3 actor column were used to decide who was a top actor.
all.actors <- imdb %>% select(actor_1_name, actor_2_name, actor_3_name) %>% do.call(c, args=.)
actors.freq <- data.frame(actor=all.actors) %>% filter(!is.na(actor)) %>%
  group_by(actor) %>% summarize(n=n()) %>%
  arrange(desc(n))
top.actors <- actors.freq %>% filter(n>10) %>% pull(actor)
imdb <- imdb %>%
  mutate(num_top_actors=(ifelse(actor_1_name%in%top.actors, 1, 0) +
                           ifelse(actor_2_name%in%top.actors, 1, 0) +
                           ifelse(actor_3_name%in%top.actors, 1, 0)))

## facebook_like columns -  we made a column num_pop_actors for the total number of popular actors 
## in a movie based off of their Facebook likes. With this, we decided to throw 
## out cast_facebook likes as it seemed repetitive. It’s pretty mute since there’s 
## also actor Facebook likes
actor.likes <- imdb %>% select(actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes) %>%
  do.call(c, args=.)
actors.likes <- data.frame(actor=all.actors, likes=actor.likes) %>%
  filter(!is.na(actor)) %>% group_by(actor) %>% summarize(likes=max(likes)) %>%
  arrange(desc(likes))
pop.actors <- actors.likes %>% filter(likes>quantile(likes, probs=0.99)) %>%
  pull(actor)
imdb <- imdb %>%
  mutate(num_pop_actors=(ifelse(actor_1_name%in%pop.actors, 1, 0) +
                           ifelse(actor_2_name%in%pop.actors, 1, 0) +
                           ifelse(actor_3_name%in%pop.actors, 1, 0)))

## movie_facebook_likes - we believe it to be unreliable. 
## It seems that some of the movies with 0s in the data have more 
## likes than top movies in real life. Although, there are no NAs 
## so not much cleaning to do there. We were considering throwing it out

## I am going to throw out any variable we didn't use/clean
imdb <- imdb %>% select(-cast_total_facebook_likes, -movie_imdb_link, -facenumber_in_poster,
                        -plot_keywords, -country, -movie_facebook_likes, -director_facebook_likes,
                        -actor_1_name, -actor_2_name, -actor_3_name, -actor_1_facebook_likes,
                        -actor_2_facebook_likes, -actor_3_facebook_likes, -genres, 
                        -cast_total_facebook_likes)
plot_missing(imdb)
                        
####################################
## Write out cleaned up IMDB Data ##
####################################

write_csv(x=imdb, path="./CleanedIMDBData.csv")








