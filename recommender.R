###edX Capstone Project - Movielens ratings predictor###


#Assume that user begins with edx set and validation set (final hold-out test set). Not including edX provided script to create data sets here.

#Require and install tidyverse and caret packages
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(lubridate))
  install.packages("lubridate")

#load required packages to library
library(tidyverse)
library(lubridate)

##partition the data set created in the provided code into a test set and training set for model training and testing
#set seed for repeatable randomness
set.seed(1, sample.kind = "Rounding")

#create data partition index to split 20% of the edx set into a test set and 80% into a training set
train_index <- createDataPartition(y = edx$rating,
                                   p = 0.2,
                                   list = FALSE)

#create partitioned data sets using index
train_set <- edx[train_index,]
test_set <- edx[-train_index,]

#FOR TESTING ONLY, cut dataset down
# train_set <- train_set %>% slice_sample(n = 100000)
# test_set <- test_set %>% slice_sample(n = 10000)

#remove movies or users which only appear in one set or the other
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#write function to calculate the RMSE between predicted and actual movie ratings
RMSE <- function(actual_ratings, predicted_ratings) {
  sqrt(mean((actual_ratings - predicted_ratings) ^ 2))
}

##Train matrix factorization model
#calculate the average rating across all movies and users
mean <- mean(train_set$rating)

#calculate and average user rating above or below the mean, per movie (the 'movie effect')
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(user_eff = mean(rating - mean))

#calculate average movie rating above or below the mean for each user, per movie (the 'movie effect')
user_avgs <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(movie_eff = mean(rating - mean - user_eff))

#predict ratings using user and movie effects model
prediction <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mean + user_eff + movie_eff) %>%
  .$pred

#calculate the RMSE of the predictions
RMSE(test_set$rating, prediction)

##add date effect to model
#plot average weekly rating vs. date
train_set %>% 
  mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(week,mean)) +
  geom_point() +
  geom_smooth()

#approximate linear model to calculate the effect of date on user, movie rating
date_avgs <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  
  #add column with week of rating
  mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>%
  
  #calculate average movie rating above or below the mean for each user and movie, by week
  group_by(week) %>%
  summarize(date_eff = mean(rating-mean-user_eff-movie_eff))

##predict ratings using user, movie, and date effects model
#calculate week from test set timestamps
test_set_date <- test_set %>%
  mutate(week = round_date(as_datetime(timestamp), unit = "week"))

#calculate predicted rating
prediction <- test_set_date %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(date_avgs, by = 'week') %>%
  mutate(pred = mean + user_eff + movie_eff + date_eff) %>%
  .$pred

#calculate the RMSE of the predictions
RMSE(test_set_date$rating, prediction)

##add genre effect to model
#plot distribution of ratings by genre
train_set %>%
  group_by(genres) %>%
  filter(n() > 10000) %>%
  summarise(mean = mean(rating), sd = sd(rating), upper = mean + sd, lower = mean - sd) %>%
  ggplot(aes(genres, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#calculate average impact of genre on user rating of movie
genre_avgs <- train_set %>%
  
  #add column with week of rating
  mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
  
  #add user, movie, and date effects
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(date_avgs,by = 'week') %>%
  
  #calculate average movie rating above or below the mean for each user and movie, by genre
  group_by(genres) %>%
  summarize(genre_eff = mean(rating-mean-user_eff-movie_eff-date_eff))

#calculate predicted rating
prediction <- test_set_date %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(date_avgs, by = 'week') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mean + user_eff + movie_eff + date_eff + genre_eff) %>%
  .$pred

#calculate the RMSE of the predictions
RMSE(test_set_date$rating, prediction)

##regularize effects to be conservative when estimating based on small sample sizes
#create an array of lambda values for tuning algorithm
lambdas <- seq(0, 10, 0.25)

#perform cross validation of the regularized user + movie effects model, different values of lambda
rmses <- sapply(lambdas, function(l){ #supply array of lambda values and run cross validation
  
  #calculate user effect with each lambda
  user_avgs <- train_set %>%
    group_by(movieId) %>%
    summarize(user_eff = sum(rating - mean)/(n()+l))
  
  #calculate movie effect with each lambda
  movie_avgs <- train_set %>% 
    left_join(user_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(movie_eff = sum(rating - user_eff - mean)/(n()+l))
  
  #calculate predictions
  prediction <- test_set %>% 
    left_join(user_avgs, by = "movieId") %>%
    left_join(movie_avgs, by = "userId") %>%
    mutate(pred = mean + user_eff + movie_eff) %>%
    .$pred
  
  #return RMSE for each lambda
  return(RMSE(prediction, test_set$rating))
})

#select lambda from cross which minimizes RMSE
l <- lambdas[which.min(rmses)]

##calculate RMSE based on validation set
#remove movies or users which only appear in the validation or edx training set
validation_test <- validation %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#predict ratings for validation set using final model
validation_predicted_ratings <- validation_test %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(prediction = mean + user_eff + movie_eff) %>%
  .$prediction

#calculate and print RMSE of final model
RMSE(validation_predicted_ratings, validation_test$rating)
