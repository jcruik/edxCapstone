###edX Capstone Project - Movielens ratings predictor###


#Assume that user begins with edx set and validation set (final hold-out test set). Not including edX provided script to create data sets here.

#Require and install tidyverse and caret packages
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(caret))
  install.packages("caret")

#load required packages to library
library(tidyverse)
library(caret)

##partition the data set created in the provided code into a test set and training set for model training and testing
#set seed for repeatable randomness
set.seed(1)

#create data partition index to split 20% of the edx set into a test set and 80% into a training set
train_index <- createDataPartition(y = edx$rating,
                                   p = 0.2,
                                   list = FALSE)

#create partitioned data sets using index
train_set <- edx[-train_index]
test_set <- edx[train_index]

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
  # filter(n() >= 100) %>%
  summarize(movie_eff = mean(rating - mean - user_eff))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(prediction = mean + user_eff + movie_eff) %>%
  .$prediction

RMSE(test_set$rating, predicted_ratings)

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
