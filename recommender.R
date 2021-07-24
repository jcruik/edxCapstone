###edX Capstone Project - Movielens ratings predictor###


###########################################################
# Create edx set, validation set (final hold-out test set)#
###########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Require and install tidyverse and caret packages
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(lubridate))
  install.packages("lubridate")
if (!require(caret))
  install.packages("caret")

#load required packages to library
library(tidyverse)
library(lubridate)
library(caret)

##partition the data set created in the provided code into a test set and training set for model training and testing
#set seed for repeatable randomness
set.seed(1, sample.kind = "Rounding")

#create data partition index to split 20% of the edx set into a test set and 80% into a training set
train_index <- createDataPartition(y = edx$rating,
                                   p = 0.8,
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
RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings) ^ 2))
}

##Train matrix factorization model
#calculate the average rating across all movies and users
mean <- mean(train_set$rating)

#calculate and average user rating above or below the mean, per movie (the 'movie effect')
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(movie_eff = mean(rating - mean))

#calculate average movie rating above or below the mean, per user (the 'user effect')
user_avgs <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(user_eff = mean(rating - mean - movie_eff))

#predict ratings using user and movie effects model
prediction <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mean + user_eff + movie_eff) %>%
  .$pred

#calculate the RMSE of the predictions
user_movie_RMSE <- RMSE(test_set$rating, prediction)

##add date effect to model
#plot average weekly rating vs. date
date_plot <- train_set %>% 
  mutate(date = as_datetime(timestamp), week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(week,mean)) +
  geom_point() +
  geom_smooth() +
  ylab("Mean Rating") +
  xlab("Week")

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
user_movie_date_RMSE <- RMSE(test_set_date$rating, prediction)

##add genre effect to model
#plot distribution of ratings by genre
genre_plot <- train_set %>%
  group_by(genres) %>%
  filter(n() > 50000) %>%
  summarise(mean = mean(rating), sd = sd(rating), upper = mean + sd, lower = mean - sd) %>%
  ggplot(aes(genres, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Mean Rating") +
  xlab("Genre")

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
user_movie_date_genre_RMSE <- RMSE(test_set_date$rating, prediction)

##plot sample size of each movie, user
#count ratings by movie
movie_count <- train_set %>%
  count(movieId)

#plot the frequency of ratings, by movie
movie_count_plot <- movie_count %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 20) +
  ggtitle("Movies") +
  xlab("Number of Ratings") +
  xlim(c(0,1000))

#count ratings by user
user_count <- train_set %>%
  count(userId)

#plot the frequency of ratings, by user
user_count_plot <- user_count %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 20) +
  xlab("Number of Ratings") +
  ggtitle("Users") +
  xlim(c(0,1000))

##regularize effects to be conservative when estimating based on small sample sizes
#create an array of lambda values for tuning algorithm
lambdas <- seq(0, 10, 1)

#perform cross validation of the regularized user + movie effects model, different values of lambda
rmses <- sapply(lambdas, function(l){ #supply array of lambda values and run cross validation
  
  #calculate movie effect with each lambda
  movie_avgs <- train_set %>%
    group_by(movieId) %>%
    summarize(user_eff = sum(rating - mean)/(n()+l))
  
  #calculate user effect with each lambda
  user_avgs <- train_set %>%
    left_join(movie_avgs, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(movie_eff = sum(rating - mean - user_eff)/(n()+l))
  
  #calculate date effect with each lambda
  date_avgs <- train_set %>%
    left_join(user_avgs, by = 'userId') %>%
    left_join(movie_avgs, by = 'movieId') %>%
    mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
    group_by(week) %>%
    summarize(date_eff = sum(rating-mean-user_eff-movie_eff)/(n()+l))
  
  #calculate genre effect with each lambda
  genre_avgs <- train_set %>% 
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
    left_join(date_avgs, by = 'week') %>%
    group_by(genres) %>%
    summarize(genre_eff = sum(rating-mean-user_eff-movie_eff-date_eff)/(n()+l))
  
  #calculate predictions
  prediction <- test_set %>%
    mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    left_join(date_avgs, by = 'week') %>%
    left_join(genre_avgs, by = 'genres') %>%
    mutate(pred = mean + user_eff + movie_eff + date_eff + genre_eff) %>%
    .$pred
  
  #return RMSE for each lambda
  return(RMSE(prediction, test_set$rating))
})
#plot RMSE against lambda
lambda_plot <- qplot(lambdas,rmses)

#select lambda from cross validation which minimizes RMSE
l <- lambdas[which.min(rmses)]

#record minimum RMSE using regularized model
regularized_RMSE <- min(rmses)

##calculate final model using entire edx data set
#calculate the average rating across all movies and users
mean <- mean(edx$rating)

#calculate movie effect
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(user_eff = sum(rating - mean)/(n()+l))

#calculate user effect
user_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(movie_eff = sum(rating - mean - user_eff)/(n()+l))

#calculate date effect
date_avgs <- edx %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(week) %>%
  summarize(date_eff = sum(rating-mean-user_eff-movie_eff)/(n()+l))

#calculate genre effect
genre_avgs <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(date_avgs, by = 'week') %>%
  group_by(genres) %>%
  summarize(genre_eff = sum(rating-mean-user_eff-movie_eff-date_eff)/(n()+l))

#predict ratings for validation set using final model
prediction <- validation %>%
  mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(date_avgs, by = 'week') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mean + user_eff + movie_eff + date_eff + genre_eff) %>%
  .$pred

#round predictions which are outside the range of the 0-5 scale
prediction <- pmax(0,prediction)
prediction <- pmin(5,prediction)

#calculate and print RMSE of final model
final_model_RMSE <- RMSE(prediction, validation$rating)
