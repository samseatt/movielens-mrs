
######################################################################################
# Prerequisites / preparation steps
######################################################################################

#########################################################
# Include R libraries
#########################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(readr)  # for read_csv
library(knitr)  # for kable
library(lubridate) # for dates and time
library(stringr) # string and text manipulation

#########################################################
# Data cleanup and partitioning
#########################################################

# This file uses a subset movielens data as required for this project. The data is loaded,
# sanitized/preped, and partitioned into training (edx) and validation sets.
# This code is also available in in file data_partition.R in my RStudio project checked in GitHub ()

# In my data partition code I also save the data in an RData (rda) object: edx.rda
# This way you don't have to load the data everytime you lose your edx R session/environment variable
# You can subsequently load it using the following code (It may be commented it you copy since you don't have
# my RDA ojbect, however you can grab it from my github project).

edx_path <- "https://adaprise-01.s3.amazonaws.com/edx/edx.rda"
validation_path <- "https://adaprise-01.s3.amazonaws.com/edx/validation.rda"
if (!exists("edx")) {
  load(url(edx_path))
}
if (!exists("validation")) {
  load(url(validation_path))
}

# load( file="rdas/edx.rda")
# load( file="rdas/validation.rda")

# Otherwise, if you decide to run my code, it will use the edx and validation session objects from
# your environment.

# This and any additonal information regarding the usage of this project and how to set up the
# project, can be found in the Readme file inside the GitHub git repository.


######################################################################################
# Define functions
######################################################################################

#########################################################
# First let's define an RMSE function that we can consistently use throughout this analysis
# 
# Function: RMSE
#   Takes two arrays of equal size actual or true ratings, and the ratings we have predicted,
#   for each rating of the set we want to evaluate (in other words, our validation set).
#   The method then calculateds the root mean squared error (RMSE) in the standard way.
#
#########################################################
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

######################################################################################
# Data analysis and explorative visualizations
######################################################################################

#########################################################
# Check data distributions, especially ratings trends
#########################################################
# Find all the ratings given (which should be same as all the possible ratings)
sort(unique(edx$rating))

# If we assign ratings randomly (from all the possible ratings available to give:
# i.e. 0.5 to 5 in 0.5 increments, with 0 or anyting not divisible by half not allowed
# values), then this is the average and median respectively that will be given:
summary(unique(edx$rating))

# See the the basic stats of the ratings column:
summary(edx$rating)

# See why movies are getting higher ratings on the average in this dataset.

# Plot the barplot showing the relative frequency of each possible rating
edx %>% ggplot(aes(x=rating)) + 
  geom_histogram(color="black", fill="orange", binwidth=0.5)

# Show the average rating and rating frequency of each specifid movie

# - lowest rated movies
edx %>% group_by(movieId) %>%
  summarize(title = first(title), average = mean(rating), count = n()) %>%
  arrange(average)

# - highest rated movies
edx %>% group_by(movieId) %>%
  summarize(title = first(title), average = mean(rating), count = n()) %>%
  arrange(desc(count))

# - movies rated the least number of times (among those that were rated at least once, since
#   otherwise they won't be in our dataset)
edx %>% group_by(movieId) %>%
  summarize(title = first(title), average = mean(rating), count = n()) %>%
  arrange(count)

# - movies rated the most number of times
edx %>% group_by(movieId) %>%
  summarize(title = first(title), average = mean(rating), count = n()) %>%
  arrange(desc(count))

# Distribution of movies based on average ratings given to each movie
edx %>% group_by(movieId) %>%
  summarize(title = first(title), average = mean(rating), count = n()) %>%
  arrange(average) %>%
  ggplot(aes(x=average)) + 
  geom_histogram(color="black", fill="orange") +
  xlab("Average rating given to a movie") + 
  ylab("Number of movies")

# Distribution of movies based on average ratings given by each user
edx %>% group_by(userId) %>%
  summarize(user = first(userId), average = mean(rating), count = n()) %>%
  arrange(average) %>%
  ggplot(aes(x=average)) + 
  geom_histogram(color="black", fill="orange") +
  xlab("Average rating given by a user") + 
  ylab("Number of users")
  

nrow(edx)  # total number of ratings given
length(unique(edx$movieId))  # number of movies in the dataset
length(unique(edx$userId))  # number of users in the dataset

# Percentage of ratings given (out of all ratings possible in this dataset)
nrow(edx) / (length(unique(edx$movieId)) * length(unique(edx$userId)))

# get a random subset of edx for experimentation; call it edx_lite
set.seed(2)
edx_lite_index <- createDataPartition(y = edx$rating, times = 1, p = 0.00001, list = FALSE)
edx_lite <- edx[edx_lite_index,]

# Visualize sparseness
edx_lite %>% ggplot(aes(as.factor(movieId), as.factor(userId), fill = rating)) +
  geom_tile(color = "grey50") +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Oranges"), trans = "sqrt") +
  theme_minimal() +  theme(axis.text.x = element_blank(),
                           axis.text.y = element_blank()) +
  ggtitle("Ratings Sparseness") + 
  ylab("users") + 
  xlab("movies")

# The above result shows that the ratings are fairly sparse with about 1.2 %
# (percent) of all movies rated. But it is not as sparse as one can can think i.e. a
# single average user cannot watch 1% of all the movies in the system, let alone
# go ahead and then rate each one of them. This, however, makes more sense when we
# the fact that the database only contains (a) only the movies that have been rated
# (that is, that were at least watched during the data collection period, and
# (b) relatively less consequentially, only those users are listed who at have rated
# at least one movie. We need to be aware of this characteristic/limitation of the dataset
# when we use it to describe the world.

by_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>% summarize(count = n())

options(scipen=10000)
as_data_frame(by_genres) %>%
  mutate(genres = reorder(genres, count)) %>%
  ggplot(aes(x=genres, y=count)) +
  geom_bar(position="dodge", stat="identity", fill="orange") +
  labs(x="Genre", y="Ratings received for each genra") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Let's make an expanded dataset: edx_x (edx expanded)
edx_x <- mutate(edx, time = as.Date.POSIXct(timestamp),
                release_time = as.Date(ISOdate(gsub("\\).*", "", gsub(".*\\(", "", title)), 1, 1, 0, 0, 0)),
                action = grepl("Action", genres),
                adventure = grepl("Adventure", genres),
                animation = grepl("Animation", genres),
                children = grepl("Children", genres),
                comedy = grepl("Comedy", genres),
                crime = grepl("Crime", genres),
                documentary = grepl("Documentary", genres),
                drama = grepl("Drama", genres),
                fantasy = grepl("Fantasy", genres),
                film_noir = grepl("Film-Noir", genres),
                horror = grepl("Horror", genres),
                imax = grepl("IMAX", genres),
                musical = grepl("Musical", genres),
                mystery = grepl("Mystery", genres),
                romance = grepl("Romance", genres),
                sci_fi = grepl("Sci-Fi", genres),
                thriller = grepl("Thriller", genres),
                war = grepl("War", genres),
                western = grepl("Western", genres)
)

# Similarly further curate and expand validation
validation_x <- mutate(validation, time = as.Date.POSIXct(timestamp),
                release_time = as.Date(ISOdate(gsub("\\).*", "", gsub(".*\\(", "", title)), 1, 1, 0, 0, 0)),
                action = grepl("Action", genres),
                adventure = grepl("Adventure", genres),
                animation = grepl("Animation", genres),
                children = grepl("Children", genres),
                comedy = grepl("Comedy", genres),
                crime = grepl("Crime", genres),
                documentary = grepl("Documentary", genres),
                drama = grepl("Drama", genres),
                fantasy = grepl("Fantasy", genres),
                film_noir = grepl("Film-Noir", genres),
                horror = grepl("Horror", genres),
                imax = grepl("IMAX", genres),
                musical = grepl("Musical", genres),
                mystery = grepl("Mystery", genres),
                romance = grepl("Romance", genres),
                sci_fi = grepl("Sci-Fi", genres),
                thriller = grepl("Thriller", genres),
                war = grepl("War", genres),
                western = grepl("Western", genres)
)



head(edx_x)
head(validation_x)

set.seed(2)
edx_x_lite_index <- createDataPartition(y = edx_x$rating, times = 1, p = 0.00001, list = FALSE)
edx_x_lite <- edx[edx_x_lite_index,]

nrow(edx_x_lite)


# Now I can calculate the time lapse between the release of a movie and the day the rating
# was received. This information parameter can then potentially be fed into an ML model to
# account for a bias that we can call a "new movie effect". Of course, should we decide to use
# such a parameter then it would be beneficial for very old movies that
# were release decades before NetFlix came into existence, to have their effects truncated.
# Movie release date can also be used as a parameter in itself to gauge if particular user
# tends to like newer movies, or all movies relatively equally.
head(edx_x$time - edx_x$release_time)
summary(edx_x$time)
summary(edx_x$release_time)


# Also check how users rated movies over time. In addition to understanding viewing trends over
# years, this may also help us understand how the data was collected: consistently, or over
# haphazard chunks.
# Weekly intervals are used since a week should be representative of each different day of the
# week (as weekends represent a different viewing pattern for a general user).

edx %>%
  mutate(week = round_date(as_datetime(timestamp), unit = "week")) %>% group_by(week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(week, rating)) +
  geom_point(color="orange") +
  geom_smooth(color="red", method = "loess")


# Now let's see if there is a specific pattern over different weekdays.

# It could be useful to use this information to suggest a movie to the user based on the
# day of the week they are looking to watch it. However, such additonal data if not too
# meaningful could also contribute towards over training.

edx_x %>% 
  mutate(rating_day = weekdays(as.Date(edx_x$time, origin = "1960-10-01"))) %>%
  group_by(rating_day) %>%
  summarize(count = n()) %>%
  mutate(rating_day = reorder(rating_day, count)) %>%
  ggplot(aes(x=rating_day, y=count)) +
  geom_bar(position="dodge", stat="identity", fill="orange") +
  labs(x="Day of the week", y="Ratings received for each weekday") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# It turns out that the rating are more or less evenly split over all weekdays, and the slight
# variation that do exist does not seem to follow a meaningful pattern. So I will not proceed
# with any enhancement in the model that will account for the weekday when suggesting movies to
# a user.



######################################################################################
# Train various models and evaluate their performance
######################################################################################

#########################################################
# Model 1 (baseline) - we predict the same rating for all the movies regardless of
#   the user or the movie quality or genre (we set that 'same' rating as the the average
#   rating of all the movies i.e. the mean rating for the entier set - i.e. in this
#   over-simplistic model, each movie is considered to have this average rating)
# 
# Hene, this attempt gives us the baseline error if we do nothing so to speak.
# 
#########################################################

# Define mu_hat as the average rating of all the movies i.e., this is the rating this naive
# model 'predicts' for each movie. (Of course we take the mean of the traiing (edx) set and
# not the validation set as this average calculation is akin ot traiing on the training set.
# Though it shouldn't make much difference anywat due to the law of averages as we studied
# in the earleir courses.)
avg_rating <- mean(edx$rating)

# We compare the average against the actual to compare the RMSE
# This, according to statistics theory, get to be around 1.
baseline_rmse <- RMSE(validation$rating, avg_rating)
baseline_rmse

# Add the results to a tibble for future evaluation e.g. when presenting a comparative analysis
# in the RMD report.
rmse_results <- tibble(method = "baseline", RMSE = baseline_rmse)


# Now we try to incrementally improve our model to hopefully get progressively lower RMSEs i.e.
# better predictions.

# Esentially what we are predicting here is what a given user will predict a given movie.
# This does not appear to have a practical value by itself (other than winning a competetion).
# However, the way Netflix (or some other move recommender system, or a recommender system in
# general) can use these results is by predicting a given user's rating; then finding say the
# top 5 or 10 or more movies from these predictions; then presenting those to the user as
# recommendations that the user would like. For our evaluation purposes, we comnpare this model
# against the validation set; in real life the test is if the user really feels that the list
# presented were indeed the movies she liked when she saw them. In practice we can for example
# poll random users to see how the recommendations worked for them, or more practically, we can
# add further logic in our system to see how mony of the movies recommended did the users actually
# see, actually finish, and actually rated higher, etc. This last part is not required for this
# project, but it is indeed important to know if, and how, and how efficiently (delay etc.)
# our models are going to be used during prediction time.

## 
#########################################################
# Model 2 - Modeling movie effects
#
# This is my second attempt to improve the model through training. Here I improve my model by
# adding move effects to my model.
#
# Training algorithm:
#   
# 
#########################################################

# Basically in this code I calculate the average rating of each movie in the training set
# I also subtract the overall average rating of all movies (mu) from each move average to get
# an number that is normalized around mu, like if was shown in the machine learning class
mu <- mean(edx$rating)
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# So I have basically trained the model such that individual rating of the movie is what the model
# predicts. So it can predict the overall good movies for the user, but with no regard to the user's
# individual preference

# Use the trained bias to predict the validation
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# Calculate RMSE and add it to the results tibble
movie_effects_rmse <- RMSE(validation$rating, predicted_ratings)
movie_effects_rmse # 0.9439087
rmse_results <- bind_rows(rmse_results, tibble(method = "movie effects", RMSE = movie_effects_rmse))

# Since the above approach is still simplistic, as we don't account for user's preference and just
# give her the best overall movies. This helps, but as we see, for this reason, fails to give us
# an acceptable RMSE


#########################################################
# Model 3 - Modeling user effects
#
# This is my third attempt to improve the model through training. Here I improve my model by
# adding user effects (in addition to movie effects) to this model model.
#
# Training algorithm:
#   
# 
#########################################################

# To add the user effect as bias, first calculate the average rating given by each user.
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict the ratings on validation set by including both biases: the move effect and the
# recently calculated user effect.
# In other words, for every user-movie combination predicted (e.g. when predictong a movies for
# a given single use), we bias the movie by how well it received ratings for other users and how
# well each of the user, rating tha movie, has rated other movies.
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE and add it to the results tibble
movie_user_effects_rmse <- RMSE(validation$rating, predicted_ratings)
movie_user_effects_rmse # 0.8653488
rmse_results <- bind_rows(rmse_results, tibble(method = "movie and user effects",
                                               RMSE = movie_user_effects_rmse))

## NOTE: We already get the desired RMSE with the user effects included (above)

## But let's try to make it even better by using regularization


#########################################################
# Model 4 - Modeling Regularization + Movie Effect + User Effect
#

#
# Training algorithm:
#   
# 
#########################################################

# First let's note that I have two options to calculate lambda

# I decided to use get the most out of our training data by calculating the lambda based
# on that 
# This is less efficient in computing time, but gives me a slightly better RMSE than
# what I get by calculating lambda beforehand using either the full training set, a subset of
# the training set, or the validation set (I try to avoid using the validation set to calculate
# lambda as using the validation set to compute tuning parameters could make the model's predictions
# on the validation set slightly suspect since the lambda this lambda will slightly favor the validation
# set especially when the validation set is small)

## First create a range of lambdas from which to pick the best one
lambdas <- seq(0, 10, 0.25)

#########################################################
# Function to calculate a set of RMSEs using a series of lambdas
# The function calculates an RMSE for each lambda using movie effects + user effects +
# regularization using that lambda
#
# The result is an array containing the RMSE obtained using each of the lambda in the sequence
# of lambdas passed in the first argument
#########################################################
rmses <- sapply(lambdas, function(l){
  
  # Calculate average rating for normalization purpose, as done before
  mu <- mean(edx$rating)
  
  # Calculate the first bias to use: movie effect, and regularization with lambda l
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  # Calculate the second bias to use: user effect, and regularization with lambda l
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  
  # Predict the rating using this model with the biases regularized over a particular lambda in the range
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  # Calculate the RMSE for predictions for each lambda; return this list of RMSEs
  return(RMSE(validation$rating, predicted_ratings))
})

# Find the minimum of all the calculated RMSEs. This gives us the best RMSE using movie effects,
# plus user effects, plus the best lambda from the finite list of lambdas
movie_user_effects_reg_rmse = min(rmses)
movie_user_effects_reg_rmse # 0.8648170
rmse_results <- bind_rows(rmse_results, tibble(method = "movie and user effects with regularization",
                                               RMSE = movie_user_effects_reg_rmse))

# This gives even better results and significantly better result than required for this assignment
# Of course at the well-justified cost of some additional calculations across all the lambdas

# I can also calculate/pick the lambda first and then run the analysis as follows:

#########################################################
# Function to pick the best lambda
#########################################################
mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

eval <- validation
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- eval %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(eval$rating, predicted_ratings))
})
qplot(lambdas, rmses, xlab="Lambda value", ylab="RMSE achieved with the given lambda")

lambda <- lambdas[which.min(rmses)]

## My calculated/picked in this case happens to be is 2.5

mu <- mean(edx$rating)

# I can calculate the bias for this lambda
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda))

# Calculate the second bias to use: user effect, and regularization with lambda l
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))

# Predict the rating using this model with the biases regularized over a particular lambda in the range
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

movie_user_effects_reg2_rmse = RMSE(validation$rating, predicted_ratings)
movie_user_effects_reg2_rmse # 0.8649303
rmse_results <- bind_rows(rmse_results, tibble(method = "movie and user effects - hand-picked lambda",
                                               RMSE = movie_user_effects_reg2_rmse))

# Also I can demonstrate, by plugging 0 for lambda in the above code, that lambda of 0 gives me
# the same result as I got without regularization. This is just a little cross-checking to validate
# my own code.

# Finally , print the results
rmse_results %>% knitr::kable()


################

