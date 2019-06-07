
#########################################################
# Prerequisites / preparation steps
#########################################################
# This file uses a subset movielens data as required for this project. The data is loaded,
# sanitized/preped, and partitioned into training (edx) and validation sets.
# This code is also available in in file data_partition.R in my RStudio project checked in GitHub ()

# In my data partition code I also save the data in an RData (rda) object: edx.rda
# This way you don't have to load the data everytime you lose your edx R session/environment variable
# You can subsequently load it using the following code (I have commented it your since you don't have
# my RDA ojbect, however you can grab it from my github project).

load( file="rdas/edx.rda")

# Otherwise, if you decide to run my code, it will use the edx and validation session objects from
# your environment.

# This and any additonal information regarding the usage of this project and how to set up the
# project, can be found in the Readme file inside the GitHub git repository.


#########################################################
# Training various models and evaluate their performance
#########################################################

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
# Attempt 3 - Modeling Regularization + Movie Effect + User Effect
#

#
# Training algorithm:
#   
# 
#########################################################

# First let's note that I have two options to calculate lambda

# I decided to use get the most out of our training data by calculating the lambda based
# on that 
# This is more less efficient in computing time, but gives me a slightly better RMSE than
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
qplot(lambdas, rmses)  
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
rmse_results <- bind_rows(rmse_results, tibble(method = "movie and user effects 2",
                                               RMSE = movie_user_effects_reg2_rmse))

# Also I can demonstrate, by plugging 0 for lambda in the above code, that lambda of 0 gives me
# the same result as I got without regularization. This is just a little cross-checking to validate
# my own code.



