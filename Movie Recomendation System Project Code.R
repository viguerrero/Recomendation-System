if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)

#Create a function that calculates de RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Calculate the mean rating of all movies
mu <- mean(edx$rating)

#Calculate the Bias per Movie
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Confirming User Movie Bias
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#Calculate User Bias
user_avgs <- edx %>% 
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = mean(rating - mu - b_i))

#Confirming User Bias
user_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

#Creation of lambdas list
lambdas <- seq(0, 10, 0.25)

#Regularization of Bias and determining best Lambda Value
rmses <- sapply(lambdas, function(l){
            mu <- mean(edx$rating)
            b_i <- edx %>%
                      group_by(movieId) %>%
                      summarize(b_i = sum(rating - mu)/(n()+l))
            b_u <- edx %>% 
                      left_join(b_i, by="movieId") %>%
                      group_by(userId) %>%
                      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
            predicted_ratings <- 
                      validation %>% 
                      left_join(b_i, by = "movieId") %>%
                      left_join(b_u, by = "userId") %>%
                      mutate(pred = mu + b_i + b_u) %>%
                      .$pred
            return(RMSE(predicted_ratings, validation$rating))
        })

lambda <- lambdas[which.min(rmses)]
lambda

#visualize lambda values
qplot(lambdas, rmses) 

#Rmse calculation with optimized lambda which results in 0.864817
Final.RMSE <- mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  RMSE(validation$rating, predicted_ratings)
