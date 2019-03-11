
start_time <- Sys.time()


#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

library(knitr)
library(ggplot2)
library(gridExtra)

#---------------------------------
#---Start Section :getting data
#---------------------------------


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# introduced to extract the year of release for the movie
movielens <- movielens%>%mutate(year = as.numeric(str_replace(str_replace(str_extract(title, "[(][1-2][0,9][0-9][0-9][)]"),"\\(",""),"\\)","")))


# Validation set will be 10% of MovieLens data
set.seed(1)
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
#---------------------------------
#---End Section :getting data
#---------------------------------
print("-------FIRST---------")



#========================================================================================
#============================Regularization BAGGIG=======================================
#========================================================================================
# in order to determine the regularization coefficient LAMBDA
#  the bagging techique is used as following :
# 1. the edx train dataset is partitioned in 10 partitions
# 2. a loop trhough the created partions will do the following :
#    2.1 each partiton representing 10 % of edx will become a new test data set
#    2.2 the rest of edx data set will become a new train data set
#    2.3 a model will be constructued on the new train dataset
#    2.4 the model will be tested on the new test dataset for a sequence of lambda values
#    2.5 we will retain the lambda value for which the test returns the best RMSE
# 3. at the end of the loop we get the LAMBDA value as the average of individual lambda values obtain in the bagging process
# 4. this LAMBDA value will be used in regularization for the model built on the entire edx trainset


bagged_result=NULL

n_model=10
new_sample0=createDataPartition(edx$rating, times = n_model , p = 0.1, list = FALSE)


for (i in 1:n_model)
{

  new_sample<- new_sample0[, i]
  
  lambdas <- seq(3, 7, 0.5)
  

  edx0 <- edx[-new_sample, ]
  temp <- edx[new_sample, ]
  
  validation0 <- temp %>% 
    semi_join(edx0, by = "movieId") %>%
    semi_join(edx0, by = "userId")
  
  removed <- anti_join(temp, validation0)
  edx0 <- rbind(edx0, removed)
  
  rm(temp, removed)
  
  rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <- edx0 %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx0 %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    year_genre_rating <- edx0 %>% 
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      group_by(year, genres) %>%
      summarize(b_y_g = mean(rating - mu - b_i - b_u))
    
    
    
    predicted_ratings <- 
      validation0 %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(year_genre_rating, by = c("year","genres")) %>%
      mutate(pred = mu + b_i + b_u + b_y_g) %>%
      .$pred
    
    return(sqrt(mean((predicted_ratings - validation0$rating)^2)))
    
  })

  print(paste("the interation = ",i, " with lambda = ",lambdas[which.min(rmses)] ))
  
  #min(rmses)
  if (is.null(bagged_result))
    bagged_result = lambdas[which.min(rmses)]
  else
    bagged_result=((i-1)*bagged_result+ lambdas[which.min(rmses)])/(i)
  
}

print("================ FINAL LAMBDA that will be used in regularisation==========================")
bagged_result

#========================================================================================
#========================================================================================
#========================================================================================



lambda <- bagged_result
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n() + lambda))


b_u <- edx %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))


b_y_g <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(year, genres) %>%
  summarize(b_y_g = mean(rating - mu - b_i - b_u) )


predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y_g, by = c("year","genres")) %>%
  mutate(pred = mu + b_i + b_u + b_y_g) %>%
  .$pred


RMSE <- sqrt(mean((predicted_ratings - validation$rating)^2))

paste('RMSE = ', RMSE)
end_time <- Sys.time()
end_time - start_time
