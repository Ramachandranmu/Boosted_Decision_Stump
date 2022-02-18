library(tree)
library(ISLR)
library(MASS)

# The function split_subtrees is used for splitting the tree into left_subtree and right_subtree with the split value received as argument
# This function also differentiates between the variables to be used for the split (lstat/rm) with the attribute counter 
# the function calculates the mean of each of the left and right subtrees corresponding the the variable(lstat/rm)
# the function returns the total RSS calculated
split_subtrees <- function(train.X, split, attribute_counter){
  lstat_left_subtree = vector()
  rm_left_subtree = vector()
  medv_left_subtree = vector()
  lstat_right_subtree = vector()
  rm_right_subtree = vector()
  medv_right_subtree = vector()
  i<-1
  k<-1
  y_hat_mean_left_subtree <- 0
  y_hat_mean_right_subtree <- 0
  RSS_right_subtree <- 0
  RSS_left_subtree <- 0
  if(attribute_counter == 0)
  {
      for(row in 1:nrow(train.X))
      {
        if(train.X[row,2] < split)
        {
          lstat_left_subtree[i]<- train.X[row,2]
          rm_left_subtree[i]<- train.X[row,1]
          medv_left_subtree[i]<- train.X[row,3]
          train.X.left_subtree <- data.frame(lstat_left_subtree, rm_left_subtree, medv_left_subtree)
          i<-i+1
        }else
          {
            lstat_right_subtree[k]<- train.X[row,2]
            rm_right_subtree[k]<- train.X[row,1]
            medv_right_subtree[k]<- train.X[row,3]
            train.X.right_subtree <- data.frame(lstat_right_subtree, rm_right_subtree, medv_right_subtree)
            k<-k+1
          }
      }
    if(exists("train.X.left_subtree")){    
      y_hat_mean_left_subtree <- mean(train.X.left_subtree$medv_left_subtree)
    }
    if(exists("train.X.right_subtree")){
      y_hat_mean_right_subtree <- mean(train.X.right_subtree$medv_right_subtree)
    }
  }else
    {
      for(row in 1:nrow(train.X))
      {
        if( train.X[row,1] < split)
        {
          lstat_left_subtree[i]<- train.X[row,2]
          rm_left_subtree[i]<- train.X[row,1]
          medv_left_subtree[i]<- train.X[row,3]
          train.X.left_subtree <- data.frame(lstat_left_subtree, rm_left_subtree, medv_left_subtree)
          i<-i+1
        }else
        {
          lstat_right_subtree[k]<- train.X[row,2]
          rm_right_subtree[k]<- train.X[row,1]
          medv_right_subtree[k]<- train.X[row,3]
          train.X.right_subtree <- data.frame(lstat_right_subtree, rm_right_subtree, medv_right_subtree)
          k<-k+1
        }
      }
      if(exists("train.X.left_subtree")){ 
        y_hat_mean_left_subtree <- mean(train.X.left_subtree$medv_left_subtree)  
      }
      if(exists("train.X.right_subtree")){
        y_hat_mean_right_subtree <- mean(train.X.right_subtree$medv_right_subtree)
      }
    } 
  if(exists("train.X.right_subtree")){
    RSS_right_subtree <- calculate_RSS(train.X.right_subtree, y_hat_mean_right_subtree)  
  }
  if(exists("train.X.left_subtree")){ 
    RSS_left_subtree <- calculate_RSS(train.X.left_subtree, y_hat_mean_left_subtree)
  }
  return (RSS_left_subtree+RSS_right_subtree)
}

# Function calculate_RSS is used for calculating the RSS of the subtress given as input arguments
# function returns the value of the RSS
calculate_RSS <- function(train.X.subtree, y_hat_mean_subtree){
  RSS_subtree <- 0
  for(row in 1:nrow(train.X.subtree)){
    RSS_subtree <- RSS_subtree + (train.X.subtree[row,3]-y_hat_mean_subtree)^2
  }
  return (RSS_subtree)
}

# function DS is the main function 
# this function defines the split_vector with the different kinds of split to lstat and rm variables 
# the variable with the least RSS is chosen and the split value is given as input the test dataset to compute the RSS
# the test_MSE is calculated and printed for the best split
DS <- function(){

  data(Boston)
  Boston <- na.omit(Boston)
  attach(Boston)
  Boston <- subset(Boston, select = -c(crim,zn,indus,chas,nox,age,dis,rad,tax,ptratio,black))
  Boston <- data.frame(Boston)

  tree.Boston <- tree(medv~lstat+rm,Boston)

  set.seed(0701)
  size_of_boston <- nrow(Boston);
  train <- sample(1:nrow(Boston), size_of_boston/2)
  train.X <- Boston[train,]
  test.X <- Boston[-train,]
  train.y <- medv[train]
  test.y <- medv[-train]

  vector <- vector()
  
  split_vector_lstat <- c(1.8,1.9,2.3,3.5,4.9,5.4,6.3,7.9,8.5,10.3,13.5,15.3,17.5,20.3,23.5,26.7,29.9,33.3,35.7,37.9)
  lstat_RSS_vector <- vector()
  for(i in 1:length(split_vector_lstat)){
    lstat_RSS_vector <- c(lstat_RSS_vector, split_subtrees(train.X, split_vector_lstat[i], 0))
  }

  split_vector_rm <- c(3.6,3.7,3.8,3.9,4.3,4.5,4.7,4.9,5.5,5.7,5.9,6.3,6.5,6.7,6.9,7.5,8.1,8.3,8.5,8.7)
  rm_RSS_vector <- vector()
  for(i in 1:length(split_vector_rm)){
    rm_RSS_vector <- c(rm_RSS_vector, split_subtrees(train.X, split_vector_rm[i], 1))
  }

  lstat_RSS_min <- min(split_vector_lstat[match(c(min(lstat_RSS_vector)),lstat_RSS_vector)])
  rm_RSS_min <- min(split_vector_rm[match(c(min(rm_RSS_vector)),rm_RSS_vector)])

  if(rm_RSS_min < lstat_RSS_min){
    split_value_test <- rm_RSS_min  
    attribute_counter <- 1
  }else{
    split_value_test <- lstat_RSS_min
    attribute_counter <- 0
  }

  test_RSS_value <- split_subtrees(test.X,split_value_test,attribute_counter)

  print(c("The minimum training RSS value for lstat is : ", min(lstat_RSS_vector)))
  print(c("The training s-value for lstat is : ", lstat_RSS_min))
  print(c("The minimum training RSS value for rm is : ", min(rm_RSS_vector)))
  print(c("The training s-value for rm is : ", rm_RSS_min))
  print(c("The training MSE for Boston Dataset is : ", min(rm_RSS_vector)/nrow(train.X)))

  print(c("The test RSS value for rm is : ", test_RSS_value))
  print(c("The test MSE for Boston Dataset is : ", test_RSS_value/nrow(test.X)))
}

















