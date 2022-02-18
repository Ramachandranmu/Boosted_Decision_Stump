library(ISLR)
library(MASS)
library(tree)

# Function cal_decision_stump calculates the minimum RSS of either lstat or rm variable and returns a vector - lstat_vector or rm_vector
# these vectors hold the minimum RSS and the corresponding variable name(lstat/rm) for each value of B
cal_decision_stump <- function(train.X,train.y){
  lstat_RSS_vector <- vector(length = nrow(train.X))
  rm_RSS_vector <- vector(length = nrow(train.X))
  
  for(i in 1:nrow(train.X)){
    split_lstat <- train.X$lstat[i]
    lstat_RSS_vector[i] <- split_lstat_subtree(split_lstat,train.X,train.y)
    split_rm <- train.X$rm[i]
    rm_RSS_vector[i]<- split_rm_subtree(split_rm,train.X,train.y)
  }
  rm_vector = vector()
  lstat_vector = vector()
  
  if(min(rm_RSS_vector) < min(lstat_RSS_vector)){
    split_index =which.min(rm_RSS_vector)
    split_value <- train.X$rm[split_index]
    
    left_subtree_sum <- 0
    left_subtree_counter <- 0
    left_subtree_mean <- 0
    right_subtree_sum <- 0
    right_subtree_counter <- 0
    right_subtree_mean <- 0
    
    for(i in 1:nrow(train.X)){
      if (train.X$rm[i] < split_value)
      {
        left_subtree_sum = left_subtree_sum + train.y[i]
        left_subtree_counter = left_subtree_counter + 1
      }
      if(train.X$rm[i]>= split_value){
        right_subtree_sum = right_subtree_sum + train.y[i]
        right_subtree_counter = right_subtree_counter + 1
      }
    }
    left_subtree_mean <- left_subtree_sum/left_subtree_counter
    right_subtree_mean <- right_subtree_sum/right_subtree_counter
    rm_vector = c("rm",split_value,left_subtree_mean,right_subtree_mean)
    return (rm_vector)
  }
  else{
    split_index = which.min(lstat_RSS_vector)
    split_value <- train.X$lstat[split_index]
    
    left_subtree_sum <- 0
    left_subtree_counter <- 0
    left_subtree_mean <- 0
    right_subtree_sum <- 0
    right_subtree_counter <- 0
    right_subtree_mean <- 0
    
    for(i in 1:nrow(train.X)){
      if (train.X$lstat[i] < split_value)
      {
        left_subtree_sum = left_subtree_sum + train.y[i]
        left_subtree_counter = left_subtree_counter + 1
      }
      if(train.X$lstat[i] >= split_value){
        right_subtree_sum = right_subtree_sum + train.y[i]
        right_subtree_counter = right_subtree_counter + 1
      }
    }
    left_subtree_mean <- left_subtree_sum/left_subtree_counter
    right_subtree_mean <- right_subtree_sum/right_subtree_counter
    lstat_vector <- c("lstat",split_value,left_subtree_mean,right_subtree_mean)
    return (lstat_vector)
  }
}

# Function split_lstat_subtree handles the splitting of the tree with a split - s corresponding to lstat into left and right subtrees
# The function calculates the mean of both the left subtree and right subtree and calculates the RSS of both the trees
# the function returns the calculated RSS
split_lstat_subtree <- function(s,train.X,train.y) {
  left_subtree_sum <- 0
  left_subtree_counter <- 0
  left_subtree_mean <- 0
  right_subtree_sum <- 0
  right_subtree_counter <- 0
  right_subtree_mean <- 0
  left_subtree_RSS <- 0
  right_subtree_RSS <- 0
  for(i in 1:nrow(train.X)){
    if (train.X$lstat[i] < s)
    {
      left_subtree_sum = left_subtree_sum + train.y[i]
      left_subtree_counter = left_subtree_counter + 1
    }
    if(train.X$lstat[i] >= s){
      right_subtree_sum = right_subtree_sum + train.y[i]
      right_subtree_counter = right_subtree_counter + 1
    }
  }
  
  left_subtree_mean <- left_subtree_sum/left_subtree_counter
  right_subtree_mean <- right_subtree_sum/right_subtree_counter
  
  for(i in 1:nrow(train.X)){
    if (train.X$lstat[i] < s )
    {
      left_subtree_RSS <- left_subtree_RSS + (train.y[i]-left_subtree_mean)^2
    }
    if(train.X$lstat[i] >= s){
      right_subtree_RSS <-  right_subtree_RSS + (train.y[i]-right_subtree_mean)^2
    }
  }
  lstat_RSS <- left_subtree_RSS + right_subtree_RSS
  return(lstat_RSS)
}

# Function split_rm_subtree handles the splitting of the tree with a split - s corresponding to lstat into left and right subtrees
# The function calculates the mean of both the left subtree and right subtree and calculates the RSS of both the trees
# the function returns the calculated RSS
split_rm_subtree <- function(s,train.X,train.y){
  left_subtree_sum <- 0
  left_subtree_counter <- 0
  left_subtree_mean <- 0
  right_subtree_sum <- 0
  right_subtree_counter <- 0
  right_subtree_mean <- 0
  left_subtree_RSS <- 0
  right_subtree_RSS <- 0
  
  for(i in 1:nrow(train.X)){
    if (train.X$rm[i] < s)
    {
      left_subtree_sum = left_subtree_sum+ train.y[i]
      left_subtree_counter = left_subtree_counter + 1
    }
    if(train.X$rm[i] >= s){
      right_subtree_sum = right_subtree_sum + train.y[i]
      right_subtree_counter = right_subtree_counter + 1
    }
  }
  
  left_subtree_mean <- left_subtree_sum/left_subtree_counter
  right_subtree_mean <- right_subtree_sum/right_subtree_counter
  
  for(i in 1:nrow(train.X)){
    if (train.X$rm[i] < s )
    {
      left_subtree_RSS <- left_subtree_RSS + (train.y[i]-left_subtree_mean)^2
    }
    if(train.X$rm[i] >= s){
      right_subtree_RSS <-  right_subtree_RSS +(train.y[i]-right_subtree_mean)^2
    }
  }
  rm_RSS <- left_subtree_RSS + right_subtree_RSS
  return(rm_RSS)
}

# Function BDS is the main function which accepts an input argument B_value, the user is prompted to input the value of B
# This function calculates the updated label, the prediction funciton 
# the function returns the Test MSE for the Boston dataset
BDS <- function(B_value){
  
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
  
  learning_rate <- 0.01
  #B_value <- 1000
  
  decision_stump_matrix <- matrix(nrow=B_value,ncol=4)
  f_hat<- matrix(nrow=B_value,ncol=nrow(train.X))
  for(i in 1:B_value){
    decision_stump_matrix[i,]<- cal_decision_stump(train.X,train.y)
    for(j in 1:nrow(train.X)){
      if(decision_stump_matrix[i,1]=="lstat"){
        lstat_split_value <- as.numeric(decision_stump_matrix[i,2])
        lstat_left_subtree_prediction_val <- as.numeric(decision_stump_matrix[i,3])
        lstat_right_subtree_prediction_val <- as.numeric(decision_stump_matrix[i,4])
        if(train.X$lstat[j] < lstat_split_value)
          {
            f_hat[i,j] <- lstat_left_subtree_prediction_val
          }else{
            f_hat[i,j] <- lstat_right_subtree_prediction_val
          }
      }else if(decision_stump_matrix[i,1]=="rm")
        {
          rm_split_value <- as.numeric(decision_stump_matrix[i,2])
          rm_left_subtree_prediction_val <- as.numeric(decision_stump_matrix[i,3])
          rm_right_subtree_prediction_val <- as.numeric(decision_stump_matrix[i,4])
          if(train.X$rm[j] < rm_split_value)
            {
              f_hat[i,j] <- rm_left_subtree_prediction_val
            }else{
              f_hat[i,j] <- rm_right_subtree_prediction_val
            }
        }
      train.y[j] <- train.y[j]-learning_rate*f_hat[i,j]
    }
    print(c(decision_stump_matrix[i,1],decision_stump_matrix[i,2]))
  }
  
  squareDifference <- 0
  for(i in 1:nrow(test.X)){
    predictionRule = sum(0.01*f_hat[,i])
    squareDifference <- squareDifference+(test.X$medv[i]- (predictionRule))^2
  }
  print(c("The test MSE for BDS is : " ,squareDifference/(nrow(test.X))))
  return (squareDifference/(nrow(test.X)))
}