# function plot_test_MSE plots the MSE of the test dataset obtained from BDS2.R by invoking BDS() function against different values of B
plot_test_MSE <- function(){
  source("BDS2.R")
  MSE_vector <- vector()
  B_value_vector <- c(100,200,300,400,500,700,900)
  MSE_vector <- c(MSE_vector, BDS(100),BDS(200),BDS(300),BDS(400),BDS(500),BDS(700),BDS(900))
  plot(B_value_vector,MSE_vector)
  print(MSE_vector)
}