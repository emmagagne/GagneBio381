# -----------------------------------------------------
# Homework 10 
# 06 Apr 2021
# EKG
# -----------------------------------------------------
#
my_vec <- c(rep(0,8),1,3,5)
my_vec <- sample(my_vec)
my_vec

# Using a for loop, write a function to calculate the number of zeroes in a numeric vector. Before entering the loop, set up a counter variable counter <- 0. Inside the loop, add 1 to counter each time you have a zero in the matrix. Finally, use return(counter) for the output.

# -----------------------------------------------------
# FUNCTION zeroes_vector
# description: calculates the number of zeroes in a numeric vector
# inputs: vector containing numerics
# outputs: counter displaying number of zeroes in vector
#######################################################
zeroes_vector <- function(x=NULL) {
if(is.null(x)) {
  my_vec <- c(rep(0,8),1,3,5)
  my_vec <- sample(my_vec)
} # end of if statement
  
counter <- 0
for (i in seq_along(my_vec)) {
  if (my_vec[i]==0) {
    counter <- counter + 1
}  # end of if statement
} # end of for loop
  
  return(counter)
} # end of zeroes_vector
# -----------------------------------------------------
zeroes_vector()

# zeroes_vector function is a sucess!

# Use subsetting instead of a loop to rewrite the function as a single line of code.

# -----------------------------------------------------
# FUNCTION zeroes_vec_new 
# description counts the number of zeroes in a vector
# inputs: vector of any length of numerics
# outputs: counter of the number of zeroes in your vector
#######################################################
zeroes_vec_new <- function(x=NULL) {
  if(is.null(x)) {
    my_vec <- c(rep(0,8),1,3,5)
    my_vec <- sample(my_vec)
  } # end of if statement
# function body

counter <- sum((my_vec)[]==0)
return(counter)

} # end of zeroes_vec_new
# -----------------------------------------------------
zeroes_vec_new()

# new function works as well!

# Write a function that takes as input two integers representing the number of rows and columns in a matrix. The output is a matrix of these dimensions in which each element is the product of the row number x the column number.

# -----------------------------------------------------
# FUNCTION product_matrix 
# description: produced as matrix of given dimension which each element is the product of the row number and column number.
# inputs: two integers representing the number of rows and columns in a matrix.
# outputs: matrix of dimensions where each element is the product of the row number and column number.
#######################################################
product_matrix <- function(n=4,c=4) {
  my_mat <- matrix(NA, nrow=n, ncol=c)
  for (i in 1:nrow(my_mat)) { 
    for(j in 1:ncol(my_mat)) { 
      my_mat[i,j] <- i*j
    } 
  } 
# function body

return(my_mat)

} # end of product_matrix
# -----------------------------------------------------
product_matrix()

# Use the code from the April 8th lecture (Randomization Tests) to design and conduct a randomization test for some of your own data. You will need to modify the functions that read in the data, calculate the metric, and randomize the data. Once those are set up, the program should run correctly calling your new functions. Also, to make your analysis fully repeatable, make sure you set the random number seed at the beginning (use either set.seed() in base R, or char2seed in the TeachingDemos package

# Preliminaries ----------------------------------

library(ggplot2)
library(TeachingDemos)

set.seed(100)
char2seed("espresso withdrawal")
options(digits=10) # optional
char2seed("espresso withdrawal",set=FALSE)

# create treatment groups
trt_group <- c(rep("BoatPresent",5),rep("BoatAbsent",5))
print(trt_group)

# create response variable
z <- c(runif(5) + 10, runif(5) + 2)
print(z)

# combine vectors into a data frame
df <- data.frame(trt=trt_group,res=z)
print(df)

# look at means in the two groups
obs <- tapply(df$res,df$trt,mean)
print(obs)

# create a simulated data set

# set up a new data frame
df_sim <- df
df_sim$res <- sample(df_sim$res)
print(df_sim)

# look at mean in the to groups of randomized data
sim <- tapply(df_sim$res,df_sim$trt,mean)
print(sim)

# build functions ----------------------------------

# -----------------------------------------------------
# FUNCTION read_data 
# description read in (or generate) data set for analysis
# inputs: file name (or nothing, as in this demo)
# outputs: column data frame of observed data (ID,x,y)
#######################################################
read_data <- function(z=NULL) {
  if(is.null(z)) {
    boatpresence <- 1:20
    modulation <- boatpresence + 10*rnorm(20)
    df <- data.frame(ID=seq_along(boatpresence),
                     boatpresence,
                     modulation) }
  #   df <- read.table(file=z,
  #                   header=TRUE
  #                   stringAsFactors=FALSE)
  
  # function body
  
  return(df)
  
} # end of read_data
# -----------------------------------------------------
 read_data()

# -----------------------------------------------------
# FUNCTION get_metric 
# description calculate metric for randomization test
# inputs: 2-column data frame for regression
# outputs: regression slope
#######################################################
get_metric <- function(z=NULL) {
  if(is.null(z)){
    boatpresence <- 1:20
    modulation <- boatpresence + 10*rnorm(20)
    z <- data.frame(ID=seq_along(boatpresence),
                     boatpresence,
                     modulation) }
  
  . <- lm(z[,3]~z[,2])
  . <- summary(.)
  . <- .$coefficients[2,1]
  slope <- .
  
  # function body
  
  return(slope)
  
} # end of get_metric
# -----------------------------------------------------
 get_metric()

# -----------------------------------------------------
# FUNCTION shuffle_data 
# description: randomize data for regression analysis
# inputs: 3 column data frame (ID, xvar, yvar)
# outputs: 3 column data frame (ID, xvar, yvar)
#######################################################
shuffle_data <- function(z=NULL) {
  if(is.null(z)) {
    boatpresence <- 1:20
    modulation <- boatpresence + 10*rnorm(20)
    z <- data.frame(ID=seq_along(boatpresence),
                     boatpresence,
                     modulation) }
  z[,3] <- sample(z[,3])
  
  # function body
  
  return(z)
  
} # end of shuffle_data
# -----------------------------------------------------
shuffle_data()

# -----------------------------------------------------
# FUNCTION get_pval 
# description: calculate p value from simulation
# inputs: list of observed metric and vector of simulated metrics
# outputs: lower and upper tail probability value
#######################################################
get_pval <- function(z=NULL) {
  if(is.null(z)){
    boatpresence <- 1:20
    modulation <- boatpresence + 10*rnorm(20)
    z <- data.frame(ID=seq_along(boatpresence),
                    boatpresence,
                    modulation) }
    z <- list(z[,3], sample(z[,3]))
  p_lower <- mean(z[[2]]<=z[[1]])
  p_upper <- mean(z[[2]]>=z[[1]])
  
  # function body
  
  return(c(pL=p_lower,pU=p_upper))
} 
# end of get_pval
# -----------------------------------------------------
get_pval()

# -----------------------------------------------------
# FUNCTION plot_ran_test 
# description: create a ggplot of histogram of simulated values
# inputs: list of observed metrics and vector of simulated metrics
# outputs: saved ggplot graph
#######################################################
plot_ran_test <- function(z=NULL) {
  
  if(is.null(z)){
    z <- list(rnorm(1),rnorm(1000)) }
  df <- data.frame(ID=seq_along(z[[2]]), sim_x=z[[2]])
  p1 <- ggplot(data=df, mapping=aes(x=sim_x))
  p1 + geom_histogram(mapping=aes(fill=I("goldenrod"),
                                  color=I("black"))) +
    geom_vline(aes(xintercept=z[[1]],col="blue"))
  
  # function body
  
} # end of plot_ran_test
# -----------------------------------------------------
 plot_ran_test()


# Run Analysis ----------------------------------

n_sim <- 1000 # number of simulated data sets
x_sim <- rep(NA,n_sim) # set up empty vector for simulated slopes
df <- read_data() # get fake data
x_obs <- get_metric(df) # get slope of observed data

for (i in seq_len(n_sim)) {
  x_sim[i] <- get_metric(shuffle_data(df))
}

slopes <- list(x_obs,x_sim)
get_pval(slopes)
plot_ran_test(slopes)

# PUT PLOT HERE

# For comparison, calculate in R the standard statistical analysis you would use with these data. How does the p-value compare for the standard test versus the p value you estimated from your randomization test? If the p values seem very different, run the program again with a different starting seed (and/or increase the number of replications in your randomization test). If there are persistent differences in the p value of the standard test versus your randomization, what do you think is responsible for this difference?

# Making a fake data set
boatpresence <- 1:20
modulation <- boatpresence + 10*rnorm(20)
z <- data.frame(ID=seq_along(boatpresence),
                boatpresence,
                modulation)

# Regression analysis in R
reg_model <- lm(modulation~boatpresence, data=z) # lm means linear model

# summary has the elements that we need
summary(reg_model)

z <- unlist(summary(reg_model))

reg_stats <- list(intercept=z$coefficients1,
                  slope=z$coefficients2,
                  intercept_p=z$coefficients7,
                  slope_p=z$coefficients8, # most common for signif
                  r2=z$r.squared)
print(reg_stats)

# The p-value we get from running the linear regression is 0.012, which is not significant. The p-value obtained from the randomization test was 0.65, which is also not significant.