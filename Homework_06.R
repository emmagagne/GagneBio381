# Homework 06 working with matrices, lists, and data frames
# 25 February 2021
# EKG
# -------------------

# Problem 1

n_dims <- sample(3:10, 1)
print(n_dims)

# create a vector of consecutive integers from 1 to n_dims2
con_int <- c(1:n_dims^2)
print(con_int)

# use the sample function to randomly reshuffle these values
samp <- sample(con_int)
print(samp)

# create a square matrix with these elements
my_matrix <- matrix(samp,nrow=5,ncol=5)

# print out the matrix
print(my_matrix)

# find a function in r to transpose the matrix
transformed <- t(my_matrix)

# print it out again and note how it has changed
print(transformed)

# calculate the sum and the mean of the elements in the
# first row and the last row
sum(transformed[1,])
mean(transformed[1,])
sum(transformed[5,])
mean(transformed[5,])

# read about the eigen() function and use it on your matrix
eigen <- eigen(transformed, symmetric = TRUE, 
      only.values = FALSE,
      EISPACK = FALSE)
# $values is a vector of values sorted in decreasing order
# $vectors is a matrix of numbers bounded from -1 to 1

# dig in with the typeof() function to figure out their type
typeof(eigen$values) # it is a double
typeof(eigen$vectors) # it is a double


# Problem 2

my_matrix <- matrix(data=runif(16),ncol=4,nrow=4)
my_matrix

my_vec <- c(1:100)
my_vec <- sample(my_vec)
my_logical <- my_vec < 50
my_logical

my_letters <- c(LETTERS[1:26])
my_letters <- sample(my_letters)
my_letters

list <- list(my_matrix,my_logical,my_letters)
str(list)

# create a new list, which has the element[2,2] from the 
# matrix, the second element of the logical vector, and the
# second element of the letters vector.
new_list <- list(my_matrix[2,2],my_logical[2],my_letters[2])
new_list

# use the typeof() function to confirm the underlying data
# types of each component in this list
typeof(my_matrix[2,2]) # double
typeof(my_logical[2]) # logical
typeof(my_letters[2]) # Character

# combine the underlying elements from the new list into a
# single atomic vector with the c() function.
new_vec <- c(my_matrix[2,2],my_logical[2],my_letters[2])
print(new_vec)

# what is the data type of this vector?
typeof(new_vec) # character


# Problem 3
# Create a data frame with two variables (= columns) and 26
# cases (= rows).

#call the first variable my_unis and fill it with 26 random
# uniform values from 0 to 10
my_unis <- runif(26,0,10)
my_unis

# call the second variable my_letters and fill it with 26 # capital letters in random order.
my_letters <- sample(LETTERS[1:26])
my_letters

data <- data.frame(my_unis,my_letters)
data

# for the first variable, use a single line of code in R to
# select 4 random rows and replace the numerical values in
# those rows with NA.
my_unis[sample(1:26,4)] <- NA
my_unis

# for the first variable, write a single line of R code to
# identify which rows have the missing values.
which(!complete.cases(my_unis))

# for the second variable, sort it in alphabetical order
sort(my_letters)

# calculate the column mean for the first variable.
mean(my_unis, na.rm=TRUE)
