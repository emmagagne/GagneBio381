# Homework #5
# 18 February 2021
# EKG
#----------------------------------------

# Problem 1: Suppose x = 1.1, a = 2.2, and b = 3.3. 
# Assign each expression to the value of the variable z
# and print the value stored in z
x <- 1.1
a <- 2.2
b <- 3.3

z <- x^a^b
print(z)
z <- (x^a)^b
print(z)
z <- 3*x^3+2*x^2+1
print(z)

# Problem 2: Using the rep and seq functions, 
# create the following vectors.

z <- c(1:8,7:1)
print(z)

z <- c(1:5)
b <- rep(x=z,times=z)
print(b)

z <- c(5:1)
c <- rep(x=z,times=1:5)
print(c)

# Problem 3: Create a vector of two random uniform numbers. 
# Using one of R’s inverse trigonometry functions
#(asin(), acos(), or atan()), convert these numbers into polar 
# coordinates

z <- runif(2)
print(z)
r <- (z[1]^2+z[2]^2)^1/2
theta <- atan(z[2]/z[1])
polar_vec <- c(r,theta)
print(polar_vec)

# Problem 4: Using R expressions, update the queue successively

queue <- c("sheep", "fox", "owl", "ant")

# the serpent arrives and gets in line;
queue[5] <- "serpent" 
print(queue)

# the sheep enters the ark;


# the donkey arrives and talks his way to the front of the line


# the serpent gets impatient and leaves;


# the owl gets bored and leaves;


# the aphid arrives and the ant invites him to cut in line.


# Finally, determine the position of the aphid in the line.


# Problem 5: Use R to create a vector of all of the integers 
# from 1 to 100 that are not divisible by 2, 3, or 7.

x <- c(1:100)
print(x)
is.integer(x/2)


