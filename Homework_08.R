# -----------------------------------------------------
# Homework 08 
# 22 Mar 2021
# EKG
# -----------------------------------------------------
#
# Library ----------------------------------
library(ggplot2)
library(tidyr)

# Creating a Fake Data Set ----------------------------------

# based on the idea that whistle modulation increases as 
# a function of number of tour boats present.

# data derived from: Perez-Ortega, Betzi, et al. "Dolphin-Watching Boats Affect Whistle Frequency Modulation in Bottlenose Dolphins." Frontiers in Marine Science 8 (2021): 102.

taxiBoat <- rnorm(n=8,mean=25.46,sd=27.10)
head(taxiBoat)

tourBoat <- rnorm(n=8,mean=234.10,sd=232)
head(tourBoat)

dataFrame <- data.frame(taxiBoat,tourBoat)
head(dataFrame)

# using tidyverse to group the inflection points based on whether
# they are taxi or tour boats.
orgFrame <- gather(dataFrame,Boat.Type,PFC.Inflection.Pts,
                     taxiBoat:tourBoat)

# Calculate Stuff ----------------------------------

ANOmodel <- aov(PFC.Inflection.Pts~Boat.Type,data=orgFrame)
print(ANOmodel)
print(summary(ANOmodel))

# print(summary(ANOmodel))
#               Df   Sum Sq Mean Sq F value Pr(>F)    
# Boat.Type     1  4376550 4376550   174.5 <2e-16 ***
# Residuals   482 12086821   25076                   
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

z <- summary(ANOmodel)
str(z)
aggregate(PFC.Inflection.Pts~Boat.Type,data=orgFrame,FUN=mean)
unlist(z)
unlist(z)[7]
ANOsum <- list(Fval=unlist(z)[7],probF=unlist(z)[9])
ANOsum

# ANOsum
# $Fval
# F value1 
# 174.5287 

# $probF
# Pr(>F)1 
# 3.174175e-34 

# Plot Results ----------------------------------

ANOPlot <- ggplot(data=orgFrame,
                  aes(x=Boat.Type,
                      y=PFC.Inflection.Pts,
                      fill=Boat.Type)) +
  geom_boxplot()
print(ANOPlot)

# Homework Questions ----------------------------------

# Now begin adjusting the means of the different groups. Given the sample sizes you have chosen, how small can the differences between the groups be (the “effect size”) for you to still detect a significant pattern (p < 0.05)?

# this was the smallest difference with still detcing a significant pattern
taxiBoat <- rnorm(n=242,mean=25.46,sd=27.10)

tourBoat <- rnorm(n=242,mean=29,sd=28)

# > print(summary(ANOmodel))
#               Df Sum Sq Mean Sq F value Pr(>F)  
# Boat.Type     1   4467    4467   5.465 0.0198 *
# Residuals   482 393982     817                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Alternatively, for the effect sizes you originally hypothesized, what is the minimum sample size you would need in order to detect a statistically significant effect? Again, run the model a few times with the same parameter set to get a feeling for the effect of random variation in the data.

# this was the smallest difference with still detcing a significant pattern
taxiBoat <- rnorm(n=8,mean=25.46,sd=27.10)
tourBoat <- rnorm(n=8,mean=234.10,sd=232)

# print(summary(ANOmodel))
#               Df Sum Sq Mean Sq F value Pr(>F)  
# Boat.Type    1 262947  262947    5.11 0.0403 *
# Residuals   14 720448   51461  