# Library ----------------------------------
library(ggplot2)
library(tidyr)

# Creating a Fake Data Set ----------------------------------

# based on the idea that whistle modulation increases as 
# a function of number of tour boats present.

# data derived from: Perez-Ortega, Betzi, et al. "Dolphin-Watching Boats Affect Whistle Frequency Modulation in Bottlenose Dolphins." Frontiers in Marine Science 8 (2021): 102.

# Get Data ------------------------------------------
# Global Variables
taxiBoat <- rnorm(n=242,mean=25.46,sd=27.10)
tourBoat <- rnorm(n=242,mean=234.10,sd=232)
dataFrame <- data.frame(taxiBoat,tourBoat)

orgFrame <- gather(dataFrame,Boat.Type,PFC.Inflection.Pts,
                   taxiBoat:tourBoat)

# -----------------------------------------------------
# FUNCTION anova_sum
# description generates an ANOVA summary on the data in the data frame
# inputs: data frame of boat type and inflection point data
# outputs: ANOVA summary
#######################################################
anova_sum <- function(x=orgFrame) {
  ANOmodel <- aov(PFC.Inflection.Pts~Boat.Type,data=orgFrame)
  z <- summary(ANOmodel)
  aggregate(PFC.Inflection.Pts~Boat.Type,data=orgFrame,FUN=mean)
  unlist(z)
  unlist(z)[7]
  ANOsum <- list(Fval=unlist(z)[7],probF=unlist(z)[9])
  ANOsum
  
  return(ANOsum)

} # end of anova_sum
# -----------------------------------------------------

anova_sum()
# > anova_sum()
# $Fval
# F value1 
# 281.8467 

# $probF
# Pr(>F)1 
# 3.845925e-50 

```
# -----------------------------------------------------
# FUNCTION plot_data 
# description plots the data from the data frame
# inputs: data frame of boat types and inflection points
# outputs: box plot comparing inflection points across boat type
#######################################################
plot_data <- function(x=orgFrame) {
  ANOPlot <- ggplot(data=orgFrame,
                    aes(x=Boat.Type,
                        y=PFC.Inflection.Pts,
                        fill=Boat.Type)) +
    geom_boxplot()
  return(print(ANOPlot))
# function body


} # end of plot_data
# -----------------------------------------------------

plot_data()

```

# The resulting box plot looks like this;  
# ![box plot](https://github.com/emmagagne/GagneBio381/blob/main/homework8plot.png?raw=true)


# Using a different type of data set to generate a linear regression

boatPresence <- rnorm(n=242,mean=3.3,sd=0.30)
inflecPoints <- rnorm(n=242,mean=234.10,sd=232)

ID <- 1:242

reg_data <- data.frame(ID,boatPresence,inflecPoints)
head(reg_data)
str(reg_data)

# -----------------------------------------------------
# FUNCTION linreg_sum
# description: generates a summary of linear regression analysis
# inputs: two continuous variables in a data frame along with ID
# outputs: linear regression summary
#######################################################
linreg_sum <- function(x=boatPresence,y=inflecPoints) {
  reg_model <- lm(inflecPoints~boatPresence, data=reg_data)
  summary(reg_model)
  z <- unlist(summary(reg_model))
  reg_stats <- list(intercept=z$coefficients1,
                    slope=z$coefficients2,
                    intercept_p=z$coefficients7,
                    slope_p=z$coefficients8, # most common for signif
                    r2=z$r.squared)
  return(reg_stats)
} # end of anova_sum
# -----------------------------------------------------

linreg_sum()

# > linreg_sum()
# $intercept
# [1] 58.65596
#
# $slope
# [1] 49.38302
#
# $intercept_p
# [1] 0.7105408
#
# $slope_p
# [1] 0.3018502
#
# $r2
# [1] 0.004441044

```
# -----------------------------------------------------
# FUNCTION regression_plot
# description plots the data from the data frame
# inputs: data frame of boat noise levels and inflection points
# outputs: box plot showing inflection points as a function of boat noise levels
#######################################################
regression_plot <- function(x=reg_data) {
  reg_data <- data.frame(ID,boatPresence,inflecPoints)
  reg_plot <- ggplot(reg_data) +
    aes(x=boatPresence,y=inflecPoints) +
    geom_point() +
    stat_smooth(method=lm,se=0.99) # default se=0.95
  
  return(print(reg_plot))
  # function body
  
  
} # end of plot_data
# -----------------------------------------------------

regression_plot()

```


