# -----------------------------------------------------
# Homework 11 
# 12 Apr 2021
# EKG
# -----------------------------------------------------
#

```
Run regression model and extract stats
##################################################

# function: regStats
# fits linear model, extracts statistics
# input: 2-column data frame (x and y)
# output: slope, p-value, and r2
#------------------------------------------------- 
regStats <- function(d=NULL) {
  if(is.null(d)) {
    xVar <- runif(10)
    yVar <- runif(10)
    d <- data.frame(xVar,yVar)
  }
  . <- lm(data=d,d[,2]~d[,1])
  . <- summary(.)
  statsList <- list(Slope=.$coefficients[2,1],
                    pVal=.$coefficients[2,4],
                    r2=.$r.squared)
  return(statsList)
}

# Body of script for batch processing of regression models
library(TeachingDemos)

#--------------------------------------------
# Global variables
file_folder <- "dolphinData/"
file_names <- list.files(path=file_folder)
nFiles <- 2
fileOut <- "StatsSummary.csv"
#-----------------------------------------------------

# Create data frame to hold file summary statistics
ID <- seq_along(file_names)
file_name <- file_names
slope <- rep(NA,length(file_names))
p_val <- rep(NA,length(file_names))
r2 <- rep(NA,length(file_names))

statsOut <- data.frame(ID,file_name,slope,pVal,r2)
statsOut

# batch processing
for (i in seq_along(file_names)) {
  data <- read.table(file=paste(file_folder,file_names[i],
                                sep=""),
                     sep=",",
                     header=TRUE)
  d_clean <- data[complete.cases(data),] # subset for clean cases (no NAs)
  
  . <- regStats(d_clean) # pull out regression stats from clean file
  stats_out[i,3:5] <- unlist(.) # unlist, copy into last 3 columns
}


# set up output file and incorporate time stamp and minimal metadata
write.table(cat("# Summary stats for ",
                "batch processing of regression models","\n",
                "# timestamp: ",as.character(Sys.time()),"\n",
                "# EKG","\n",
                "# ------------------------", "\n",
                "\n",
                file=fileOut,
                row.names="",
                col.names="",
                sep=""))

# now add the data frame
write.table(x=statsOut,
            file=fileOut,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)
```

