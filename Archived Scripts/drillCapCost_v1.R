# Script Info -------------------------------------------------------------
# drillCapCost_v1.R (Drilling costs as function of depth)
# Version 1
# 06/24/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads *.csv files, queries table to get total costs by distinct API #,
#     uses nlsLM to fit resulting data as f(depth)


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Windows
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Functions directory
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"

# # Mac
# # Prepared data directory
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# # Plot directory
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# # Functions directory
# fin <- "/Users/john/Dropbox/CLEAR/DOGM Data/Functions"
# # Working directory
# work_root <- "/Users/john//Dropbox/CLEAR/DOGM Data"

setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here


# Libraries ---------------------------------------------------------------
library(sqldf)
library(zoo)

# Load required data files ------------------------------------------------

# CPI index data
load(file.path(data_root, "cpi.rda"))

# Well cost data
data <- read.csv(file.path(data_root, "well_file_data - COSTS.csv"))

# Select desired columns (API#, date, drilling costs, completion costs, days,
# and depth)
data <- data[,c(1:6)]


# Inflation adjustment ----------------------------------------------------

# Change DATE from string to date
data$DATE <- as.Date(data$DATE)

# Add column of truncated dates (year-month)
data$tdate <- as.Date(as.yearmon(data$DATE))

# Drop NA observations
data <- na.omit(data)

# Correct erroneous date entries
data$tdate[which(data$tdate == "3006-11-01")] <- as.Date("2006-11-01")
data$tdate[which(data$tdate == "3006-06-01")] <- as.Date("2006-06-01")

# Create dataframe from cpi.z object
cpi <- data.frame(index(cpi.z), coredata(cpi.z), row.names = NULL)

# Rename columns
names(cpi) <- c("ind.date", "value")

# Subset to max and min dates in "data" dataframe
cpi <- cpi[which((cpi$ind.date >= min(data$tdate) &
                  cpi$ind.date <= max(data$tdate))),]

# CPI Index - set to CPI Annual Average for 2012 (229.594)
cpi.index <- 229.594

# Predefine adjusted cost vectors
drill <- rep(0, times = nrow(data))
compl <- drill

# Calculate adjusted costs
for (i in 1:nrow(cpi)) {
  ind <- which(data$tdate == cpi$ind.date[i])
  if (length(ind) > 0){
    drill[ind] <- data$DRILLING[ind]*(cpi.index/cpi$value[i])
    compl[ind] <- data$COMPLETION[ind]*(cpi.index/cpi$value[i])
  }
}

# Combine adjusted vectors with "data" dataframe as additional columns
data <- cbind(data, drill, compl)


# SQL Queries -------------------------------------------------------------

# Compile dataframe with columns for API #, total drilling cost, date, and depth
cost <- sqldf("select distinct(API), sum(drill), sum(compl), max(DEPTH), max(DAYS)
              from data
              group by API")

# Rename columns
names(cost) <- c("API", "drill", "completion", "depth", "days")
