# Function Info -----------------------------------------------------------
# Name:      drillCapCostUpdate.R (Drilling and Completion Capital Cost Model Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# well_file_data - COSTS.csv - A *.csv file containing well cost data collected 
# from DOGM database. This input is implied, and the majority of this function 
# deals with restructuring that file, adjusting its costs for inflation (the 
# only user input that can be easily adjusted here), and finally fitting the 
# resulting data. The file is a result of data collection efforts by an Univ. of
# Utah ICSE research group during the summer of 2011 from a random group of 100
# wells in the Uinta Basin drilled between 1999 - 2011.

# cpi.rda - another implied object, this file is a "zoo" vector object giving
# CPI index values on a monthly basis.

# path - List object containing directory paths for file I/O

# basis - CPI index value being used for inflation adjustment

# ver - File version number


# Outputs -----------------------------------------------------------------

# drillCost.data - data.frame with total inflation adjusted development costs
# and measured depths (in feet) for ~65 wells randomly selected from the DOGM
# database. Note - costs in this file are cumulative.

# drillCost.fit - lm() fit object for modeling development costs as an
# exponential function of well depth.


# Description -------------------------------------------------------------

# This function loads a *.csv file containing records for the drilling and 
# completion costs for a variety of wells that were collected from DOGM database
# well report files. These costs are adjusted for inflation and maximum value
# for each well is taken as the total development cost. After trying various
# fitting approaches, the following model was found to give the best fit to the
# cost data:

# cost = a * exp(depth) + b

# where cost is the total capital cost for drilling a well (inflation adjusted 
# to "basis" dollars), depth is the measured well depth (in feet), and "a" and
# "b" are fitted constants.

# Realistically, the only way to "update" this drilling capital costs with this 
# function is just to update the fit values for inflation (if a new basis value 
# is selected). If additional cost data points are to be added, its advisable to
# generate a new function based on the data structure of the additional points.


# Function ----------------------------------------------------------------
drillCapCostUpdate <- function(path, basis, ver) {
  
  # Load required data files ------------------------------------------------
  
  # CPI index data
  load(file.path(path$data, "cpi.rda"))
  
  # Well cost data
  data <- read.csv(file.path(path$raw, "well_file_data - COSTS.csv"))
  
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
  
  # Predefine adjusted cost vectors
  drill <- rep(0, times = nrow(data))
  compl <- drill
  
  # Calculate adjusted costs
  for (i in 1:nrow(cpi)) {
    ind <- which(data$tdate == cpi$ind.date[i])
    if (length(ind) > 0){
      drill[ind] <- data$DRILLING[ind]*(basis/cpi$value[i])
      compl[ind] <- data$COMPLETION[ind]*(basis/cpi$value[i])
    }
  }
  
  # Combine adjusted vectors with "data" dataframe as additional columns
  data <- cbind(data, drill, compl)
  
  
  # SQL Queries -------------------------------------------------------------
  
  # Compile dataframe with columns for API #, total drilling cost, date, and depth
  cost <- sqldf("select distinct(API), max(drill), max(compl), max(DEPTH), max(DAYS)
              from data
              group by API")
  
  # Rename columns
  names(cost) <- c("API", "drill", "completion", "depth", "days")
  
  
  # Analysis ----------------------------------------------------------------
  
  # Get total development cost (drilling + completion)
  total <- cost$drill+cost$completion
  
  # Make into data.frame with depth data
  drillCost.data <- data.frame(total, cost$depth)
  
  # Rename
  names(drillCost.data) <- c("cost", "depth")
  
  # Only those wells which have costs > 0
  drillCost.data <- subset(drillCost.data, subset = (cost > 0))
  
  # Fit as log(y) = a + b * depth
  drillCost.fit <- lm(formula = log(cost) ~ depth, data = drillCost.data)
  
  # Get ratio of completion cost to drilling cost
  ratio <- with(cost[which(cost$completion > 0 & cost$drill > 0),], completion/drill)
  
  # Drop outlier
  ratio <- ratio[ratio < 10]
  
  # Get mean and standard deviation
  complCR <- c(mean(ratio), sd(ratio))
  names(complCR) <- c("mean", "sd")
  
  
  # Save results ------------------------------------------------------------
  
  save(file = file.path(path$data, paste("drillCost_", ver, ".rda", sep = "")),
       list= c ("drillCost.data", "drillCost.fit", "complCR"))
}