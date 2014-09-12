### Water Balance Function ###

# Inputs ------------------------------------------------------------------

# wsim - data.table with well information; using runID, tDrill, and frack info
# here

# psim - matrix of production volume timeseries (of oil or gas) for each well

# data_root - filepath to pre-processed water data (linear models and CDFs)

# Outputs -----------------------------------------------------------------

# result - list object with complete water balance, including overall results
# and all intermediate computations for each iteration in the overall simulation


# Description -------------------------------------------------------------

# This function calculates each term in the following water balance equations:

# [1] (recycled) = (produced) - (disposal well + evaporated)
# [2] (water in) = (drilling + fracking + flooding) - (recycled)
# [3] (water intensity) = (water in) / (oil produced)

# All terms are handled here with the exception of fracking water usage, which
# is determined on an individual well basis in the welldata.R script.

# === Method ===

# The script first loads a preprocessed .rda file containing the model its and 
# CDF for each term in the balance equation. Next, the total amount of oil 
# produced from oil wells is calculated (NOTE - REPLACE WITH TOTAL OIL 
# PRODUCTION FROM ALL WELLS IF/WHEN OGW IS IMPLEMENTED). The amount of produced 
# water is then calculated as a funtion of total oil production. Similarly the 
# amount of water disposed of via deep water injection wells is calculated as a 
# function of produced water. Water disposed of via evaporation ponds and water 
# injected for flooding are both determined by using runif() to pick a ratio 
# from their respective CDFs. Fracking water is summed together for all wells 
# drilled in the same time step in the same simulation iteration. Drilling water
# is assumed to be a fixed % of fracking water (NOTE - REPLACE WITH ACTUAL DATA 
# FROM DOGM). Finally, equations [1]-[3] are calculated and the results, along
# with all the terms, are exported as a list object.

# Function ----------------------------------------------------------------
water <- function(wsim, psim, data_root) {
  
  # === Load required data files ===
  load(file.path(data_root, "water_models.rda"))
  
  
  # === Get total oil production in each time step for each run ===  
  # Predefine matrix for storing oil summation results with rows = runID and
  # columns = timesteps
  oil <- matrix(0, nrow = max(wsim$runID), ncol = ncol(psim))
  
  # For each runID, get the column sum of oil production from all wells
  # associated with that runID in each timestep
  for (i in 1:nrow(oil)) {
    oil[i,] <- colSums(psim[which(wsim$runID == i &
                                  wsim$wellType == "OW"),])
  }
  
  
  # === Produced water ===  
  # Calculated as f(oil) from linear regression model prod.water.lm
  prod <- with(prod.water.lm, coefficients[2]*oil+coefficients[1])
  
  
  # === Disposal water via deep water injection ===
  # Calculated as f(produced water) from linear regression model disp.water.lm
  disp <- with(disp.water.lm, coefficients[2]*prod+coefficients[1])
  
  
  # === Evaporation water ===
  # Predefine matrix for evaporation ratios (water evaporate:water produced)
  evap <- matrix(0, nrow = nrow(prod), ncol = ncol(prod))
  
  # Generate matrix of evaporation ratios
  for (i in 1:nrow(evap)) {
    evap[i,] <- evap.cdf$ratio[findInterval(runif(ncol(evap)), c(0, evap.cdf$CDF))] # Ratio (water evaporated)/(water produced)
  }
  
  # Multiply by produced water to get total evaporation water
  evap <- evap*prod
  
  
  # === Fracking Water ===
  # Predefine space for summation results
  frack <- matrix(0, nrow = nrow(prod), ncol = ncol(prod))
  
  # For each runID i and timestep j, get sum of fracking water used
  for (i in 1:nrow(frack)) {
    temp <- subset(wsim, subset = (runID == i), select = c("tDrill", "frack"))
    for (j in 1:ncol(frack)) {
      frack[i,j] <- sum(temp$frack[which(temp$tDrill == j)])
    }
  }
  
  
  # === Flooding Water ===
  # Predefine matrix for water flood ratios (water flooded:oil produced)
  flood <- matrix(0, nrow = nrow(prod), ncol = ncol(prod))
  
  # Generate matrix of water flood ratios
  for (i in 1:nrow(flood)) {
    flood[i,] <- flood.cdf$ratio[findInterval(runif(ncol(flood)), c(0, flood.cdf$CDF))] # Ratio (flooding water)/(oil produced)
  }
  
  # Multiply by oil production to get flooding water total
  flood <- flood*oil
  
  
  # === Drilling ===
  # Assuming 10% of total water usage for drilling & fracking right now. To be
  # replaced by actual data from DOGM well files in the future.
  drill <- 0.1
  
  # Calculate from assumed total fraction stated above
  drill <- drill/(1-drill)*frack
  
  
  # === Water Balance ===
  # Water recycled
  recycle <- prod-(disp+evap)
  
  # Water into system
  water.in <- (drill+frack+flood)-recycle
  
  # Water Intensity Ratio
  intensity <- water.in/oil
  
  
  # === Return ===
  # Wrap results into single list object
  result <- list(prod,
                 disp,
                 evap,
                 recycle,
                 drill,
                 frack,
                 flood,
                 water.in,
                 intensity)
  
  # Return result
  return(result)
}