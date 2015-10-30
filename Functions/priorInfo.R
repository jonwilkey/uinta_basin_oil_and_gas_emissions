# Function Info -----------------------------------------------------------
# Name:      priorInfo.R (Data for wells drilled prior to start of simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# apilist - list of API numbers for which information is to be gathered

# p - DOGM database data frame

# field - vector containing field numbers that will be analyzed individually


# Outputs -----------------------------------------------------------------

# result - data frame with information for all prior wells on API #, field
# number, well type, well depth, and lease type


# Description -------------------------------------------------------------

# This function collects the "hard" information necessary for including the 
# prior well population in the calculation of economic and environmental impacts
# in subsequent steps of the simulation. "Hard" information gathered here are
# details about each well that don't change (i.e. well depth, type, etc.).


# Function ----------------------------------------------------------------
priorInfo <- function(apilist, p, field) {
  
  # Get list of all the info needed
  temp <- sqldf("select distinct p_api, w_field_num, h_well_type, h_td_md,
                w_lease_type, w_county
                from p
                group by p_api")
  
  # Merge with apilist
  result <- merge(x = apilist, y = temp, by.x = 1, by.y = "p_api", all.x = T)
  
  # Rename columns
  names(result) <- c("api", "fieldnum", "wellType", "depth", "lease", "county")
  
  # Rewrite field numbers to match simulation field number selection. Start by 
  # building index of rows in result whose field numbers match those in the
  # fields being analyzed individually
  ind <- NULL
  for (i in 1:(length(field)-1)) {
    ind <- c(ind, which(result$fieldnum == field[i]))
  }
  
  # Replace all other field numbers with 999
  result$fieldnum[-ind] <- 999
  
  # Add in tDrill entries for prior wells as zeroes
  result$tDrill <- rep(0, length(apilist))
  
  # Replace lease type #s with strings (switch expression in royalty.R function
  # requires switch to operate on a string, not a numerical value)
  result$lease[which(result$lease == 1)] <- "federal"
  result$lease[which(result$lease == 2)] <- "indian"
  result$lease[which(result$lease == 3)] <- "state"
  result$lease[which(result$lease == 4)] <- "fee"
  
  # Return result data frame
  return(result)
}

