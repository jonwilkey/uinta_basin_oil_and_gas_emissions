# Function Info -----------------------------------------------------------
# Name:      sim_lease.R (Simulated Well Lease Type Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# fieldnum - vector of with elements filled by wellTypes for each well

# cdf.flt - CDFs for surface lease types (federal, state, etc.) by field


# Outputs -----------------------------------------------------------------

# lease - character vector of randomly selected surface lease types


# Description -------------------------------------------------------------

# This function randomly picks the surface lease type for each well in the
# fieldnum vector based on the CDFs for lease types by field as given in cdf.flt


# Function ----------------------------------------------------------------

sim_lease <- function(fieldnum, cdf.flt) {
  
  # Predefine vector for well depth
  lease <- rep(0, times = length(fieldnum))
  
  # Extract CDF values
  temp <- as.matrix(cdf.flt[,2:5])
  
  # Pick surface lease (1 - Federal, 2 - Indian, 3 - State, 4 - Fee). For each
  # field...
  for (i in 1:nrow(cdf.flt)) {
    
    # Get index of wells in that field
    ind <- which(fieldnum == cdf.flt$Field[i])
    
    # For each well in field, randomly pick lease type
    lease[ind] <- findInterval(runif(length(ind)), c(0, temp[i,]))
  }
  
  # Replace lease type #s with strings (switch expression in royalty.R function
  # requires switch to operate on a string, not a numerical value)
  lease[which(lease == 1)] <- "federal"
  lease[which(lease == 2)] <- "indian"
  lease[which(lease == 3)] <- "state"
  lease[which(lease == 4)] <- "fee"
  
  # Return the wellType vector
  return(lease)
}