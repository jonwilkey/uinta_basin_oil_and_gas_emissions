# Function Info -----------------------------------------------------------
# Name:      sim_wellType.R (Simulated Well Type Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# fieldnum - vector of with elements filled by field numbers for each well

# prob - data.frame with CDF of a well being dry or a gas well by field


# Outputs -----------------------------------------------------------------

# wellType - character vector of randomly selected well types (OW, GW, or D)


# Description -------------------------------------------------------------

# This function randomly picks well types for each well in the fieldnum vector
# based on the CDFs for the well types by field as given in prob


# Function ----------------------------------------------------------------

sim_wellType <- function(fieldnum, prob) {
  
  # Predefine output vector
  wellType <- rep(0, times = length(fieldnum))
  
  # For each field in probl
  for (i in 1:nrow(prob)) {
    
    # Get index of wells located in that field
    ind <- which(fieldnum == prob$field[i])
    
    # First, randomly pick if well is dry
    dry <- runif(length(ind)) <= prob$dry[i]
    
    # If the well is dry
    wellType[ind] <- ifelse(test = dry,
                            
                            # If true, set wellType to "D"
                            yes = "D",
                            
                            # If not, its either a gas well or oil well, so randomly pick if gas well
                            no = ifelse(test = runif(length(which(dry == FALSE))) <= prob$gas[i],
                                        
                                        # If true, its a gas well "GW"
                                        yes = "GW",
                                        
                                        # if false, its an oil well "OW"
                                        no = "OW"))
  }
  
  # Return the wellType vector
  return(wellType)
}