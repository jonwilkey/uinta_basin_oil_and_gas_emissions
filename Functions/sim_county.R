# Function Info -----------------------------------------------------------
# Name:      sim_county.R (Simulated County Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# fieldnum - vector of with elements filled by field numbers for each well

# prob - data.frame with probability of a well being located in Uintah County
# (if not, it's located in Duchesne county)


# Outputs -----------------------------------------------------------------

# county - character vector of randomly selected county location


# Description -------------------------------------------------------------

# This function randomly picks what county a well is located in  based on the 
# probabilities for whether or not a well is located in Uintah county as
# specified in prob


# Function ----------------------------------------------------------------
sim_county <- function(fieldnum, prob) {
  
  # Predefine output vector
  county <- rep(0, times = length(fieldnum))
  
  # For each field in probl
  for (i in 1:nrow(prob)) {
    
    # Get index of wells located in that field
    ind <- which(fieldnum == prob$field[i])
    
    # First, randomly pick if well is located in Uintah county
    pick <- runif(length(ind)) <= prob$Uintah[i]
    
    # Save out as character string indicating selected county
    county[ind] <- ifelse(test = pick,
                            yes  = "UINTAH",
                            no   = "DUCHESNE")
  }
  
  # Return the wellType vector
  return(county)
}