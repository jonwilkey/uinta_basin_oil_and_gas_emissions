# Function Info -----------------------------------------------------------
# Name:      sim_tdrill.R (Simulated Time Drilled Vector Calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# Drilled - vector of with elements for each time step in simulation and values
# indicating the number of wells drilled in that timestep


# Outputs -----------------------------------------------------------------

# tDrill - vector with one element for every well predicted in Drilled vector
# with integer values indicating the time step that well was drilled in


# Description -------------------------------------------------------------

# This function builds a vector with one element for each well in the Drilled
# vector and element values indicating which time step each well is drilled in.


# Function ----------------------------------------------------------------

sim_tdrill <- function(Drilled) {
  
  # Create a vector with one element for every well in Drilled vector
  tDrill <- rep(0, times = sum(Drilled))
  
  # Initialize loop counters
  a <- 1 # wellID
  b <- 1 # Tstep
  
  # For every well in Drilled vector, get time step drilled
  for (i in 1:length(Drilled)) {
    
    # If wells were drilled in current timestep of Drilled vector
    if (Drilled[i] > 0) {
      
      # Then for each well drilled
      for (j in 1:Drilled[i]) {
        
        # Set that elements value to the counter "b"
        tDrill[a] <- b
        
        # And increment the element of tDrill by 1
        a <- a+1
      }
    }
    
    # Finally, increment the time step by 1
    b <- b+1
  }
  
  # Return the tDrill vector
  return(tDrill)
}
