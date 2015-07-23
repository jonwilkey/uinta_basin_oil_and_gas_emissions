# Function Info -----------------------------------------------------------
# Name:      priorProdReworkAdjust.R (Production Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wpri - data.frame with information about each well prior well with fits

# timesteps - Number of months to be simulated

# ppri - list with data.frames for monthly production volumes of oil and gas
# from prior wells with fits


# Outputs -----------------------------------------------------------------

# ppri - Modified to remove production volumes from wells with reworks after the
# time step of the rework


# Description -------------------------------------------------------------

# This function overwrites as zero the production of oil and gas from reworked
# wells from the time step of the rework to the end of the simulation period.


# Function ---------------------------------------------------------------- 
priorProdReworkAdjust <- function(wpri, timesteps, ppri) {
  
  # To account for reworks, rewrite any wells with nonzero rework values with
  # zeroes starting from date of rework
  for (i in which(wpri$rework > 0)) {
    
    # Zero out production after rework
    ppri$oil[i,wpri$rework[i]:timesteps] <- 0
    ppri$gas[i,wpri$rework[i]:timesteps] <- 0
  }
  
  # Return results
  return(ppri)
}
