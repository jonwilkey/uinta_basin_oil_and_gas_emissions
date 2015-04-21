# Function Info -----------------------------------------------------------
# Name:      sim_dupRework.R (Duplicate Simulated Well Reworks)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wsim - simulated well information data frame which has columns for rework and
# tDrill time steps


# Outputs -----------------------------------------------------------------

# wsim - with duplicated reworked wells


# Description -------------------------------------------------------------

# This function duplicates reworked wells


# Function ----------------------------------------------------------------

sim_dupRework <- function(wsim) {
  
  # If there are reworks for new wells, duplicate reworked wells by...
  if (length(which(wsim$rework > 0)) > 0) {
    
    # Pull out rows to be duplicated
    temp <- wsim[which(wsim$rework > 0),]
    
    # Replace tDrill in duplicated rows by value in rework column
    temp$tDrill <- temp$rework
    
    # Replace rework values in temp data frame with NAs
    temp$rework <- NA
    
    # Bind rework wells with to end of wsim
    wsim <- rbind(wsim, temp)
  }
  
  # Return the rework vector
  return(wsim)
}