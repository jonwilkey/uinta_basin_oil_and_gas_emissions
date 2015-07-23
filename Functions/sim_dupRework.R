# Function Info -----------------------------------------------------------
# Name:      sim_dupRework.R (Duplicate Simulated Well Reworks)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wsim - simulated well information data frame which has columns for rework and
# tDrill time steps

# wpri - information data frame on prior wells with fits


# Outputs -----------------------------------------------------------------

# wsim - with duplicated reworked wells


# Description -------------------------------------------------------------

# This function duplicates reworked wells. Any reworks in the population of 
# prior wells (either with or without fits) are counted as new wells after this 
# operation, and it is further assumed that they have no time delays in their
# production.


# Function ----------------------------------------------------------------

sim_dupRework <- function(wsim, wpri) {
  
  # If there are reworks for new wells, duplicate reworked wells by...
  if (length(which(wsim$rework > 0)) > 0) {
    
    # Pull out rows to be duplicated
    temp <- wsim[which(wsim$rework > 0),]
    
    # Replace tDrill in duplicated rows by value in rework column
    temp$tDrill <- temp$rework
    
    # Replace rework values in temp data frame with NAs
    temp$rework <- NA
    
    # Replace value of tend for reworked prior wells with zeros
    temp$tend <- 0
    
    # Bind rework wells with to end of wsim
    wsim <- rbind(wsim, temp)
  }
  
  # If there are reworks for prior wells with fits
  if (length(which(wpri$rework > 0)) > 0) {
    
    # Pull out rows to be duplicated
    temp <- wpri[wpri$rework > 0,]
    
    # Restructure to match wsim format
    temp <- data.frame(tDrill =   temp$rework,
                       prior =    TRUE,
                       tend =     0,
                       fieldnum = temp$fieldnum,
                       wellType = temp$wellType,
                       td.oil =   rep(0, nrow(temp)),
                       td.gas =   rep(0, nrow(temp)),
                       depth =    temp$depth,
                       lease =    temp$lease,
                       rework =   rep(NA, nrow(temp)))
    
    # Bind rework wells with to end of wsim
    wsim <- rbind(wsim, temp)
  }
  
  # Return wsim
  return(wsim)
}