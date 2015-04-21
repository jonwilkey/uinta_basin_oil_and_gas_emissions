# Function Info -----------------------------------------------------------
# Name:      sim_depth.R (Simulated Well Depth Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wellType - vector of with elements filled by wellTypes for each well

# cdf.depth.ow - CDFs for well depth for oil wells

# cdf.depth.gw - CDFs for well depth for gas wells


# Outputs -----------------------------------------------------------------

# depth - vector of randomly selected depths in feet


# Description -------------------------------------------------------------

# This function randomly picks the depth for each well in the wellType vector
# based on the CDFs for the well types as given in cdf.depth.ow or cdf.depth.gw


# Function ----------------------------------------------------------------

sim_depth <- function(wellType, cdf.depth.ow, cdf.depth.gw) {
  
  # Predefine vector for well depth
  depth   <- rep(0, times = length(wellType))
  
  # Generate total measured depth for each well based on well type (note -
  # assuming that dry wells are gas wells for purpose of depth calculation)
  depth[which(wellType == "OW")] <- cdf.depth.ow$x[findInterval(runif(length(which(wellType == "OW"))),
                                                                c(0, cdf.depth.ow$y),
                                                                all.inside = T)]
  depth[which(wellType == "GW" |
                wellType == "D")] <- cdf.depth.gw$x[findInterval(runif(length(which(wellType == "GW" |
                                                                                      wellType == "D"))),
                                                                 c(0, cdf.depth.gw$y),
                                                                 all.inside = T)]
  
  # Return the wellType vector
  return(depth)
}