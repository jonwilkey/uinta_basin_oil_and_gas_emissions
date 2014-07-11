# productionsim.R (Production Simulation)
# Version 1
# 07/09/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -This function generates psim matrix, which gives the amount of oil or gas
#     produced from a well (depending on well type) as a function of time.

# Function call
productionsim <- function(wsim, data_root, timesteps, calltype = "sim") {
  
  # If simulation run, generate production data from decline curve coefficients
  # in wsim
  if (calltype == "sim") {
    
    # Predefine production matrix (rows = individual wells, columns = timesteps)
    psim <- matrix(0, nrow = nrow(wsim), ncol = length(timesteps))
    
    # Calculate production by selecting all wells drilled in a given timestep
    # and calculating production simultaneously for all selected wells
    for (i in 1:length(timesteps)) {
      ind <- which(wsim$tDrill == i)
      for (t in 1:(length(timesteps)+1-i)) {
        psim[ind,(i+t-1)] <- wsim$a[ind]*(1+wsim$b[ind]*wsim$c[ind]*t)^(-1/wsim$b[ind])
      }
    }
  } else {
    # Else load actual DOGM data on production from schedule_v1.R
    load(file.path(data_root, "psim_actual_v1.rda"))
    
    # Define as psim
    psim <- psim.actual
  }
  
  # Cleanup NA, NaN, and negative values from psim
  for (i in 1:length(timesteps)) {
    ind <- which(is.na(psim[,i]) | psim[,i] == "NaN" | psim[,i] < 0)
    psim[ind,i] <- 0
  }
    
  # Return psim as result
  return(psim)
}