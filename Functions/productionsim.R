### Production Simulation ###

# Inputs ------------------------------------------------------------------

# nrun - number of iterations in simulation

# wsim - well information data.table with decline curve coefficients

# data_root - location of saved data.frame psim_actual_v1.rda with production
# history of all actual wells in Basin over simulation timeframe

# timesteps - vector of dates comprising number of timesteps in model

# production.type - character switch indicating method for determining prodution
# volumes (simulated or actual)


# Outputs -----------------------------------------------------------------

# psim - matrix with rows = individual wells and columns = timesteps that gives
# production volume timeseries (of either oil or gas) for each well

# Description -------------------------------------------------------------

# This function calculates the production volumes of oil or gas (depending on
# well type) for each well in wsim according to the hyperbolic decline curve
# equation:

# q = a*(1+b*c*t)^(-1/b)

# where q is prodcution (in bbl oil or MCF gas), a is the initial production 
# rate, b is the decline exponent, c is the initial decline rate, and t is time.

# If the value of production.type == "a" then the model calculates the
# production volumes using the values of a, b, and c stored in wsim. The
# calucaltion is conducted sequentially for all timesteps, simultaneously
# calculating the production for all wells drilled in the same timestep. In the
# event that the coefficients result in any production values that are nonreal
# (negative, NaN, or NA), those values are overwritten as zeros.

# Else, the function is a validation run and the function loads and returns the 
# actual production histories stored in the prepared data.frame psim.actual and 
# returns that matrix as psim. If nrun > 1 then matrix is concatonated "nrun"
# times.


# Function ----------------------------------------------------------------
productionsim <- function(nrun, wsim, data_root, timesteps, production.type) {
  
  if (production.type == "a") {
    # If simulation run, generate production data from decline curve
    # coefficients in wsim
    
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
    
    # Concatonate actual production matrix "nrun" times
    temp <- psim.actual
    if (nrun > 1) {
      for (i in 1:(nrun-1)) {
        temp <- rbind(temp, psim.actual)
      }
    }
    
    # Define as psim
    psim <- temp
  }
  
  # Cleanup NA, NaN, and negative values from psim
  for (i in 1:length(timesteps)) {
    ind <- which(is.na(psim[,i]) | psim[,i] == "NaN" | psim[,i] < 0)
    psim[ind,i] <- 0
  }
    
  # Return psim as result
  return(psim)
}