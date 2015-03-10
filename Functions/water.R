# Function Info -----------------------------------------------------------
# Name:      water.R (Water balance calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Inputs ------------------------------------------------------------------

# wsim - data.table with well information

# osim/gsim - matrix of production volume timeseries (of oil or gas) for each
# well

# dw.lm - linear regression model for drilling water (mud and cement) usage


# Outputs -----------------------------------------------------------------

# result - list object with complete water balance, including overall results
# and all intermediate computations for each iteration in the overall simulation


# Description -------------------------------------------------------------

# This function calculates each term in the following water balance equations:

# [1] (recycled) = (produced) - (disposal well + evaporated)
# [2] (water in) = (drilling + fracking + flooding) - (recycled)
# [3] (water intensity) = (water in) / (oil produced)

# All of the terms except 'drilling' and those on the LHS of Eqs.[1]-[3] are 
# randomly picked from CDF functions in cdf.water for each well in the 
# welldata() function. 'drilling' is calculated as a function of depth based on 
# the linear regression model 'dw.lm' contained in the 'water.lm' object. The 
# LHS terms are calculated as a time series after calculating the water usage of
# the other terms on the RHS by mutlipying each of the randomly picked ratios by
# either production volumes (oil or gas, depnding on well type) or produced 
# water. One time terms like fracking water usage and drilling water usage are
# assumed to occur at the time that the well we drilled.

# The results of solving Eqs.[1]-[3] are returned as a list object, along with
# the column sums (i.e. time series totals) of all RHS terms.


# Function ----------------------------------------------------------------
water <- function(wsim, osim, gsim, dw.lm) {
  
  # Calculate known/modeled water balance terms -----------------------------
  
  # Preallocate space for results
  pw <-   matrix(0, nrow = nrow(osim), ncol = ncol(osim))
  disp <- pw
  evap <- pw
  inj <-  pw
  dw <-   pw
  fw <-   pw
  
  # For each well (i.e. row) in wsim
  for (i in 1:nrow(wsim)) {
    
    # Calculate produced water as timeseries based on well type. Check - is well
    # oil well?
    if (wsim$wellType[i] == "OW") {
      
      # If yes, multiply produced water ratio by oil production
      pw[i,] <- wsim$pw[i]*osim[i,]
      
      # Alternatively, it could be a gas well
    } else if (wsim$wellType[i] == "GW") {
      
      # In which case multiply instead by gas production
      pw[i,] <- wsim$pw[i]*gsim[i,]
      
    } # Otherwise it's a dry well, and leave produced water row as zeros
    
    # Next, get water disposal through injection wells as timeseries
    disp[i,] <- wsim$disp[i]*pw[i,]
    
    # Evaporation ponds as timeseries
    evap[i,] <- wsim$evap[i]*pw[i,]
    
    # Water flooding as timeseries
    inj[i,] <- wsim$inj[i]*osim[i,]
    
    # For one-time events, place in correct matrix location based on value of
    # wsim$tDrill.
    fw[i, wsim$tDrill[i]] <- wsim$frack[i]
    
    # Also, calculate drilling water usage as function of well depth. Since it's
    # possible for a shallow well to be outside the range of the dw.lm 
    # regression model, assume that any well shallower than the shallowest well
    # in the model fit uses the same amount of water as the fitted value of that
    # shallowest well
    if (wsim$depth[i] >= min(dw.lm$model$depth)) {
      
      # If depth is greater than the shallowest well, just use the fitted model
      dw[i, wsim$tDrill[i]] <- dw.lm$coef[2]*wsim$depth[i]+dw.lm$coef[1]
      
    } else {
      
      # Use that fitted estimate for the shallowest well
      dw[i, wsim$tDrill[i]] <- min(fitted(dw.lm))
    }
  } 
  
  
  # Solve water balance equation for unknown terms --------------------------
  
  # Calculate amount of water recycled as time series
  recycle <- pw-(disp+evap)
  
  # Calculate (water in) term
  wtr.in <- (dw+fw+inj)-recycle
  
  # Finally, calculate water intensity ratio
  wtr.r <- colSums(wtr.in)/colSums(osim)
  
  
  # Return results ---------------------------------------------------------
  
  # Wrap results into single list object
  result <- list(pw =      colSums(pw),
                 disp =    colSums(disp),
                 evap =    colSums(evap),
                 recycle = colSums(recycle),
                 dw =      colSums(dw),
                 fw =      colSums(fw),
                 inj =     colSums(inj),
                 wtr.in =  colSums(wtr.in),
                 wtr.r =   wtr.r)
  
  # Return result
  return(result)
}