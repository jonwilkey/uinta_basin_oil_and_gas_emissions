### RIMS II Employment Calculation Function ###

# Inputs ------------------------------------------------------------------

# multiplier - value of RIMS II final demand multiplier (jobs / $1e6 dollars)

# wsim - well information data.table with drilling schedule and cost data

# LOC - lease operating cost matrix

# nrun - number of iterations in overall simulation


# Outputs -----------------------------------------------------------------

# jobs.RIMS - Number of jobs created as result of spending by oil and gas
# industry


# Description -------------------------------------------------------------

# This function determines the total spending from the oil and gas industry as:

# invest = (sum of capital costs for drilling) + (sum of operating costs)

# for each year of the simulation and returns the number of jobs created by that
# spending using the RIMS II multiplier supplied in the function input. The
# exact meaning of the # of jobs created in each year is somewhat complicated,
# see documention in RIMS II user guide for more infomration.


# Function ----------------------------------------------------------------
RIMS <- function (multiplier, wsim, LOC, nrun) {
    
  # Preallocate investment schedule space
  invest <- matrix(0, nrow = nrun, floor(ncol(LOC)/12))
  
  # Determine spending schedule on annual basis
  for (i in 1:nrun) {
    
    # Get summation of capital investments for drilling and completing wells
    for (j in 1:ncol(invest)) {
      tstart <- 12*(j-1)+1
      tstop <- 12*(j-1)+12
      ind <- which(wsim$runID == i &
                   wsim$tDrill >= tstart &
                   wsim$tDrill <= tstop)
      invest[i,j] <- sum(wsim$cost[ind])
    }
    
    # Reset selection index to be all wells for runID == i
    ind <- which(wsim$runID == i)
    
    # Add summation of LOCs to previous sum of capital investments
    for (j in 1:ncol(invest)) {
      tstart <- 12*(j-1)+1
      tstop <- 12*(j-1)+12
      invest[i,j] <- invest[i,j]+sum(rowSums(LOC[ind,tstart:tstop], na.rm = TRUE), na.rm = TRUE)
    }
  }
  
  # RIMS II job creation calculation (muliplier * spending in millions of
  # dollars)
  jobs.RIMS <- multiplier*invest/1e6
  
  # Return result
  return(jobs.RIMS)
}