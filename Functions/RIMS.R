### RIMS II Employment Calculation Function ###

# Inputs ------------------------------------------------------------------

# multiplier - value of RIMS II final demand multiplier (jobs / $1e6 dollars)

# wsim - well information data.table with drilling schedule and cost data

# LOC - lease operating cost matrix

# tsteps - number of time steps in simulation

# RIMS.cpi - CPI basis for RIMS II multiplier

# cpi - CPI basis for simulation


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
RIMS <- function (multiplier, wsim, LOC, tsteps, RIMS.cpi, cpi) {
  
  # Preallocate spending vector
  invest <- rep(0, tsteps)
  
  for (i in 1:length(invest)) {
    
    # Calculate spending (sum of capital and operating costs) 
    ind <-       which(wsim$tDrill == i)
    invest[i] <- with(wsim[ind, ], sum(cap.drill + cap.compl + LEC)) + sum(LOC[, i])
  }
  
  # Inflation adjust to same cost basis as RIMS multiplier
  invest <- invest * (RIMS.cpi / cpi)
  
  # RIMS II job creation calculation (muliplier * spending in millions of
  # dollars)
  jobs.RIMS <- multiplier*invest/1e6
  
  # Return result
  return(jobs.RIMS)
}