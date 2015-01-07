### Severance Tax Function ###

# Inputs ------------------------------------------------------------------

# wsim - well information data.table with information on drilling schedule

# psim - matrix of production volume timeseries (either oil or gas)

# rsim - matrix of royalty payments

# op - vector of inflation-adjusted oil prices

# gp - vector of inflation-adjusted gas prices

# ind.ow - index (row numbers) of oil wells

# ind.gw - index (row numbers) of gas wells

# API - API gravity of oil product (for discounting product at wellhead)


# Outputs -----------------------------------------------------------------

# stsim - matrix of severance tax payments


# Description -------------------------------------------------------------

# This function determines the severance tax payments for each well (rows)
# during each timestep (columns) according to the following set of equations:

# [1] ST = rST * volume
# [2] rst = (s_low * (1 - f_st) + s_high * f_st) * TV + s_cf * TV
# [3] TV = WV - royalty_rate

# where ST is the severance tax payment, rST is the severance tax rate ($/bbl or
# MCF), s_low is the severance tax rate on values below $13, s_high is the 
# severance tax rate on values above $13, f_st is the fraction of product value
# that is > $13, TV is the taxable value, s_cf is the conservation fee, WV is
# the wellhead value of the product, and royalty_rate is the $/bbl or $/MCF paid
# in royalties on each well at each timestep.

# A generic "calc" function is used to implement equations [1]-[3]. royalty_rate
# (rrate) is determined by dividing rsim by psim. Volumes from psim, prices from
# gp and op, and the calculated royatly rates in rrate are then passed to calc, 
# which calculates severance taxes for oil wells and gas wells by timestep. To
# exempt the first six months of production from each well from severance taxes
# the function next scans through each well and overwrites the calculated
# severance taxes in the first six time steps of its existence with zeroes.


# Function ----------------------------------------------------------------
stax <- function (wsim, psim, rsim, op, gp, ind.ow, ind.gw, API) {
  
  # Actual ST calc function
  calc <- function (volume, price, royalty_rate, API) {
    # This function determines severance tax payments.
    # Severance tax rates
    s_low  <- 0.030 # for values <= $13/bbl
    s_high <- 0.050 # for values > $13/bbl
    s_cf   <- 0.002 # Conservation fee
    
    # Determine wellhead value (WV). API gravity of WTI (39.6 degrees) is used as
    # the basis for this calculation. If producing an unconventional oil (from
    # shale or sand), replace the default API gravity argument in the function
    # call with the API gravity of the oil in question
    WV <- (API / 39.6) * price
    
    # Deductions on WV to determine taxable value (TV). Currently just deducting
    # royalty payments (given here on $/bbl basis from "R" function)
    TV <- WV - royalty_rate
    
    # Determine fraction of TV above split tax rate (f_st) of $13/bbl
    f_st <- rep(0, length(TV))
    f_st <- ifelse(test = (TV - 13)/TV < 0,
                   yes = 0,
                   no = (TV - 13)/TV)
    
    # ST per unit volume
    rST <- (s_low * (1 - f_st) + s_high * f_st) * TV + s_cf * TV
    
    # ST total
    ST <- rST * volume
    return(ST)
  }
  
  # This is slow, fix NaN problem so we can get past ifelse()-ing to handle
  # division by 0
  rrate <- ifelse(test = rsim/psim != "NaN",
                  yes = rsim/psim,
                  no = 0)
  
  # Define severance tax (stsim) matrix
  stsim <- matrix(0, nrow = nrow(wsim), ncol = ncol(psim))
  
  # Calculate severance taxes for oil/gas wells in each month of simulation
  # using calc function
  for (i in 1:ncol(psim)) {
    stsim[ind.gw,i] <- calc(psim[ind.gw,i], gp[i], rrate[ind.gw,i], API)
    stsim[ind.ow,i] <- calc(psim[ind.ow,i], op[i], rrate[ind.ow,i], API)
  }
  
  # Exempt first six months of production by writing 0 over ST calculations from proceeding loop
  for (i in 1:(ncol(psim)-5)) {
    ind <- which(wsim$tDrill == i)
    stsim[ind,i:(i+5)] <- 0
  }
  
  # Handle special case where < 5 time steps remain in matrix row
  for (i in (ncol(psim)-4):ncol(psim)) {
    ind <- which(wsim$tDrill == i)
    stsim[ind,i:ncol(stsim)] <- 0
  }
    
  # Return result
  return(stsim)
}