### Severance Tax Function ###

# Inputs ------------------------------------------------------------------

# type - character string for use with switch() to calculate either oil or gas ST

# tDrill - vector giving timestep during which each well was drilled

# psim - matrix of production volumes (bbl oil or MCF gas)

# rsim - matrix of royalty payments (in $)

# ep - vector of inflation-adjusted energy prices ($/bbl or $/MCF)

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
stax <- function (type, tDrill, psim, rsim, ep, sto, API) {
  
  # Oil severance tax calculation function
  oilcalc <- function (sto, volume, price, royalty_rate, API) {
    # This function determines severance tax payments.
    # Severance tax rates
    s_low  <- sto[1] # for values <= $13/bbl
    s_high <- sto[2] # for values > $13/bbl
    s_cf   <- sto[3] # Conservation fee
    
    # Determine wellhead value (WV). API gravity of WTI (39.6 degrees) is used as
    # the basis for this calculation. If producing an unconventional oil (from
    # shale or sand), replace the default API gravity argument in the function
    # call with the API gravity of the oil in question
    WV <- (API/sto[4]) * price
    
    # Deductions on WV to determine taxable value (TV). Currently just deducting
    # royalty payments (given here on $/bbl basis)
    TV <- WV - royalty_rate
    
    # Determine fraction of TV above split tax rate (f_st) of cutoff threshold ($13/bbl)
    f_st <- rep(0, length(TV))
    f_st <- ifelse(test = (TV - sto[5])/TV < 0,
                   yes = 0,
                   no = (TV - sto[5])/TV)
    
    # ST per unit volume
    rST <- (s_low * (1 - f_st) + s_high * f_st) * TV + s_cf * TV
    
    # ST total
    ST <- rST * volume
    return(ST)
  }
  
  # Divide royalty payments by production rates to get $/bbl or $/MCF royalty
  # rate
  rrate <- rsim/psim
  
  # Define severance tax (stsim) matrix
  stsim <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
  
  # Calculate oil or gas severance taxes
  switch(type,
         
         # For oil, use oilcalc function
         oil = {
           
           # For each timestep, run oilcalc function
           for (i in 1:ncol(psim)) {
             stsim[,i] <- oilcalc(sto =          sto,
                                  volume =       psim[,i],
                                  price =        ep[i],
                                  royalty_rate = rrate[,i],
                                  API =          API)
           }
         },
         
         # For gas, use gascalc function
         gas = {
           
           # For each timestep, run gascalc function
           for (i in 1:ncol(psim)) {
             stsim[,i] <- oilcalc(sto =          sto,
                                  volume =       psim[,i],
                                  price =        ep[i],
                                  royalty_rate = rrate[,i],
                                  API =          API)
         })
  
  
  # Exempt first sto[6] months of production by writing 0 over ST calculations from proceeding loop
  for (i in 1:(ncol(stsim)-(sto[6]-1))) {
    ind <- which(tDrill == i)
    stsim[ind,i:(i+(sto[6]-1))] <- 0
  }
  
  # Handle special case where < sto[6]-1 time steps remain in matrix row
  for (i in (ncol(stsim)-(sto[6]-2)):ncol(stsim)) {
    ind <- which(tDrill == i)
    stsim[ind,i:ncol(stsim)] <- 0
  }
    
  # Return result
  return(stsim)
}