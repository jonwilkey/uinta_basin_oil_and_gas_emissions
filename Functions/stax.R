# severance_tax.R (Severance Tax Calculation)
# Version 1
# 07/10/14
# Jon Wilkey

# Severance Tax Function
stax <- function (wsim, psim, rsim, op, gp, ind.ow, ind.gw) {
  
  # Actual ST calc function
  calc <- function (volume, price, royalty_rate, API = 39.6) {
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
    stsim[ind.gw,i] <- calc(psim[ind.gw,i], gp[i], rrate[ind.gw,i])
    stsim[ind.ow,i] <- calc(psim[ind.ow,i], op[i], rrate[ind.ow,i])
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