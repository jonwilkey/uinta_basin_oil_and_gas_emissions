# Function Info -----------------------------------------------------------
# Name:      stax.R (Severance tax calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# type - character string for use with switch() to calculate either oil or gas
# ST

# tDrill - vector giving timestep during which each well was drilled

# psim - matrix of production volumes (bbl oil or MCF gas)

# rsim - matrix of royalty payments (in $)

# ep - vector of inflation-adjusted energy prices ($/bbl or $/MCF)

# st.low - Low ST rate for oil/gas for values <= cutoff value threshold

# st.high - High ST rate for oil/gas for values > cutoff value threshold

# st.con - Conservation fee rate

#st.cut - Oil or gas cutoff value threshold ($/bbl or $/MCF) for switch from low
#to high ST rate

# st.skip - Number of timesteps from date well is drilled to exempt from ST

# strip - Production volume below which a well is classified as a stripper well,
# at which point it is exempt from severance taxes


# Outputs -----------------------------------------------------------------

# stsim - matrix of severance tax payments


# Description -------------------------------------------------------------

# This function determines the severance tax payments for each well (rows)
# during each timestep (columns) according to the following set of equations:

# [1] ST = rST * volume
# [2] rst = (st.low * (1 - f_st) + st.high * f_st) * TV + st.con * TV
# [3] TV = price - royalty_rate

# where ST is the severance tax payment, rST is the severance tax rate ($/bbl or
# MCF), st.low is the severance tax rate on values below $13, st.high is the 
# severance tax rate on values above $13, f_st is the fraction of product value 
# that is > $13, TV is the taxable value, st.con is the conservation fee, price
# is the wellhead value of the product, and royalty_rate is the $/bbl or $/MCF
# paid in royalties on each well at each timestep.

# A generic "calc" function is used to implement equations [1]-[3]. royalty_rate
# (rrate) is determined by dividing rsim by psim. Volumes from psim, prices from
# ep, and the calculated royatly rates in rrate are then passed to calc, which
# calculates severance taxes for oil wells and gas wells by timestep.

# To exempt the first "skip" months of production from each well from severance
# taxes the function next scans through each well and overwrites the calculated
# severance taxes in the first six time steps of its existence with zeroes.

# Finally, to exempt stripper wells an ifelse() function call is used to search 
# for any wells that have less than "strip" production volume in psim, and if 
# there are any, then those months have their severance tax payments set to
# zero.


# Function ----------------------------------------------------------------
stax <- function (type, tDrill, psim, rsim, ep, st.low, st.high, st.con, st.cut,
                  st.skip, strip) {
  
  # Severance tax calculation function
  calc <- function (st.low, st.high, st.con, st.cut, volume, price, royalty_rate) {
    
    # Deduct royalty payments from price to determine taxable value (TV)
    TV <- price - royalty_rate
    
    # Determine fraction of TV above split tax rate (f_st) of cutoff threshold
    f_st <- rep(0, length(TV))
    f_st <- ifelse(test = (TV - st.cut)/TV < 0,
                   yes = 0,
                   no = (TV - st.cut)/TV)
    
    # ST per unit volume
    rST <- (st.low * (1 - f_st) + st.high * f_st) * TV + st.con * TV
    
    # ST total
    ST <- rST * volume
    return(ST)
  }
  
  # Divide royalty payments by production rates to get $/bbl or $/MCF royalty 
  # rate. In case production is zero, use ifelse() to scan psim and replace any
  # 0's with 1's (rsim == 0 IFF psim == 0, so 0/1 = 0 and we avoid making NA's)
  rrate <- rsim/ifelse(psim == 0, 1, psim)
  
  # Define severance tax (stsim) matrix
  stsim <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
  
  # Calculate oil or gas severance taxes for each timestep
  for (i in 1:ncol(psim)) {
    stsim[,i] <- calc(st.low =       st.low,
                      st.high =      st.high,
                      st.con =       st.con,
                      st.cut =       st.cut,
                      volume =       psim[,i],
                      price =        ep[i],
                      royalty_rate = rrate[,i])
  }
  
  
  # Exempt first st.skip months of production by writing 0 over ST calculations
  # from proceeding loop
  for (i in 1:(ncol(stsim)-(st.skip-1))) {
    ind <- which(tDrill == i)
    stsim[ind,i:(i+(st.skip-1))] <- 0
  }
  
  # Handle special case where < st.skip-1 time steps remain in matrix row
  for (i in (ncol(stsim)-(st.skip-2)):ncol(stsim)) {
    ind <- which(tDrill == i)
    stsim[ind,i:ncol(stsim)] <- 0
  }
  
  # Exempt stripper wells by checking for production below strip rate
  stsim <- ifelse(test = psim < strip, yes = 0, stsim)
  
  # Return result
  return(stsim)
}