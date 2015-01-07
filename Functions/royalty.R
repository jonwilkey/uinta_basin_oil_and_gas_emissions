### Royalty Calculation ###

# Inputs ------------------------------------------------------------------

# op - vector of inflation-adjusted oil prices

# gp - vector of inflation-adjusted gas prices

# wsim - data.table with information about each well

# psim - matrix with production records for each simulated well


# Outputs -----------------------------------------------------------------

# rsim - matrix of royalty payments for each well (rows) in each timestep
# (columns)


# Description -------------------------------------------------------------

# This function determines royalty payments according to the following equation:

# (royalty payment) = (rate as function of landownership) * (volume of oil or
# gas produced) * (price of oil or gas)

# This is accomplished using an internal "calc" function that runs on each 
# possible combination of well type and surface landownership. An index of all
# the wells associated with each combination is generated and the resulting
# royalty payments are calculated in each timestep for each of the combinations.
# The final resulting matrix of royalty payments is returned as rsim.


# Function ----------------------------------------------------------------
royalty <- function (op, gp, wsim, psim) {
  
  # Actual royalty calculation function
  calc <- function (volume, price, landowner) {
    # This function determines the royalty payments.
    
    # Pick rate based on landownership
    rate <- switch(landowner,
                   federal = 0.1250,
                   indian  = 0.1667,
                   state   = 0.1250,
                   fee     = 0.1250)
    
    calc <- rate * volume * price
    return(calc)
  }
  
  # Predefine space for royalties matrix "rsim"
  rsim <- matrix(0, nrow = nrow(wsim), ncol = ncol(psim))
  
  # Get indices for each combination of well type and landowner
  ind.gw.fed <- which(wsim$wellType == "GW" & wsim$landOwner == "federal")
  ind.gw.ind <- which(wsim$wellType == "GW" & wsim$landOwner == "indian")
  ind.gw.sta <- which(wsim$wellType == "GW" & wsim$landOwner == "state")
  ind.gw.fee <- which(wsim$wellType == "GW" & wsim$landOwner == "fee")
  
  ind.ow.fed <- which(wsim$wellType == "OW" & wsim$landOwner == "federal")
  ind.ow.ind <- which(wsim$wellType == "OW" & wsim$landOwner == "indian")
  ind.ow.sta <- which(wsim$wellType == "OW" & wsim$landOwner == "state")
  ind.ow.fee <- which(wsim$wellType == "OW" & wsim$landOwner == "fee")
  
  # Apply royalty calc function to each combination
  for (i in 1:ncol(psim)) {
    rsim[ind.gw.fed,i] <- calc(psim[ind.gw.fed,i], gp[i], "federal")
    rsim[ind.gw.ind,i] <- calc(psim[ind.gw.ind,i], gp[i], "indian")
    rsim[ind.gw.sta,i] <- calc(psim[ind.gw.sta,i], gp[i], "state")
    rsim[ind.gw.fee,i] <- calc(psim[ind.gw.fee,i], gp[i], "fee")
    
    rsim[ind.ow.fed,i] <- calc(psim[ind.ow.fed,i], op[i], "federal")
    rsim[ind.ow.ind,i] <- calc(psim[ind.ow.ind,i], op[i], "indian")
    rsim[ind.ow.sta,i] <- calc(psim[ind.ow.sta,i], op[i], "state")
    rsim[ind.ow.fee,i] <- calc(psim[ind.ow.fee,i], op[i], "fee")
  }
  
  # Return matrix with results
  return(rsim)
}