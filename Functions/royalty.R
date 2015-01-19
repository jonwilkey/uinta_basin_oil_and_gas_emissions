# Function Info -----------------------------------------------------------
# Name:      royalty.R (Royalty calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# ep - vector of inflation-adjusted energy prices in $/bbl (for oil) or $/MMBtu
# (for gas)

# lease - vector of surface lease types

# psim - matrix with production records for each simulated well in bbl (for oil)
# or MMBtu (for gas)


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
royalty <- function (royaltyRate, ep, lease, psim) {
  
  # Actual royalty calculation function
  calc <- function (volume, price, landowner) {
    # This function determines the royalty payments.
    
    # Pick rate based on landownership
    rate <- switch(landowner,
                   federal = royaltyRate["federal"],
                   indian  = royaltyRate["indian"],
                   state   = royaltyRate["state"],
                   fee     = royaltyRate["fee"])
    
    calc <- rate * volume * price
    return(calc)
  }
  
  # Predefine space for royalties matrix "rsim"
  rsim <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
  
  # Get indices for lease type
  ind.fed <- which(lease == "federal")
  ind.ind <- which(lease == "indian")
  ind.sta <- which(lease == "state")
  ind.fee <- which(lease == "fee")
  
  # Apply royalty calc function to each combination
  for (i in 1:ncol(psim)) {
    rsim[ind.fed,i] <- calc(psim[ind.fed,i], ep[i], "federal")
    rsim[ind.ind,i] <- calc(psim[ind.ind,i], ep[i], "indian")
    rsim[ind.sta,i] <- calc(psim[ind.sta,i], ep[i], "state")
    rsim[ind.fee,i] <- calc(psim[ind.fee,i], ep[i], "fee")
  }
  
  # Return matrix with results
  return(rsim)
}