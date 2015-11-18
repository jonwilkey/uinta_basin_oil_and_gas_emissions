# Function Info -----------------------------------------------------------
# Name:      EIAsimLT.R (EIA long-term price path simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# nrun - Number of Monte-Carlo simulation runs

# Eoil/Egas.LT - EIA AEO forecast matrix for oil and gas prices, respectively


# Outputs -----------------------------------------------------------------

# opsim/gpsim - a matrix of oil/gas prices in basis $/bbl or $/MCF with rows =
# random draws (1 row for each MC run) and columns = time steps


# Description -------------------------------------------------------------

# This function randomly picks rows of from the long-term quantiled EIA forecast
# matrices Eoil.LT and Egas.LT. The rows picked are the forecast results.


# Function ----------------------------------------------------------------

EIAsimLT <- function(nrun, Eoil.LT, Egas.LT) {
  
  # Energy price path simulation --------------------------------------------
  
  # Pick random rows for Eoil and Egas and then multiply op and gp by the
  # corresponding error % to get resulting price paths
  
  # Make random row picks
  otemp <- round(runif(nrun, min = 1, max = nrow(Eoil.LT)))
  gtemp <- round(runif(nrun, min = 1, max = nrow(Egas.LT)))
  
  # Preallocate space for results
  opsim <- matrix(0, nrow = nrun, ncol = ncol(Eoil.LT))
  gpsim <- opsim
  
  for (j in 1:nrun) {
    
    opsim[j, ] <- Eoil.LT[otemp[j], ]
    gpsim[j, ] <- Egas.LT[gtemp[j], ]
  }
  
  
  # Return results ----------------------------------------------------------
  
  return(list(op = opsim, gp = gpsim))
}