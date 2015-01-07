# Function Info -----------------------------------------------------------
# Name:      ctax.R (Corporate income tax calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wsim - Well information data.table with corporate income tax rates

# psim - monthly production records

# ind.ow - row index for wells which are oil wells in wsim

# ind.gw - row index for wells which are gas wells in wsim


# Outputs -----------------------------------------------------------------

# ci** - matrix of corporate income taxes paid to state/federal government on 
# oil/gas  production. Abbreviations are ciSO (state oil), ciSG (state gas),
# ciFO(federal oil), and ciFG(federal gas).


# Description -------------------------------------------------------------

# This function calculates corporate income taxes by mutliplying production
# records in psim by the corporate income tax rates for each well given by wsim.


# Function ---------------------------------------------------------------- 
ctax <- function(wsim, psim, ind.ow, ind.gw) {
  
  # Predefine matrices for calculation results
  ciSO <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
  ciSG <- ciSO
  ciFO <- ciSO
  ciFG <- ciSO
  
  # Calculate corporate income taxes for on-type (OOW & GGW) production
  for (i in 1:ncol(psim)) {
    ciSO[ind.ow,i] <- wsim$cirSO[ind.ow]*psim[ind.ow,i]
    ciSG[ind.gw,i] <- wsim$cirSG[ind.gw]*psim[ind.gw,i]
    ciFO[ind.ow,i] <- wsim$cirFO[ind.ow]*psim[ind.ow,i]
    ciFG[ind.gw,i] <- wsim$cirFG[ind.gw]*psim[ind.gw,i]
  }
  
  # Create list
  ci <- list(ciSO, ciSG, ciFO, ciFG)
  
  # Return list
  return(ci)
}