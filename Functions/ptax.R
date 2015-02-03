# Function Info -----------------------------------------------------------
# Name:      ptax.R (Property Tax Calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# OP - Vector inflation-adjusted oil prices on $/bbl basis

# GP - Vector of inflation-adjusted gas prices on $/MCF basis

# osim - Matrix of oil production volumes for each well (rows) in each timestep
# (columns) in units of bbl

# gsim - Matrix of gas production volumes for each well (rows) in each timestep
# (columns) in units of MCF

# pTaxfrac - Conversion factor for property tax fraction, assuming that property
# taxes can be estimated as fraction of revenue from oil and gas sales


# Outputs -----------------------------------------------------------------

# result - Property tax results matrix in basis $


# Description -------------------------------------------------------------

# This function determines property taxes for each well as a timeseries by [1]
# calculating the revenue from oil and gas sales and then [2] multiplying that
# revenue by pTaxfrac (a conversion factor generated randomly for each well).


# Function ----------------------------------------------------------------
ptax <- function (OP, GP, osim, gsim, pTaxfrac) {
  
  # Preallocate space for results
  revenue <- matrix(0, nrow = nrow(osim), ncol = ncol(osim))
  result <-  revenue
  
  # For each timestep (i.e. column)
  for (i in 1:ncol(osim)) {
    
    # Calculate revenue from oil and gas
    revenue[,i] <- OP[i]*osim[,i]+GP[i]*gsim[,i]
    
    # Calculate property taxes
    result[,i] <- pTaxfrac*revenue[,i]
  }
  
  # Return result
  return(result)
}