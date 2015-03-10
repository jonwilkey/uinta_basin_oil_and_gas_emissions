# Function Info -----------------------------------------------------------
# Name:      ctax.R (Corporate income tax calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# OP - Vector inflation-adjusted oil prices on $/bbl basis

# GP - Vector of inflation-adjusted gas prices on $/MCF basis

# osim - Matrix of oil production volumes for each well (rows) in each timestep
# (columns) in units of bbl

# gsim - Matrix of gas production volumes for each well (rows) in each timestep
# (columns) in units of MCF

# NTIfrac - Conversion factor for estimating net taxable income (NTI), assuming 
# that NTI can be estimated as fraction of revenue from oil and gas sales


# Outputs -----------------------------------------------------------------

# result - List with state and federal corporate income tax results matrix in
# basis $


# Description -------------------------------------------------------------

# This function calculates corporate income taxes for each well as a timeseries 
# by [1] calculating the revenue from oil and gas sales, [2] multiplying that 
# revenue by NTIfrac (a conversion factor generated randomly for each well), and
# finally [3] applying the tax rates CIrate.state and CIrate.fed to the
# estimated NTI to find corporate income taxes.


# Function ---------------------------------------------------------------- 
ctax <- function(OP, GP, osim, gsim, NTIfrac, CIrate.state, CIrate.fed) {
  
  # Preallocate space for results
  revenue <- matrix(0, nrow = nrow(osim), ncol = ncol(osim))
  NTI <-     revenue
  state <-   revenue
  federal <- revenue
  
  # For each timestep (i.e. column)
  for (i in 1:ncol(osim)) {
    
    # Calculate revenue from oil and gas
    revenue[,i] <- OP[i]*osim[,i]+GP[i]*gsim[,i]
    
    # Calculate NTI
    NTI[,i] <- NTIfrac*revenue[,i]
    
    # Calculate state corporate income taxes
    state[,i] <-   NTI[,i]*CIrate.state
    federal[,i] <- NTI[,i]*CIrate.fed-state[,i]
  }
  
  # Combine into list
  result <- list(state = state, fed = federal)
  
  # Return result
  return(result)
}