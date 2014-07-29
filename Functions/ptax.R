### Property Tax Calculation ###

# Inputs ------------------------------------------------------------------

# data_root - directory path point to location of saved linear model fitted
# equation for lease operating costs leaseOpCost_v1.rda

# wsim - well information data.table with information on the depth of each well

# psim - matrix of production volume timeseries (either oil or gas) for each
# well

# op - Vector inflation-adjusted oil prices on $/bbl basis

# gp - Vector of inflation-adjusted gas prices on $/MCF basis

# ind.ow - Index (row numbers) of oil wells

# ind.gw - Index (row numbers) of gas wells

# basis - Consumer price index (CPI) value at the desired time

# rsim - matrix of royalty payments for each well (rows) at each time step
# (columns)

# stsim - matrix of severance tax payments for each well (rows) at each time
# step (columns)


# Outputs -----------------------------------------------------------------

# result - function returns matrix of lease operating costs LOC. Once issues
# surrounding negative values of NPV are resolved result will be turned into a
# list object which will return both LOC and property tax matrix ptax.

# Description -------------------------------------------------------------

# This function determines property taxes for each well during each timestep by 
# determing the NPV of each well at each timestep and applying the property tax 
# rates in the Uinta Basin to that NPV. Currently, calculated values of LOCs are
# on the same order of magnitude as gross revenue from oil and gas sales. 
# Consequently, the NPV of almost all wells is negative, and no property taxes 
# are paid (contradicting actual property tax data). Until this issue is
# resolved only the matrix of LOCs is returned by this function.


# Function ----------------------------------------------------------------
ptax <- function (data_root, wsim, psim, op, gp, ind.ow, ind.gw, basis, rsim, stsim) {
  
  # Load leasing operating cost fits
  load(file.path(data_root, "leaseOpCost_v1.rda"))
  
  # Predefine NPV and LOC matrix
  rev <- matrix(0, nrow = nrow(wsim), ncol = ncol(psim))
  LOC <- rev
  
  # Determine revenue (price * production)
  for (i in 1:ncol(rev)) {
    rev[ind.ow,i] <- op[i]*psim[ind.ow,i]
    rev[ind.gw,i] <- gp[i]*psim[ind.gw,i]
  }
  
  # Determine lease operating costs - LOC is in 2009 dollars (CPI = 214.537), so 
  # it must be adjusted for inflation to desired basis and converted to monthly 
  # value. Also, production rate must be in terms of SCFD (so divide initial 
  # production rate coefficient by 30 to covert from months to days). Finally LOC
  # data is in dollars per year, so convert from annual costs to monthly costs by
  # dividing by 12.
  for (i in 1:ncol(LOC)) {
    LOC[ind.ow,i] <- (fit.LOC.oil$coefficients[2]*op[i]+
                        fit.LOC.oil$coefficients[3]*wsim$depth[ind.ow]+
                        fit.LOC.oil$coefficients[1])*(basis/214.537)*(1/12)
    LOC[ind.gw,i] <- (fit.LOC.gas$coefficients[2]*gp[i]+
                        fit.LOC.gas$coefficients[3]*wsim$depth[ind.gw]+
                        fit.LOC.gas$coefficients[4]*wsim[ind.gw,]$a/30+
                        fit.LOC.gas$coefficients[1])*(basis/214.537)*(1/12)
  }
  
  # Take deductions for royalties, severance taxes, and LOCs. Problem - NPV in 
  # many cases is << 0, which would have the obvious result of no property taxes
  # being paid (despite clear records of property taxes actually being paid).
  # Rest of ptax code development on hold until NPV issue resolved.
  NPV <- rev-rsim-stsim-LOC
  
#   # Specify annual discount rate
#   rate <- 0.1164
#   
#   # Calculate vector of discount factors
#   DF <- (1+rate/12)^(-c(0:(length(all_months)-1)))
#   
#   # Create list of results for return call
#   result <- list()
  result <- LOC

  # Return list of results
  return(result)
}